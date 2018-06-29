# Copyright 2018 Robert Carnell

rm(list=ls())

stopifnot(require(assertthat))
assertthat::assert_that(require(rstan))
assertthat::assert_that(require(compiler))

currSys <- tolower(Sys.info()["sysname"])

if (currSys == "linux")
{
  repository_path <- file.path("~","repositories","StarWarsRankingMachine")
} else
{
  repository_path <- file.path("C:","Users","Rob","Documents","Repositories",
                               "StarWarsRankingMachine")
}
tmp <- assertthat::assert_that(dir.exists(repository_path))
load(file=file.path(repository_path, "RawData.Rdata"))

extractMatrix <- function(x, position, na_val)
{
  # NA's induced by corercion
  temp <- suppressWarnings(sapply(x, function(x) as.numeric(x$data[[position]]), USE.NAMES=FALSE))
  temp[which(is.na(temp), arr.ind = TRUE)] <- na_val
  return(temp)
}

###############################################################################
# Global Parameters

set.seed(10904)
# the NA value to be used in the stan program.  stan does not have an NA
stan_na <- -9999.0

###############################################################################

stan_model_code <- "
functions {
  real likfunc(vector theta1, vector x, real stan_na)
  {
    int M = num_elements(x);
    real s = M * (M - 1.0) / 2.0;
    
    for (i in 1:(M - 1))
    {
      for (j in (i+1):M)
      {
        if (fabs(x[i] - stan_na) < 1E-9 || 
        fabs(x[j] - stan_na) < 1E-9)
        {
          s += 0.0;
        }
        else if ((x[i] > x[j] && theta1[i] > theta1[j]) ||
                 (x[i] < x[j] && theta1[i] < theta1[j]))
        {
          s += 1.0;
        } 
        else
        {
          s -= 1.0;
        }
      }
    }
    return s;
  }

  real likfuncmat_lpdf(matrix dat, vector theta1, real stan_na)
  {
    real s = 0;
    int M = num_elements(theta1);
    int N = cols(dat);
    vector[M] person_options;
    
    for (n in 1:N)
    {
      person_options = dat[:,n];
      s += likfunc(theta1, person_options, stan_na);
    }
    return s;
  }
}
data {
  int<lower=0> Npeople;
  int<lower=0> Noptions;
  matrix[Noptions, Npeople] y;
  vector<lower=0>[Noptions] alpha;
  real stan_na;
  matrix[Noptions, Noptions] sigma;
}

parameters {
  vector<lower=0, upper=1>[Noptions] theta;
} 

model {
  theta ~ multi_normal_cholesky(alpha, sigma);
  y ~ likfuncmat(theta, stan_na);
} 
"

fits <- vector("list", 10)
for (i in 3:10)
{
  qi <- extractMatrix(questionResponseList, i, stan_na)
  # starting mean of all rankings (prior)
  alpha <- rep(0.5, nrow(qi))
  # variance of all rankings with negative covariance (prior)
  sigma <- matrix(-0.1, nrow(qi), nrow(qi))
  diag(sigma) <- 5
  dati <- list(Npeople = ncol(qi), 
               Noptions = nrow(qi), 
               y = qi, 
               alpha = alpha,
               stan_na = stan_na,
               sigma = sigma)
  fits[[i]] <- stan(model_code = stan_model_code, model_name="model1",  
               data = dati, iter = 4000, chains = 3, warmup=1500, thin=1,
               cores = 3, verbose = TRUE)
}

for (i in 3:10)
{
  print(fits[[i]])
}

traceplot(fits[[7]]) + ylim(0, 1)

###############################################################################

# generate a new estimated theta by switching one pair of rankings
generateNewTheta <- function(theta, dat)
{
  # dat is a dummy entry here because optim will pass it in
  ind <- sample(1:length(theta), size = 2, replace = FALSE)
  temp <- theta[ind[1]]
  theta[ind[1]] <- theta[ind[2]]
  theta[ind[2]] <- temp
  return(theta)
}
cGenerateNewTheta <- cmpfun(generateNewTheta)

likfunc <- function(theta, x)
{
  stopifnot(length(x) == length(theta))
  s <- 0
  for (i in 1:(length(x) - 1))
  {
    for (j in (i + 1):length(x))
    {
      if (is.na(x[i]) || is.na(x[j]))
        next
      if (x[i] < x[j] && theta[i] < theta[j] ||
          x[i] > x[j] && theta[i] > theta[j])
      {
        s <- s + 1
      } else
      {
        s <- s - 1
      }
    }
  }
  return(s)
}
cLikFunc <- cmpfun(likfunc)

likfuncmat <- function(theta, dat)
{
  s <- 0
  for (m in 1:ncol(dat))
  {
    s <- s + cLikFunc(theta, dat[,m])
  }
  return(s)
}
cLikFuncMat <- cmpfun(likfuncmat)

sannFits <- vector("list", 10)
for (i in 3:10)
{
  qi <- extractMatrix(questionResponseList, i, NA)

  o1 <- optim(par = 1:nrow(qi), fn = cLikFuncMat, gr = cGenerateNewTheta, 
              dat = qi, method = "SANN", control = list(fnscale = -1))
  o2 <- optim(par = nrow(qi):1, fn = cLikFuncMat, gr = cGenerateNewTheta, 
              dat = qi, method = "SANN", control = list(fnscale = -1))
  o3 <- optim(par = sample(1:nrow(qi), size=nrow(qi), replace=FALSE), 
              fn = cLikFuncMat, gr = cGenerateNewTheta, 
              dat = qi, method = "SANN", control = list(fnscale = -1))
  o4 <- optim(par = sample(1:nrow(qi), size=nrow(qi), replace=FALSE), 
              fn = cLikFuncMat, gr = cGenerateNewTheta, 
              dat = qi, method = "SANN", control = list(fnscale = -1))
  
  #sannFits[[i]] <- optim(par = 1:nrow(qi), fn = cLikFuncMat, gr = cGenerateNewTheta, 
  #           dat = qi, method = "SANN", control = list(fnscale = -1))
  sannFits[[i]] <- list(par=rank(apply(rbind(o1$par, o2$par, o3$par, o4$par), 2, mean)))
  print(paste0("Sann Fit ", i, " complete"))
}

###############################################################################

save(sannFits, fits, file=file.path(repository_path, "RawFits.Rdata"))
