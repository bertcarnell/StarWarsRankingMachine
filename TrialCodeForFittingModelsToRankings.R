# Example code to Estimate the ranking of a set of options given
#   a dataset of N people raking M options with missing rankings
#   Ex:  Person 1:  1, 2, 3, 4
#        Person 2:  NA, 2, 1, NA

require(rstan)
require(devtools)
require(assertthat)

################################################################################

rdirichlet <- function(n, alpha, allowZero=FALSE) {
  ## pick n random deviates from the Dirichlet function with shape
  ## parameters alpha.  alpha = 0 is allowed and produces 0 for those options
  
  lena <- length(alpha)
  stopifnot( lena > 1 && n > 0 )
  if (!allowZero)
  {
    stopifnot(all(alpha > 0))
    x <- matrix(rgamma(lena*n, alpha), ncol = lena, byrow = TRUE)
    sm <- x %*% rep(1, lena)
    return(x / as.vector(sm))
  } else
  {
    stopifnot(all(alpha >= 0))
    ind <- which(alpha != 0)
    X <- sapply(alpha[ind], function(x) rgamma(n, x, 1))
    Xsum <- apply(X, 1, sum)
    Y <- apply(X, 2, "/", Xsum)
    Z <- matrix(0, nrow = n, ncol = length(alpha))
    Z[,ind] <- Y
    return(Z)
  }
}

# rankins are all 1 based, eg. 1,2,3,4 or 1,2,3
expandRanking <- function(ranks, maxrank)
{
  m <- max(ranks, na.rm = TRUE) - 1
  assertthat::assert_that(maxrank >= m + 1, msg = "maxrank must be greater than the maximum of ranks")
  if (maxrank == m + 1) return(ranks)  
  return((ranks - 1)*(maxrank - 1)/m + 1)
}
expandRanking(1:4, 5)
expandRanking(c(1,NA,2,NA,3,NA), 9)
tryCatch(expandRanking(1:4, 3), error = function(e) "success")
expandRanking(1:4, 4)

################################################################################
# Global Parameters

set.seed(10904)
# people taking the survey 
N <- 11
# options per question
M <- 10
# desired ranking for testing
desiredRanking <- M:1
# the NA value to be used in the stan program.  stan does not have an NA
stan_na <- -9999.0

###############################################################################
# create Example Data

X <- rdirichlet(N, desiredRanking)
X <- apply(X, 1, function(x) {
  # randomly insert NA some times
  if (rbinom(1, size = 1, prob = 0.5) == 1)
  {
    ind <- sample(1:length(x), size = 1)
    x[ind] <- NA
  }
  return(rank(x, na.last = "keep"))
})

# check that the mean rankings were as intended
cbind(rank(apply(X, 1, mean, na.rm = TRUE)), desiredRanking)

# what if we changed the numeric value of those with NAs
# 1 2 3 4
# 1 NA NA 2
# mean= 1 2 3 3 => bad ranking

# 1 2 3 4
# 1 NA NA 4
# mean= 1 2 3 4 => good ranking

Xalt <- apply(X, 2, function(x) expandRanking(x, M))
cbind(rank(apply(Xalt, 1, mean, na.rm = TRUE)), desiredRanking)

X2 <- rdirichlet(N, desiredRanking)
X2 <- apply(X2, 1, function(x) {
  # randomly insert NA some times
  if (rbinom(1, size = 1, prob = 0.5) == 1)
  {
    ind <- sample(1:length(x), size = 1)
    x[ind] <- NA
  }
  # insert a few more
  if (rbinom(1, size = 1, prob = 0.2) == 1)
  {
    ind <- sample(1:length(x), size = 1)
    x[ind] <- NA
  }
  return(rank(x, na.last = "keep"))
})

cbind(rank(apply(X2, 1, mean, na.rm = TRUE)), desiredRanking)

X2alt <- apply(X2, 2, function(x) expandRanking(x, M))
apply(X2alt, 1, mean, na.rm = TRUE)
cbind(rank(apply(X2alt, 1, mean, na.rm = TRUE)), desiredRanking)

###############################################################################
# create a likelihood function for a vector x of rankings given a 
#   true set of rankings

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
      if ((x[i] < x[j] && theta[i] < theta[j]) ||
          (x[i] > x[j] && theta[i] > theta[j]))
      {
        s <- s + 1 / choose(n = length(x), k = 2)
      } 
    }
  }
  return(s)
}

likfunc(sample(1:10, size = 10, replace = FALSE), sample(1:10, size = 10, replace = FALSE))
likfunc(1:10, 1:10)
likfunc(c(1,2,3,5,4,6,7,8,9,10), 1:10)
likfunc(c(2,1,3,5,4,6,7,8,9,10), 1:10)

# create a total likelihood for N people
likfuncmat <- function(theta, dat)
{
  s <- 0
  for (m in 1:ncol(dat))
  {
    s <- s + likfunc(theta, dat[,m])
  }
  return(s)
}

# optimize
o <- optim(par = c(1,2,3,6,5,4,9,8,7,10), likfuncmat, dat = X, control = list(fnscale = -1))
rank(o$par)

# this optimization framework does not work over multiple types of optimization algorithms

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

# the simulated annealing strategy also does not work
o <- optim(par = c(1,2,3,6,5,4,9,8,7,10), fn = likfuncmat, gr = generateNewTheta, 
           dat = X, method = "SANN", control = list(fnscale = -1))
o$par

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
likfuncmat <- function(theta, dat)
{
  s <- 0
  for (m in 1:ncol(dat))
  {
    s <- s + likfunc(theta, dat[,m])
  }
  return(s)
}

# this method works
o <- optim(par = c(1,2,3,6,5,4,9,8,7,10), fn = likfuncmat, gr = generateNewTheta, 
           dat = X, method = "SANN", control = list(fnscale = -1))
o$par

###############################################################################

# switch to a more differentiable likelihood function and one that
#   minimized squared error
likfunc <- function(theta, x)
{
  maxx <- max(x, na.rm = TRUE)
  minx <- min(x, na.rm = TRUE)
  maxt <- max(theta, na.rm = TRUE)
  mint <- min(theta, na.rm = TRUE)
  stopifnot(length(x) == length(theta))
  s <- 0
  for (i in 1:(length(x) - 1))
  {
    for (j in (i + 1):length(x))
    {
      if (is.na(x[i]) || is.na(x[j]))
        next
      s <- s + ((x[i] - x[j])/(maxx - minx) - (theta[i] - theta[j])/(maxt - mint))^2
    }
  }
  return(s)
}

likfunc(1:10, 1:10)
likfunc(c(1,2,3,5,4,6,7,8,9,10), 1:10)
likfunc(c(2,1,3,5,4,6,7,8,9,10), 1:10)
likfunc(10:1, 1:10)
likfunc(c(1,2,3,6,5,4,9,8,7,10), X[,1])

likfuncmat <- function(theta, dat)
{
  s <- 0
  for (m in 1:ncol(dat))
  {
    s <- s + likfunc(theta, dat[,m])
  }
  return(s)
}

# these optimization frameworks work better, but still not great
o <- optim(par = c(1,2,3,6,5,4,9,8,7,10), likfuncmat, dat = X, control = list(maxit = 1000))
o
rank(o$par)

# the simulated annealing method works well
o <- optim(par = c(1,2,3,6,5,4,9,8,7,10), fn = likfuncmat, gr = generateNewTheta, 
           dat = X, method = "SANN", control = list(trace = 1))
o
cbind(rank(apply(X, 1, mean, na.rm = TRUE)), o$par)

o <- optim(par = c(1,2,3,6,5,4,9,8,7,10), fn = likfuncmat, gr = generateNewTheta, 
           dat = X2, method = "SANN", control = list(trace = 1))
o
cbind(rank(apply(X2, 1, mean, na.rm = TRUE)), o$par)

################################################################################
# Attempt the same model in Stan

# use this method if your PC does not have the Rtools directory in its path
# check
devtools::find_rtools()
# if not found, use this
add_rtools_path_for_devtools <- function(rtoolsBinPath=file.path("C:", "Rtools", "bin"),
                                         gccBinPath=file.path("C:", "Rtools", "gcc-4.6.3", "bin"))
{
  require(devtools)
  # expected message about not finding Rtools
  if (!suppressMessages(devtools::find_rtools()))
  {
    add_path(rtoolsBinPath, after = 0)
    add_path(gccBinPath, after = 1)
    if (!devtools::find_rtools())
    {
      message <- c("added Path is still not correct", devtools::get_path())
      stop(message)
    }
    return(TRUE)
  }
  return(TRUE)
}
add_rtools_path_for_devtools()

############

likfunc <- function(theta, x)
{
  maxx <- max(x, na.rm = TRUE)
  maxt <- max(theta, na.rm = TRUE)
  stopifnot(length(x) == length(theta))
  s <- 0
  for (i in 1:(length(x) - 1))
  {
    for (j in (i + 1):length(x))
    {
      if (is.na(x[i]) || is.na(x[j]))
        next
      if ((x[i] < x[j] && theta[i] < theta[j]) || (x[i] > x[j] && theta[i] > theta[j]))
        s <- s + exp(-1*((x[i] - x[j])/maxx - (theta[i] - theta[j])/maxt)^2)
    }
  }
  return(s)
}

likfunc(1:10, 1:10)
likfunc(c(1,2,3,5,4,6,7,8,9,10), 1:10)
likfunc(c(2,1,3,5,4,6,7,8,9,10), 1:10)
likfunc(10:1, 1:10)
likfunc(c(1,2,3,6,5,4,9,8,7,10), X[,1])

stanmodelcode <- "
functions {
  real likfunc(vector theta1, vector x, real stan_na)
  {
    real s = 0;
    int M = num_elements(x);
    real maxx = max(x);
    real maxt = max(theta1);
    
    if (fabs(maxx - stan_na) < 1E-9)
      return 0.0;
    for (i in 1:(M - 1))
    {
      for (j in (i+1):M)
      {
        if (fabs(x[i] - stan_na) > 1E-9 && 
            fabs(x[j] - stan_na) > 1E-9)
        {
          s += exp(-1*((x[i] - x[j])/maxx - (theta1[i] - theta1[j])/maxt)^2);
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
}

parameters {
  vector<lower=0, upper=1>[Noptions] theta;
} 

model {
  // theta ~ dirichlet(alpha);
  y ~ likfuncmat(theta, stan_na);
} 
"

# stan can't read in missing, so use -9999
y <- X
y[which(is.na(X), arr.ind = TRUE)] <- stan_na
alpha <- c(1,2,3,6,5,4,9,8,7,10)
dat <- list(Npeople = N, 
            Noptions = M, 
            y = y, 
            alpha = alpha,
            stan_na = stan_na);
fit <- stan(model_code = stanmodelcode, model_name = "example", 
            data = dat, iter = 2012, chains = 3,
            verbose = TRUE)

print(fit)
rank(get_posterior_mean(fit)[1:M,4])
traceplot(fit)

y <- X2
y[which(is.na(X2), arr.ind = TRUE)] <- stan_na
alpha <- c(1,2,3,6,5,4,9,8,7,10)
dat <- list(Npeople = N, 
            Noptions = M, 
            y = y, 
            alpha = alpha,
            stan_na = stan_na);
fit2 <- stan(model_code = stanmodelcode, model_name = "example", 
            data = dat, iter = 2012, chains = 3,
            verbose = TRUE)

print(fit2)
rank(get_posterior_mean(fit2)[1:M,4])
traceplot(fit2)

###############################################################################

stanmodelcode2 <- "
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
}

parameters {
  vector<lower=0, upper=1>[Noptions] theta;
} 

model {
  // theta ~ dirichlet(alpha);
  y ~ likfuncmat(theta, stan_na);
} 
"

# stan can't read in missing, so use -9999
y <- X
y[which(is.na(X), arr.ind = TRUE)] <- stan_na
alpha <- c(1,2,3,6,5,4,9,8,7,10)
dat <- list(Npeople = N, 
            Noptions = M, 
            y = y, 
            alpha = alpha,
            stan_na = stan_na);
fit <- stan(model_code = stanmodelcode2, model_name = "example2", 
            data = dat, iter = 2012, chains = 3, warmup=2000, thin=1,
            verbose = TRUE)

print(fit)
rank(get_posterior_mean(fit)[1:M,4])
traceplot(fit)

y <- X2
y[which(is.na(X2), arr.ind = TRUE)] <- stan_na
alpha <- c(1,2,3,6,5,4,9,8,7,10)
dat <- list(Npeople = N, 
            Noptions = M, 
            y = y, 
            alpha = alpha,
            stan_na = stan_na);
fit2 <- stan(model_code = stanmodelcode2, model_name = "example2", 
             data = dat, iter = 2012, chains = 3,
             verbose = TRUE)

print(fit2)
rank(get_posterior_mean(fit2)[1:M,4])
traceplot(fit2)

###############################################################################

stanmodelcode3 <- "
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

# stan can't read in missing, so use -9999
y <- X
y[which(is.na(X), arr.ind = TRUE)] <- stan_na
alpha <- rep(0.5, 10)
sigma <- matrix(-0.1, M, M)
diag(sigma) <- 5
dat <- list(Npeople = N, 
            Noptions = M, 
            y = y, 
            alpha = alpha,
            stan_na = stan_na,
            sigma = sigma)
fit <- stan(model_code = stanmodelcode3, model_name = "example3", 
            data = dat, iter = 3000, chains = 3, warmup=1000, thin=1,
            verbose = TRUE)

print(fit)
rank(get_posterior_mean(fit)[1:M,4])
traceplot(fit) + ylim(0, 1)


