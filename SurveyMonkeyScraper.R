# Copyright 2018 Rob Carnell
rm(list=ls())

stopifnot(require(assertthat))
tmp <- assertthat::assert_that(require(RSelenium))
tmp <- assertthat::assert_that(require(magrittr))
tmp <- assertthat::assert_that(require(rvest))

currSys <- tolower(Sys.info()["sysname"])

if (currSys != "linux")
{
  require(rstudioapi)
}

if (currSys == "linux")
{
  repository_path <- file.path("~","repositories","StarWarsRankingMachine")
} else
{
  repository_path <- file.path("C:","Users","Rob","Documents","Repositories",
                               "StarWarsRankingMachine")
}
tmp <- assertthat::assert_that(dir.exists(repository_path))

url <- "https://www.surveymonkey.com/"

if (currSys != "linux")
{
  passwd_str <- rstudioapi::askForPassword("Enter SurveyMonkey pw")
} else 
{
  passwd_str <- ""
}

rD <- rsDriver()
remDr <- rD[["client"]]

#######
# login
remDr$navigate(paste0(url, "user/sign-in"))
elem <- remDr$findElement(using="id", value="username")
elem$sendKeysToElement(list('bertcarnell'))

elem <- remDr$findElement(using="id", value="password")
elem$sendKeysToElement(list(passwd_str))
elem <- remDr$findElement(using="class", value="wds-button")
elem$clickElement()

remDr$navigate(paste0(url, "analyze/86jkOYoEheqIAGUISYfWrC8GF1x4TttJjKrc8B3C4Ysw54ujZcW6on00eLZagCgg"))

Sys.sleep(5)
elem <- remDr$findElement(using="id", value="mode_tab_individual_responses")
elem$clickElement()

Sys.sleep(5)
responseList <- list()
#responseList[[16]] <- remDr$getPageSource()
elem <- remDr$findElement(using="class", value="analyze-pages-content-wrapper")
responseList[[16]] <- elem$getPageSource()

m <- regexpr("RESPONDENTS: [1-9][0-9]* of [1-9][0-9]*", responseList[[16]])
nResponders <- as.numeric(substring(responseList[[16]], 
                                    m + attr(m, "match.length") -2, m + attr(m, "match.length")-1))
m <- regexpr("Respondent [#]", responseList[[16]])
currResponder <- as.numeric(trimws(substring(responseList[[16]], m + attr(m, "match.length"), m + attr(m, "match.length")+2)))
assertthat::assert_that(currResponder == 16)

for (i in (nResponders-1):1)
{
  #i <- 13
  elem <- remDr$findElement(using="class", value="wds-button--arrow-left")
  elem$clickElement()
  Sys.sleep(2)
  #responseList[[i]] <- remDr$getPageSource()
  elem <- remDr$findElement(using="class", value="analyze-pages-content-wrapper")
  responseList[[i]] <- elem$getPageSource()
  currResponder <- as.numeric(trimws(substring(responseList[[i]], m + attr(m, "match.length"), m + attr(m, "match.length")+2)))
  assertthat::assert_that(i == currResponder)
}

# go back to the default
for (i in 2:nResponders)
{
  elem <- remDr$findElement(using="class", value="wds-button--arrow-right")
  elem$clickElement()
}

# get question labels before iterating through questions
questionList <- vector("list", 10)
temp1 <- responseList %>% extract2(16) %>% extract2(1) %>% read_html()
metatemp <- temp1 %>% html_nodes("ul.respondent-info-fields") %>% extract(1) %>%
  html_nodes("span.value")
tempQuestionResponse <- temp1 %>% html_nodes("div.response-question-list") %>% extract(1)
# ensure there are 10 questions
assertthat::assert_that(length(tempQuestionResponse %>% html_nodes("div.question-title")) == 10)
tempQuestionResponse2 <- tempQuestionResponse %>% html_children()

questionList[[1]] <- tempQuestionResponse2 %>% extract(1) %>% 
  html_nodes("div.response-question-title-text") %>% html_text() %>% trimws()
questionList[[2]] <- tempQuestionResponse2 %>% extract(2) %>% 
  html_nodes("div.response-question-title-text") %>% html_text() %>% trimws()
for (j in 3:10)
{
  questionList[[j]] <- tempQuestionResponse2 %>% extract(j) %>% 
    html_nodes("div.response-container.matrix-rating") %>%
    html_children() %>% html_children %>% html_nodes("span.response-text-label") %>%
    html_text() %>% trimws()
}

questionResponseList <- vector("list", nResponders)
for (i in 1:nResponders)
{
  # i <- 1
  questionResponseList[[i]]$data <- vector("list", 10)
  
  extractnum <- ifelse(i == 1, 1, 2)

  temp1 <- responseList %>% extract2(i) %>% extract2(1) %>% read_html()
  metatemp <- temp1 %>% html_nodes("ul.respondent-info-fields") %>% extract(extractnum) %>%
    html_nodes("span.value")
  tempQuestionResponse <- temp1 %>% html_nodes("div.response-question-list") %>% extract(extractnum)
  # ensure there are 10 questions
  assertthat::assert_that(length(tempQuestionResponse %>% html_nodes("div.question-title")) == 10)
  tempQuestionResponse2 <- tempQuestionResponse %>% html_children()
  
  # get the metadata about the respondent
  meta <- list()
  meta$Collector <- metatemp %>% extract(1) %>% html_text %>% trimws()
  meta$Start <- metatemp %>% extract(3) %>% html_text %>% trimws()
  meta$Stop <- metatemp %>% extract(4) %>% html_text %>% trimws()
  meta$Duration <- metatemp %>% extract(5) %>% html_text %>% trimws()
  meta$IP <- metatemp %>% extract(6) %>% html_text %>% trimws()
  questionResponseList[[i]]$meta <- meta
  
  # get answer 1 + 2
  questionResponseList[[i]]$data[[1]] <- tempQuestionResponse2 %>% 
    extract(1) %>% html_nodes("span.response-text") %>%
    html_text() %>% trimws()
  questionResponseList[[i]]$data[[2]] <- tempQuestionResponse2 %>% 
    extract(2) %>% html_nodes("span.response-text") %>%
    html_text() %>% trimws()
  
  # get answer 3 - 10
  for (j in 3:10)
  {
    questionResponseList[[i]]$data[[j]] <- tempQuestionResponse2 %>% 
      extract(j) %>% html_nodes("div.response-container.matrix-rating") %>%
      html_children() %>% html_children %>% html_nodes("div.response-text") %>% 
      html_nodes("span") %>% html_text() %>% trimws()
  }
}

save(responseList, questionList, questionResponseList, 
     file=file.path(repository_path, "RawData.Rdata"))

q1 <- sapply(questionResponseList, function(x) x$data[[1]])
table(q1)
q2 <- sapply(questionResponseList, function(x) x$data[[2]])
mean(as.numeric(q2))
suppressWarnings(sapply(questionResponseList, function(x) as.numeric(x$data[[3]])))
suppressWarnings(sapply(questionResponseList, function(x) as.numeric(x$data[[4]])))
suppressWarnings(sapply(questionResponseList, function(x) as.numeric(x$data[[5]])))
suppressWarnings(sapply(questionResponseList, function(x) as.numeric(x$data[[6]])))
suppressWarnings(sapply(questionResponseList, function(x) as.numeric(x$data[[7]])))
suppressWarnings(sapply(questionResponseList, function(x) as.numeric(x$data[[8]])))
suppressWarnings(sapply(questionResponseList, function(x) as.numeric(x$data[[9]])))
suppressWarnings(sapply(questionResponseList, function(x) as.numeric(x$data[[10]])))

                        
