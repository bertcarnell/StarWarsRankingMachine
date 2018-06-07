stopifnot(require(assertthat))
#tmp <- assertthat::assert_that(require(rvest))
tmp <- assertthat::assert_that(require(RSelenium))

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

###############################################################################

queryForArticleType <- function(type, html_doc, date, html_article_vector, link_vector)
{
  temp_ho <- html_nodes(html_doc, paste0("span.field-content :contains('",type,"')")) %>% 
    html_attr("href")
  if (length(temp_ho) > 0)
  {
    temp_html <- paste0('<a href="https://www.genealogybank.com', temp_ho, '">', date, '</a>')
    links <- c(link_vector, paste0("https://www.genealogybank.com", temp_ho))
    results <- c(html_article_vector, temp_html)
    return(list(results=results, bfound=TRUE, links=links))
  } else
  {
    return(list(results=html_article_vector, bfound=FALSE, links=link_vector))
  }
}

queryAdler <- function(article_type, sDateStart, sDateEnd, query_output_file, login_session)
{
  dateToSearchMin <- as.Date(sDateStart)
  dateToSearchMax <- as.Date(sDateEnd)
  
  tuesdays <- findTuesdays(dateToSearchMin, dateToSearchMax)
  print(paste0("Querying for ", length(tuesdays$tuesdays), " days"))
  results <- character()
  links <- character()
  for (i in seq_along(tuesdays$html_tuesdays))
  {
    print(tuesdays$tuesdays[i])
    query1 <- paste0("https://www.genealogybank.com/explore/newspapers/all/usa/pennsylvania/reading/reading-adler",
                     "?fname=",
                     "&lname=",
                     "&fullname=",
                     "&rgfromDate=", tuesdays$html_tuesdays[i],
                     "&rgtoDate=", tuesdays$html_tuesdays[i],
                     "&formDate=",
                     "&formDateFlex=exact",
                     "&dateType=range",
                     "&kwinc=",
                     "&kwexc=")
    test <- TRUE  
    while (test)
    {
      tryCatch(expr = {
        page1_session <- jump_to(login_session, query1)
        page1_html <- read_html(page1_session)
      }, error = function(e) {print("Error with query"); return(links)})
      if (article_type == "Obit")
      {
        temp <- queryForArticleType("Historical Obituary", page1_html, tuesdays$tuesdays[i], 
                                    results, links)
        results <- temp$results
        bfound <- temp$bfound
        links <- temp$links
        temp <- queryForArticleType("Mortuary Notice", page1_html, tuesdays$tuesdays[i], 
                                    results, links)
        results <- temp$results
        links <- temp$links
        bfound <- bfound | temp$bfound
      }
      else if (article_type == "Marriage")
      {
        temp <- queryForArticleType("Matrimony Notice", page1_html, tuesdays$tuesdays[i], 
                                    results, links)
        results <- temp$results
        links <- temp$links
        bfound <- temp$bfound
        temp <- queryForArticleType("Marriage/Engagement Notice", page1_html, tuesdays$tuesdays[i], 
                                    results, links)
        results <- temp$results
        links <- temp$links
        bfound <- bfound | temp$bfound
      } else
      {
        stop("Article type not recognized")
      }
      
      # go to the next page if nothing was found
      if (!bfound)
      {
        temp_next <- html_nodes(page1_html, "li.page-item.page-next") %>% html_children() %>% 
          html_attr("href")
        if (length(temp_next) == 1)
        {
          test <- TRUE
          query1 <- paste0("https://www.genealogybank.com", temp_next)
          print("Next Page")
        } else if (length(temp_next) == 0)
        {
          test <- FALSE
        } else
        {
          stop("error unknown")
        }
      } else
      {
        test <- FALSE
      }
    }
  }
  
  if (length(results) > 0)
  {
    results_html <- read_html(paste("<p>", paste(results, collapse="</p><p>"), "</p>"))
    write_html(results_html, query_output_file, options = "format")
  } else
  {
    stop("No Results Found")
  }
  return(links)
}


###############################################################################

pgsession <- html_session(url)
pgsession <- follow_link(pgsession, "LOG IN")

pgform <- html_form(pgsession)[[1]] 

filled_form <- set_values(pgform,
                          `username` = "bertcarnell", 
                          `password` = passwd_str)

pg_session_logged_in <- submit_form(pgsession, filled_form, submit = '<unnamed>')

page1_html <- read_html(pgsession)

html_nodes(page1_html, "a.log-in.simplified_and_signup.static-buttons")

temp_ho <- html_nodes(html_doc, paste0("span.field-content :contains('",type,"')")) %>% 
  html_attr("href")
pgform <- html_form(pgsession)[[1]] 

filled_form <- set_values(pgform,
                          `username` = "bertcarnell@gmail.com", 
                          `password` = passwd_str)

pg_session_logged_in <- submit_form(pgsession, filled_form)


  filled_form <- set_values(pgform,
                            `username` = "bertcarnell@gmail.com", 
                            `password` = passwd_str)
  
  pg_session_logged_in <- submit_form(pgsession, filled_form)
  
  #links <- queryAdler("Obit", "1796-11-29", "1798-01-01", output_file, pg_session_logged_in)
  #links <- queryAdler("Obit", "1798-01-01", "1810-01-01", output_file, pg_session_logged_in)
  
  #marriage_links <- queryAdler("Marriage", "1796-11-29", "1798-01-01", output_file_marriage, 
  #                             pg_session_logged_in)
  #marriage_links <- queryAdler("Marriage", "1798-01-01", "1810-01-01", output_file_marriage, 
  #                             pg_session_logged_in)
  
  #save(links, marriage_links, file = file.path(output_dir, "links1798.Rdata"))
  #save(links, marriage_links, file = file.path(output_dir, "links1810.Rdata"))
  #load(file = file.path(output_dir, "links1798.Rdata"))
  #load(file = file.path(output_dir, "links1810.Rdata"))
  
  if (opt$article_type == "obit")
  {
    links <- queryAdler("Obit", opt$start_date, opt$end_date, output_file, pg_session_logged_in)
  } else if (opt$article_type == "marriage")
  {
    links <- queryAdler("Marriage", opt$start_date, opt$end_date, output_file, pg_session_logged_in)
  } else
  {
    stop("Article Type Not Recognized")
  }
  save(links, file = output_save_file)
}

if (opt$get_image)
{
  load(file = output_save_file)
  remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                   port = 4444L,
                                   browserName = "chrome")
  driver_stats <- remDr$open()
  assertthat::assert_that(tolower(driver_stats$browserName) == "chrome")
  
  #######
  # login
  remDr$navigate(url)
  elem <- remDr$findElement(using="id", value="dropdowntoggle-login")
  elem$clickElement()
  elem <- remDr$findElement(using="id", value="username")
  elem$sendKeysToElement(list('bertcarnell@gmail.com'))
  
  elem <- remDr$findElement(using="id", value="password")
  elem$sendKeysToElement(list(passwd_str))
  elem <- remDr$findElement(using="id", value="btnLogin")
  elem$clickElement()
  
  for (i in 1:length(links))
  {
    print(paste0(i, ": ", links[i]))
    print("Downloading...")
    tryCatch({
      if (opt$article_type == "obit")
      {
        remDr <- getArticleImage(remDr, links[i], "Obituary")
      } else if (opt$article_type == "marriage")
      {
        remDr <- getArticleImage(remDr, links[i], "Marriage")
      } else
      {
        stop("Article Type Not Recognized")
      }
    }, error = function(e){
      print(e)
      cat("trying again...\n")
      Sys.sleep(5)
      if (opt$article_type == "obit")
      {
        remDr <- getArticleImage(remDr, links[i], "Obituary")
      } else if (opt$article_type == "marriage")
      {
        remDr <- getArticleImage(remDr, links[i], "Marriage")
      } else
      {
        stop("Article Type Not Recognized")
      }
    })
  }
  
  remDr$close()
}


ClientID
NPnyNc7_TN-G3kRwP7ilvw

Secret
8642895571115613401048677272393412540

Access Token
FEk40QGlfQuAYxU5PtnZhq-nSuC9WpOuNPb5vOS1TPt8ekJHiHdSXCnyIW75Y6VTfCv1uAi1XEBq5976RY9fCclRYAWZIrIpv4O6Ab3JUmCMCfj3nb3k9bP22LFT6w.7


