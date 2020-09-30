install.packages("RSelenium")
library(RSelenium)
library(stringr)

# searched keywords
job_titles <- c("data scientist", "data analyst")

# running a driver
remote <- rsDriver(browser = "firefox")
client <- remote[["client"]]
client$navigate("https://www.pracuj.pl/")


# Scrapping offers' list
search_for_offers <- function(keyword){
  
  # offers' list
  kw_input <- client$findElement(using = "css selector", "[data-test=autocomplete-kw-input]")$
    findChildElements(using = "tag name", "input")
  
  kw_input[[1]]$sendKeysToElement(list(keyword))
  kw_input[[1]]$submitElement()
  
  # collecting data
  offers_list <- client$findElement(using = "class name", "results__list-container")$
    findChildElements(using = "class name", "offer-details__title-link")
  
  offers_links <- list()
  
  for(i in 1:length(offers_list)){
    offers_links[[i]] <- unlist(offers_list[[i]]$getElementAttribute("href"))
  }
  
  # filtering off nulls
  offers_links = offers_links[-which(sapply(offers_links, is.null))]
  
  return(offers_links)
}


# Scrapping job requirements
get_requirements <- function(offer_link){
  client$navigate(offer_link)
  Sys.sleep(2)
  
  descr <- NULL
  
  tryCatch(
    expr = {descr <- client$findElement(using = "id", "description")$getElementText()},
    error = function(e){print(paste("No description element on page", offer_link))}
  )
  
  return(c(link = offer_link, descr = unlist(descr)))
}

# Formatting nad saving data
save_data <- function(offers_data, file_name){
  
  # don't count records without description element
  offers_data <- offers_data[-which(sapply(offers_data, function(x){length(x)<2}))]
  
  # turn into data frame
  offers_dt <- unlist(offers_data, recursive = F)
  i <- 1:length(offers_dt)
  offers_dt <- data.frame(link = offers_dt[i%%2 == 1], descr = offers_dt[i%%2 == 0])
  
  # get rid off spaces
  offers_dt$descr <- trimws(offers_dt$descr, which = "both")
  offers_dt$link <- as.character(offers_dt$link)
  
  # save file
  write.table(offers_dt, file_name)
  
}


offers_links <- search_for_offers(job_titles[[1]])
offers_data <- sapply(offers_links, get_requirements)
save_data(offers_data, paste(job_titles[[1]],"offers.csv"))


offers_links <- search_for_offers(job_titles[[2]])
offers_data_2 <- sapply(offers_links, get_requirements)
save_data(offers_data_2, paste(job_titles[[2]],"offers.csv"))


# close the driver and stop the selenium server
client$close()
remote[["server"]]$stop()
