install.packages("RSelenium")
library(RSelenium)
library(stringr)


job_titles <- c("data scientist", "data analyst")



# uruchamianie
binman::list_versions("chromedriver")

remote <- rsDriver(browser = "firefox")
client <- remote[["client"]]

client$navigate("https://www.pracuj.pl/")

keywordInput <- client$findElement(using = "css selector", "[data-test=autocomplete-kw-input]")
searchButton <- client$findElement(using = "css selector", "[data-test=form-submit]")

# wyszukiwanie ofert
kw_input <- client$findElement(using = "css selector", "[data-test=autocomplete-kw-input]")$
  findChildElements(using = "tag name", "input")

kw_input[[1]]$sendKeysToElement(list(job_titles[[1]]))
kw_input[[1]]$submitElement()

# zbieranie danych
offers_list <- client$findElement(using = "class name", "results__list-container")$
  findChildElements(using = "class name", "offer-details__title-link")

offers_links <- list()

for(i in 1:length(offers_list)){
 offers_links[[i]] <- unlist(offers_list[[i]]$getElementAttribute("href"))
}

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

offers_links = offers_links[-which(sapply(offers_links, is.null))]
offers_data <- sapply(offers_links, get_requirements)
offers_data <- offers_data[-which(sapply(offers_data, function(x){length(x)<2}))]

offers_dt <- unlist(offers_data, recursive = F)
i <- 1:length(offers_dt)
offers_dt <- data.frame(link = offers_dt[i%%2 == 1], descr = offers_dt[i%%2 == 0])


offers_dt$descr <- trimws(offers_dt$descr, which = "both")
offers_dt$link <- as.character(offers_dt$link)

write.table(offers_dt, "data_scientist_offers.csv")








kw_input[[1]]$sendKeysToElement(list(job_titles[[2]]))
kw_input[[1]]$submitElement()

# zbieranie danych
offers_list <- client$findElement(using = "class name", "results__list-container")$
  findChildElements(using = "class name", "offer-details__title-link")

offers_links <- list()

for(i in 1:length(offers_list)){
  offers_links[[i]] <- unlist(offers_list[[i]]$getElementAttribute("href"))
}

offers_links = offers_links[-which(sapply(offers_links, is.null))]
offers_data_2 <- sapply(offers_links, get_requirements)
offers_data_2 <- offers_data_2[-which(sapply(offers_data_2, function(x){length(x)<2}))]

offers_dt_2 <- unlist(offers_data_2, recursive = F)
i <- 1:length(offers_dt_2)
offers_dt_2 <- data.frame(link = offers_dt_2[i%%2 == 1], descr = offers_dt_2[i%%2 == 0])

offers_dt_2$descr <- trimws(offers_dt_2$descr, which = "both")
offers_dt_2$link <- as.character(offers_dt_2$link)

write.table(offers_dt_2, "data_analyst_offers.csv")


client$close()
# stop the selenium server
remote[["server"]]$stop()
