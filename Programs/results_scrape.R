library(tidyverse)
library(parallel)

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterEvalQ(cl, {library(rvest); library(dplyr); library(httr)})

scrape_search_page <- function(page = 1) {
  url_start <- "http://results.chicagomarathon.com/2017/?page="
  url_end <- "&event=MAR&lang=EN_CAP&num_results=1000&pid=search&search%5Bnation%5D=%25&search_sort=name"
  
  response <- RETRY("GET", paste0(url_start, page, url_end))
  html_result <- read_html(response)
  
  results <- html_result %>%
    html_table() %>%
    `[[`(1) %>%
    `names<-`(make.names(names(.))) %>%
    mutate_at(vars(Place.Overall:Place.Division), as.character) %>%
    as_tibble()
  
  urls <- html_result %>%
    html_nodes("a") %>%
    html_attr("href")
  
  urls <- urls[grep(pattern = "?content=detail", x = urls)]
  
  list(results = results, urls = urls)
}

get_individual_results <- function(url) {
  base_url <- "http://results.chicagomarathon.com/2017/"
  response <- RETRY("GET", paste0(base_url, url))
  
  response %>%
    read_html() %>%
    html_table()
}

pages <- 1:55
search_pages <- parLapply(pages, scrape_search_page, cl = cl) %>% transpose()
results_raw <- bind_rows(search_pages$results)
individual_urls <- unlist(search_pages$urls)
save(results_raw, file = "results_raw.rda")

individual_results_raw <- parLapply(cl, individual_urls, get_individual_results)
save(individual_results_raw, file = "individual_results_raw.rda")