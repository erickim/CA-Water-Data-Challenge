## !! you only need to run this once !!

# gsheets doc with the datasets was downloaded 9/6/18 at 7 am
# there may have been more links added since
library(rvest)
library(readxl)
library(tidyverse)

gsheet_path <- "data/datasets-gsheets.xlsx"

sheets <- excel_sheets(gsheet_path)
sheets_dir <- gsub(" ", "-", sheets)

docs <- lapply(sheets,
               function(sheet) {
                 read_excel(gsheet_path, sheet = sheet)
                 })
names(docs) <- sheets

# if youre on pc this probably wont work
for (i in sheets_dir) {
  system(paste0("mkdir data/", i))
}

# I'm looking to use purrr's pluck here, not rvest's
links <- lapply(sheets[1:2],
                function(sheet) {
                  docs %>%
                    pluck(sheet) %>%
                    filter(grepl("http", `Link to Open Data Format`)) %>%
                    pull(`Link to Open Data Format`)
                })
names(links) <- sheets[1:2]

download_data <- function(links) {
  links %>%
    read_html() %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    data.frame(links = .) %>%
    filter(grepl(pattern = "pdf|csv|json", links)) %>%
    pull(links) %>%
    as.character()
}

# downloading recommended links
recommended_links <- links %>%
  pluck(sheets[1]) %>%
  map(download_data)
recommended_links[[17]] <- links %>% pluck(sheets[1]) %>% pluck(17)
names(recommended_links) <- docs %>%
  pluck(sheets[1]) %>%
  filter(grepl("http", `Link to Open Data Format`)) %>%
  pull(`Data Set Name`) %>%
  gsub(pattern = "\\(|\\)|\\r\\n|-|\\*|,|:|\\/|\\'", replacement = "") %>%
  gsub(pattern = " ", replacement = "_")
# no easily downloadable file for this one
recommended_links$USGS_Groundwater_Levels <- NULL

for (i in names(recommended_links)) {
  command <- paste0("mkdir data/", sheets_dir[1], "/", i)
  system(command)
  
  links <- recommended_links %>% pluck(i)
  
  # prevent overflowing of connection
  Sys.sleep(2)
  
  for (j in 1:length(links)) {
    start <- links[j] %>% regexpr(pattern = "\\/[^\\/]*$")
    
    file <- paste0("data/", sheets_dir[1], "/", i, "/",
                   substring(links[j],
                             first = start + 1,
                             last = nchar(links[j])))
    
    download.file(url = links[j], destfile = file)
  }
}


# downloading wolfram candidates linkes
wolfram_candidates_links <- links %>%
  pluck(sheets[2]) %>%
  map(download_data)
names(wolfram_candidates_links) <- docs %>%
  pluck(sheets[2]) %>%
  filter(grepl("http", `Link to Open Data Format`)) %>%
  pull(`Data Set Name`) %>%
  gsub(pattern = "\\(|\\)|\\r\\n|-|\\*|,|:|\\/|\\'", replacement = "") %>%
  gsub(pattern = " ", replacement = "_")

for (i in names(recommended_links)) {
  command <- paste0("mkdir data/", sheets_dir[2], "/", i)
  system(command)
  
  links <- recommended_links %>% pluck(i)
  
  # prevent overflowing of connection
  Sys.sleep(2)
  
  for (j in 1:length(links)) {
    start <- links[j] %>% regexpr(pattern = "\\/[^\\/]*$")
    
    file <- paste0("data/", sheets_dir[2], "/", i, "/",
                   substring(links[j],
                             first = start + 1,
                             last = nchar(links[j])))
    
    download.file(url = links[j], destfile = file)
  }
}


# should get the following links
# dict
# https://data.ca.gov/sites/default/files/PUBLIC%20WATER%20SYSTEMS%20DATA%20DICTIONARY_0.pdf
# data
# https://data.ca.gov/sites/default/files/Public%20Potable%20Water%20Systems%20FINAL%2006-22-2018_0.csv