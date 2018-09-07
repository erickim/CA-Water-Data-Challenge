## !! you only need to run this once !!

# gsheets doc with the datasets was downloaded 9/6/18 at 7 am
# there may have been more links added since
library(rvest)
library(readxl)
library(tidyverse)

gsheet_path <- "data/datasets-gsheets.xlsx"

doc <- read_excel(gsheet_path, sheet = "Recommended")

# I'm looking to use purrr's pluck here, not rvest's
links <- doc %>%
  filter(grepl("http", `Link to Open Data Format`)) %>%
  pull(`Link to Open Data Format`)

download_data <- function(links) {
  links %>%
    read_html() %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    data.frame(link = .) %>%
    filter(grepl(pattern = "pdf|csv|json", link)) %>%
    pull(link) %>%
    as.character()
}

# downloading recommended links
doc_links <- links %>%
  map(download_data)
doc_links[[17]] <- links %>% pluck(17)
names(doc_links) <- doc %>%
  filter(grepl("http", `Link to Open Data Format`)) %>%
  pull(`Data Set Name`) %>%
  gsub(pattern = "\\(|\\)|\\r\\n|-|\\*|,|:|\\/|\\'|\\.", replacement = "") %>%
  gsub(pattern = " ", replacement = "_")
# no easily downloadable file for this one
doc_links$USGS_Groundwater_Levels <- NULL

for (i in names(doc_links)[13:16]) {
  command <- paste0("mkdir data/", i)
  system(command)

  loop_links <- doc_links %>% pluck(i)

  for (j in 1:length(loop_links)) {
    start <- loop_links[j] %>% regexpr(pattern = "\\/[^\\/]*$")

    file <- paste0("data/", i, "/",
                   substring(loop_links[j],
                             first = start + 1,
                             last = nchar(loop_links[j])))

    download.file(url = loop_links[j], destfile = file)
  }
}
