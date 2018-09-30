#""""""""""#
# cleaning #
#""""""""""#

library(tidyverse)

fetch_active_public_schools <- function(tsv_link = "https://www.cde.ca.gov/schooldirectory/report?rid=dl1&tp=txt",
                                        log_missing = T) {
  public_schools <- read_tsv(tsv_link)
  
  public_schools_active <- public_schools %>%
    filter(StatusType == "Active") %>%
    mutate(missing = is.na(AdmEmail1) &
             is.na(AdmEmail2) &
             is.na(AdmEmail3))
  
  if(log_missing) 
    public_schools_active %>%
    filter(missing) %>%
    write_csv(path = "data/pubschls_active_missingcontacts.csv")
  
  public_schools_active %<>% filter(!missing)
}
