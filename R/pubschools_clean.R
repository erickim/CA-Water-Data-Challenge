#""""""""""#
# cleaning #
#""""""""""#

library(tidyverse)

public_schools <- read_tsv("https://www.cde.ca.gov/schooldirectory/report?rid=dl1&tp=txt")

public_schools_active <- public_schools %>%
  filter(StatusType == "Active") %>%
  mutate(missing = is.na(AdmEmail1) &
           is.na(AdmEmail2) &
           is.na(AdmEmail3))

public_schools_active %>%
  filter(missing) %>%
  write_csv(path = "data/pubschls_active_missingcontacts.csv")

public_schools_active %>%
  filter(!missing) %>%
  write_csv(path = "data/pubschls_active.csv")
