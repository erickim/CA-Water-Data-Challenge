## A suite of functions for cleaning and merging datasets.

# Load convenience functions,
library(here)
here = here::here
library(glue)
library(pryr)
library(sf)
library(foreign)
library(forcats)

## For common R-specific tasks
source(glue("{here()}/R/convenience.R"))


#############################
#### DOWNLOAD & CLEANING ####
fetch_water_service_areas <- function(link = "https://data.ca.gov/sites/default/files/service_areas.kml") {
  link %>%
    read_sf() %>%
    transmute(region_id = pwsid,
              name = Name,
              natural_name = str_to_title(name),
              city = address_city_name,
              mailing_address = 
                glue("{addr_line_one_txt}, {addr_line_two_txt}{address_zip}",
                     addr_line_two_txt = recode(addr_line_two_txt + " ", 
                                                'NA ' = '')) %>% 
                as.character(),
              zipcode = address_zip,
              county = d_prin_cnty_svd_nm,
              owner_type_code = as.factor(owner_type_code),
              fed_type = as.factor(d_pws_fed_type_cd),
              population = d_population_count,
              geometry)
}

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

fetch_water_quality_analyses <- function(cache_level = c('0', '1', '2'), 
                                         dir = "data/water_quality",
                                         fresh_fetch = FALSE) {
  cache_level <- match.arg(cache_level)
  
  fetch_raw_dbfs <- function(dir = "data/water_quality/") {
    link_chemical <- "https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/edtlibrary/chemical.zip"
    link_siteloc <- "https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/edtlibrary/siteloc.zip"
    link_watersys <- "https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/edtlibrary/watsys.zip"
    link_storet <- "https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/edtlibrary/storet.zip"
    
    create_if_needed(dir)
    c(link_chemical, link_siteloc, link_watersys, link_storet) %>%
      walk2(c("chemical", "siteloc", "watsys", "storet"), 
            function(link, name) {
              zip_file <- paste0(dir, name, ".zip")
              download.file(link, zip_file) 
              unzip(zip_file, exdir = dir)
              unlink(zip_file)
            }
      )
  }
  
  if (fresh_fetch) fetch_raw_dbfs(dir)
  
  ## Water systems.
  systems <-  read.dbf("data/water_quality/watsys.dbf") %>%
    as_tibble %>%
    transmute(
      system_id = as.character(SYSTEM_NO),
      system_name = SYSTEM_NAM,
      supplier = HQNAME,
      population = as.numeric(POP_SERV),
      num_connections = as.numeric(CONNECTION),
      area = AREA_SERVE
    )
  
  if(cache_level > 1) 
    saveRDS(systems, "data/water_quality/clean/systems.Rds")
  
  ## Drinking water sources.
  sources <-  read.dbf("data/water_quality/siteloc.dbf") %>%
    as_tibble %>%
    transmute(
      source_id = as.character(PRI_STA_C),
      county = COUNTY,
      district = DISTRICT,
      system_id = as.character(SYSTEM_NO), ## foreign key in water_sys
      source_type = WATER_TYPE,
      source_name = SOURCE_NAM,
      source_status = STATUS
    ) %>%
    mutate(
      source_type = fct_recode(
        source_type,
        NULL = "C",
        `well/groundwater` = "g",
        `well/groundwater` = "G",
        mixed = "M",
        NULL = "P",
        surface = "S",
        NULL = "T",
        waste = "W"
      )
    ) %>% left_join(systems)
  
  if(cache_level > 1) 
    saveRDS(sources, "data/water_quality/clean/sources.Rds")
  
  ## Chemicals 
  chemicals <- read.dbf("data/water_quality/storet.dbf") %>%
    as_tibble %>%
    transmute(
      chem_id = as.character(STORE_NUM),
      chem_name = CHEMICAL__,
      chem_aka1 = AKA1,
      chem_aka2 = AKA2,
      chem_type = CLS,
      chem_code = RPT_CDE,
      chem_units = RPT_UNIT,
      mcl = MCL, ## maximum contaminant level
      rphl = RPHL ## recommended public health level
    ) %>%
    mutate(
      chem_type = fct_recode(
        chem_type,
        agricultural = "A",
        `ion chemistry` = "B",
        purgeable = "P",
        radiological = "R",
        inorganics = "T",
        trihalomethane = "TH",
        other = "X"
      )
    )
  
  if(cache_level > 1) 
    saveRDS(chemicals, "data/water_quality/clean/chemicals.Rds")
  
  ## Water sample analysis results.
  ## This is for 2012/01/02 through 2018/09/17.
  analyses <- read.dbf("data/water_quality/chemical.dbf") %>%
    as_tibble %>%
    transmute(
      lab_id = as.character(LAB_NUM), ## foreign key in lab.dbf (not used)
      source_id = as.character(PRIM_STA_C), ## foreign key in site_locs
      sample_date = SAMP_DATE,
      chem_id = as.character(STORE_NUM), ## foreign key in storet.dbf
      result_modifier = XMOD,
      result = FINDING
    ) %>% left_join(chemicals) %>% left_join(sources) %>%
    filter(!is.na(result), !is.na(county)) ## removes 2,463 rows
  
  if(cache_level > 0) 
    saveRDS(analyses, "data/water_quality/clean/analyses.Rds")
  
  return(analyses)
}
#############################

#################
#### MERGING ####
geo_join <- function(points_df, poly_df, 
                     lon_col = "lon", 
                     lat_col = "lat") {  
  points_df %>% 
    st_as_sf(coords = c(lon_col, lat_col),
             crs = st_crs(poly_df)) %>%
    st_join(poly_df, join = st_intersects)
}

geo_join.test <- function(poly_df = watersystems) {
  fake_data <- 
    data_frame(id = c("My House in Berkeley", "Should be Linda Vista", "Third Entry, Alameda"),
               lat = c(37.863444, 36.42647, 37.84737),
               lon = c(-122.256300, -120.0538, -122.2836))
  
  fake_data %>% 
    geo_join(poly_df, "lon", "lat")
}

merge_water_data <- function(watersystems, schools, analyses) {
  geo_join(schools, watersystems, 'Longitude', 'Latitude') %>%
    left_join(analyses %>% mutate(region_id = paste0("CA", system_id)), 
              by = "region_id")
}

fetch_clean_dataframe <- function() {
  watersystems <- fetch_water_service_areas()
  schools <- fetch_active_public_schools()
  analyses <- fetch_water_quality_analyses()
  
  merge_water_data(watersystems, schools, analyses)
}
#################