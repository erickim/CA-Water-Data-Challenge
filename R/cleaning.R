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

CHEMICALS <- c("ARSENIC",
               "BENZENE",
               "TOLUENE",
               "ETHYLBENZENE",
               "TURBIDITY, LABORATORY",
               "MERCURY",
               "LEAD",
               "CADMIUM")

#############################
#### DOWNLOAD & CLEANING ####
fetch_water_service_areas <- function(link = "https://data.ca.gov/sites/default/files/service_areas.kml") {
  link %>%
    read_sf() %>%
    transmute(system_id = str_sub(pwsid, start = 3),
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
              geometry) %>%
    unique.data.frame()
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
    
    dir.recreate(dir)
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
    ) %>%
    filter(source_status %in% c("AR", "AT", "AU")) %>%
    left_join(systems, by = "system_id")
  
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
  geometries <-
    watersystems %>% select(system_id, geometry)
  pws_base <- 
    watersystems %>% select(-geometry) %>% group_by(system_id) %>% nest(.key = "pws_info")
  schools_nest <- 
    schools %>% 
    filter(SOCType == "Elementary Schools (Public)", 
           ! is.na(AdmEmail1)) %>%
    transmute(School, 
              City, County, 
              Name = paste(AdmFName1, AdmLName1), 
              Email = AdmEmail1,
              Longitude, Latitude) %>%
    geo_join(watersystems, 'Longitude', 'Latitude') %>%
    select(School, 
           City, County, 
           Name, Email,
           system_id,
           system_name = natural_name) %>%
    group_by(system_id) %>%
    nest(.key = "schools")
  analyses_nest <-
    analyses %>% 
    filter(chem_name %in% CHEMICALS, sample_date >= ymd("2017-01-01"),
           result > 0) %>% ## cuts down to 82935 rows (1% of total)
    transmute(sample_date, 
              chem_id, chem_name, chem_aka1, chem_type, chem_code,
              result_modifier, chem_units, result,
              mcl, percent_over_mcl = round(100*((result / mcl) - 1), 2),  over_mcl = result >= mcl,
              rphl, percent_over_rphl = round(100*((result / rphl) - 1), 2), over_rphl = result >= rphl,
              system_id, system_name = str_to_title(system_name), supplier,
              source_type, source_name, source_status) %>%
    group_by(system_id) %>% 
    nest(.key = "analyses")
  water_data <-
    geometries %>%
    inner_join(pws_base) %>%
    inner_join(schools_nest) %>%
    inner_join(analyses_nest)
}

fetch_clean_dataframe <- function() {
  watersystems <- fetch_water_service_areas()
  schools <- fetch_active_public_schools()
  analyses <- fetch_water_quality_analyses()
  
  merge_water_data(watersystems, schools, analyses)
}

get_nested_column <- function(water,
                              desired_system_id = "0110005", ## East Bay Mud by default
                              col = analyses) { ## col: one of (analyses, schools)
  col <- enquo(col)
  water %>% 
    filter(system_id == desired_system_id) %>%
    pull(!! col) %>%
    extract2(1)
}

summarize_water_data <- function(water, retain_geometry = T) {
  summary <-
    water %>%
    transmute(system_id, geometry,
              `Number of Schools` = schools %>% map_int(nrow),
              `Number of (Dangerous) Samples since 2017` = analyses %>% map_int(nrow),
              `Proportion of Chemicals over MCL` = 
                analyses %>% map_dbl(~ mean(.$over_mcl)),
              `Highest Percentage over MCL` = 
                analyses %>% map_dbl(~ max(.$percent_over_mcl)),
              `Offending Chemical over MCL` = 
                analyses %>% map_chr(~ as.character(.$chem_name)[which.max(.$percent_over_mcl)]),
              `Proportion of Chemicals over RPHL` = 
                analyses %>% map_dbl(~ mean(.$over_rphl)),
              `Highest Percentage over RPHL` = 
                analyses %>% map_dbl(~ max(.$percent_over_rphl)),
              `Offending Chemical over RPHL` = 
                analyses %>% map_chr(~ as.character(.$chem_name)[which.max(.$percent_over_rphl)])) %>%
    arrange(desc(`Proportion of Chemicals over MCL`))
  if (! retain_geometry) st_geometry(summary) <- NULL
  summary
}

## TODO (iff school-level geometries are required)
unnest_water_data <- function(water) {
  
}
#################