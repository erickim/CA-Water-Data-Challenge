require(dplyr)
require(forcats)
require(foreign) ## for read.dbf

####################################################
## Clean 9. CA Drinking Water Quality (by Suppliers)
####################################################

## Data downloaded on 09/20/2018 from
## https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/EDTlibrary.html
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

fetch_water_quality_analyses <- function(cache_level = c('0', '1', '2')) {
    cache_level <- match.arg(cache_level)
    
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
        )
    
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

