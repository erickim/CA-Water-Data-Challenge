## A suite of functions for cleaning and merging datasets.

# Load convenience functions,
library(here)
here = here::here
library(glue)
library(pryr)
## For common R-specific tasks
source(glue("{here()}/R/convenience.R"))
library(sf)

#############################
#### DOWNLOAD & CLEANING ####
read_water_service_areas <- function(link = "https://data.ca.gov/sites/default/files/service_areas.kml") {
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

#############################

#################
#### MERGING ####
geo_join <- function(points_df, poly_df, 
                     lon_col = "lon", 
                     lat_col = "lat") {  
  points_df %>% 
    st_as_sf(coords = c(lon_col, lat_col)) %>%
    st_join(poly_df, join = st_intersects)
}

geo_join.test <- function(poly_df = watersystems) {
  fake_data <- 
    data_frame(id = c("My House in Berkeley", "Should by Linda Vista"),
               lat = c(37.863444, 36.42647),
               lon = c(-122.256300, -120.0538)) %>% 
    st_as_sf(coords = c("lon", "lat"), 
             crs = st_crs(watersystems))
  
  fake_data %>% 
    geo_join(poly_df, "lon", "lat")
}
#################