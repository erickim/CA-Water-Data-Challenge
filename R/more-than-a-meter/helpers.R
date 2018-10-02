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
                analyses %>% map_chr(~ as.character(.$chem_name)[which.max(.$percent_over_mcl)])) %>%
    arrange(desc(`Proportion of Chemicals over MCL`))
  if (! retain_geometry) st_geometry(summary) <- NULL
  summary
}
