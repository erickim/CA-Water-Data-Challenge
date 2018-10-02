suppressPackageStartupMessages(library(gmailr))

use_secret_file('data/water-alarm-gmailr.json')

## returns dataframe of the shape (to, subject, body)
generate_content <- function(water) {
  generate_analysis_summary <- function(analysis) {
    analysis %>%
      arrange(chem_id, desc(sample_date)) %>%
      group_by(chem_name) %>%
      summarise(system_name = first(system_name),
                mcl = first(mcl),
                percent_samples_over_mcl = round(100*mean(result >= mcl), 2),
                recent_test_date = first(sample_date),
                recent_result = first(result),
                recent_result_percent_over_mcl = round(100*((recent_result / mcl) - 1), 2)) %>%
      ungroup() %>%
      filter(recent_result_percent_over_mcl >= 0)
  }
  
  generate_body <- function(analysis_summary) {
    system_name <- 
      analysis_summary %$% unique(system_name)
    
    header <-
      glue("We are a group of statistical consultants and biostatisticians behind a product called The Water Alarm for Children. Using the most recent state-level water test data (https://data.ca.gov/dataset/drinking-water-%E2%80%93-laboratory-water-quality-results), The Water Alarm for Children has detected water safety violations that may put students at your school at risk.\n\nWe are contacting you because you are part of the {system_name} Public Water Service region, and recent tests have indicated unsafe levels of known hazardous contamination in your drinking water supply - specifically, contaminants that are known to have especially harmful impacts on children.")
    
    generate_bullet <- function(system_name,
                                chem_name, 
                                mcl, 
                                percent_samples_over_mcl, 
                                recent_test_date, 
                                recent_result, 
                                recent_result_percent_over_mcl) {
      chem_name <- chem_name %>% str_to_lower() %>% str_extract("[:word:]+")
      
      percent_statement <- if_else(recent_result_percent_over_mcl == Inf, 
                                   ". There is no safe level of consumption for this contaminant", 
                                   paste0(" by ", recent_result_percent_over_mcl, "%"))
      
      consumes <- function(chem_name, recent_result) {
        if (chem_name == "turbidity") 
          return("")
        glue("Currently, an average child at your school may consume as much as {1.5*recent_result} micrograms of {chem_name} in a school day.")
      }
      
      in_the_last_two_years <- function(chem_name, percent_samples_over_mcl) {
        if (chem_name == "lead") 
          return("")
        glue("In the last two years, {percent_samples_over_mcl}% of samples in your region have breached this limit. ")
      }
      glue("The most recent test for {chem_name} in your water supply (conducted on {recent_test_date}) indicates that {chem_name} exceeds the current US EPA Maximal Contaminant Level{percent_statement}. {in_the_last_two_years(chem_name, percent_samples_over_mcl)}{consumes(chem_name, recent_result)}")
    }    
    
    bullets <-
      analysis_summary %>%
      mutate(chem_name = as.character(chem_name), recent_test_date = as.character(recent_test_date)) %>%
      pmap_chr(generate_bullet) %>%
      paste(collapse = "\n")
    
    guidance <-
      "In light of these safety hazards, we urge you to consider providing water bottled from other sources for the children at your school. We have also created a Facebook group (https://www.facebook.com/groups/more.than.a.meter/) for schools, parents, and researchers to get involved and start fixing these water safety hazards, school by school. Please alert your PTA and any other members of your community who share an interest in ensuring the safety of the public water supply for California's children."
    
    body <- 
      glue("{header}\n\n{bullets}\n\n{guidance}")
    
    if (nrow(analysis_summary) == 0) return("") 
    body
  }
  
  st_geometry(water) <- NULL
  water %>%
    transmute(schools = schools %>% map(~ {df <- select(., to = Email, school = School); st_geometry(df) <- NULL; df}),
              body = analyses %>% map_chr(~ generate_analysis_summary(.) %>% generate_body())) %>%
    unnest() %>%
    filter(body != "") %>%
    select(to, school, body)
}

send_email <- function(to = "contact@arun.run", 
                       school = "Travilah Elementary School", 
                       body = "Nothing to see here...") {
  mime(To = to,
       From = "arun.ramamurthy.md@gmail.com",
       Subject = glue("Water Safety Alert Concerning {school}"),
       body = glue("To the Main Office of {school}:\n\n{body}\n\nWarm regards,\nMore than a Meter")) %>%
    send_message()
}

send_emails <- function(emails, dry_run = T) {
  if (dry_run) {
    emails %<>% 
      sample_n(5) %>%
      mutate(to = "contact@arun.run")
    print(emails)
  }
  emails %>% pwalk(send_email)
}

send_batch_email <- function(water, dry_run = T) {
  water %>%
    generate_content() %>%
    send_emails(dry_run)
}