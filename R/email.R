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
      glue("We are a group of statistical consultants behind a product called More than a Meter.\n\nWe are contacting you because you are part of the {system_name} Public Water Service region, and recent tests have indicated unsafe levels of known hazardous contamination in your drinking water supply - specifically, contaminants that are known to have especially harmful impacts on children.")
    
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
      
      effects <- function(chem_name) {
        chem_name %>% {case_when(
          . == "turbidity" ~ "bacterial growth",
          . == "arsenic" ~ "death")}}
      
      consumes <- function(chem_name, recent_result) {
        if (chem_name == "turbidity") return("")
        else
          glue("Currently, an average child at your school may consume as much as {1.5*recent_result} micrograms of {chem_name} in a school day.")
      }
      
      in_the_last_two_years <- function(chem_name, percent_samples_over_mcl) {
        if (chem_name == "lead") return("")
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
      "In light of these safety hazards, we urge you to consider providing bottled water for the most affected children at your school."
    
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

### Dear XY,
### We are More than a Meter (LLC) (Pending). We love children and kids.
### We have written a system to XYZ
### We have detected you have X levels of chemical X in your water supply, 
##### Z% higher than the maximal contaminent level set by the California Dept of Public Health 
#### We have also detected Z levels of chemical Y.
### An average child at your school may consume as much as ZZZ of chemical X and SSS of chemical Y  in a school day. This has risks XYZ, especially for children.
### Additionally, chemical Z has known carcinagenic effects, especially on children.
### We are telling you this because we believe you are in the right capacity to help minimize exposure and alert the right bodies of power to this issue.
### Some immediate actions you can take are:
#### - Give bottled water
#### - Alert PTA
#### - Alert your local Congressperson and/or Senator.
#### - If your budget allows for it, consider implementing a Z filter. This may help with these chemicals X.
##### Get to it, we wish you a wild and wonderous quest.
### Godspeed, More than a Meter LLC
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