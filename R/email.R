suppressPackageStartupMessages(library(gmailr))

use_secret_file('data/water-alarm-gmailr.json')

generate_content <- function(df) {
  generate_content_singleton <- function(analysis) {
    
  }
}

send_email <- function(to = "contact@arun.run", 
                       subject = "Hello world!", 
                       body = "Nothing to see here...") {
  mime(
    To = to,
    From = "arun.ramamurthy.md@gmail.com",
    Subject = subject,
    body = body) %>%
    send_message()
}

send_batch_email <- function(df) {
  df %>%
    generate_content() %>%
    walk(send_email)
}