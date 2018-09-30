suppressPackageStartupMessages(library(gmailr))

use_secret_file('data/water-alarm-gmailr.json')

generate_content <- function(df) {
  generate_content_singleton <- function(analysis) {
    
  }
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
                       subject = "Hello world!", 
                       body = "Nothing to see here...") {
  mime(To = to,
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