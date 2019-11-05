# STAR Application QA
# Evan Kramer

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(googlesheets)
setwd("C:/Users/evan.kramer/Downloads")

# Get the number and names of all tabs in the sheet
n_ws = gs_title("STAR Application UAT") %>% 
  .['n_ws'] %>% 
  as.numeric() 

names_ws = gs_read(gs_title("STAR Application UAT"), ws = "UAT") %>% 
  select(`Primary UAT Analyst`) %>% 
  filter(!is.na(`Primary UAT Analyst`)) %>%
  distinct() 

# Load each sheet and bind rows
star_uat_errors = tibble()
for(ws in sort(unique(names_ws$`Primary UAT Analyst`))) {
  star_uat_errors = (gs_read(gs_title("STAR Application UAT"), ws = ws)) %>% 
    mutate_all("as.character") %>% 
    filter(`Primary UAT Sign-off` == "Error Found") %>% 
    bind_rows(., star_uat_errors)
  Sys.sleep(6) # make the machine wait so it doesn't get throttled by the API for excessive requests
}






#   names() %>% 
#   .['ws']
#   # gs_read() %>% 
#   View()
# # gs_auth(new_user = T)
# # n_ws
