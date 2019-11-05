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
star_uat_errors = mutate(star_uat_errors, Notes = ifelse(!is.na(Notes), Notes, X14)) %>% 
  select(-X14, -ends_with("_1")) %>% 
  arrange(`LEA ID`, `School ID`, `Framework (or Report Card)`, `Metric Name`, `Subgroup Name (Limited to STAR subgroups and All Report Card Students)`)

# Output errors (then add to Google sheet)
write_csv(
  star_uat_errors, 
  str_c("C:/Users/evan.kramer/Downloads/star_uat_errors (", now(), ").csv"), 
  na = ""
)

# Compare initial file with current errors
anti_join(
star_uat_errors,
  gs_read(gs_title("STAR Application UAT"), ws = "Errors") %>% 
    mutate_all("as.character"),
  by = names(star_uat_errors)
)

# Add to Errors sheet -- takes forever
# gs_edit_cells(
#   gs_title("STAR Application UAT"),
#   ws = "Errors",
#   input = star_uat_errors,
#   trim = T
# )

# Alternative way to add an Errors sheet -- also takes forever
if("Errors" %in% gs_ws_ls(gs_title("STAR Application UAT"))) {
  gs_ws_delete(gs_title("STAR Application UAT"), ws = "Errors")
}
gs_ws_new(
  gs_title("STAR Application UAT"),
  ws_title = "Errors",
  row_extent = nrow(star_uat_errors),
  col_extent = ncol(star_uat_errors),

  gs_edit_cells(
    ss = gs_title("STAR Application UAT"),
    ws = "Errors",
    input = group_by(star_uat_errors, Notes) %>% 
      summarize_all("first") %>% 
      ungroup()
    # input = head(star_uat_errors, 200)
  )
)

