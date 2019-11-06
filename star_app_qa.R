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
n_ws = gs_title("Report Card/STAR Application UAT") %>% 
  .['n_ws'] %>% 
  as.numeric() 

names_ws = gs_read(gs_title("Report Card/STAR Application UAT"), ws = "UAT") %>% 
  select(`Primary UAT Analyst`) %>% 
  filter(!is.na(`Primary UAT Analyst`)) %>%
  distinct() 

# Load each sheet and bind rows
star_uat_errors = tibble()
for(ws in sort(unique(names_ws$`Primary UAT Analyst`))) {
  star_uat_errors = gs_read(
    gs_title("Report Card/STAR Application UAT"), 
    ws = ws,
    cell_range = "A:M",
    col_types = str_flatten(rep("c", 14))
  ) %>% 
    mutate_all("as.character") %>% 
    filter(`Primary UAT Sign-off` == "Error Found") %>% 
    bind_rows(., star_uat_errors)
  Sys.sleep(6) # make the machine wait so it doesn't get throttled by the API for excessive requests
}

# Add and combine variables
errors_sheet = mutate(star_uat_errors, Notes = ifelse(!is.na(Notes), Notes, X14)) %>% 
  select(-X14, -ends_with("_1")) %>% 
  # Filter out flagged errors that appear to be rounding issues?
  # This method is more aggressive in flagging issues as rounding errors -- subsequently changed to less aggressive
  filter(!str_detect(str_to_lower(Notes), "round") & !str_detect(Notes, "2019")) %>%  
  arrange(`School Name`, `Framework (or Report Card)`, `Metric Name`, `Subgroup Name (Limited to STAR subgroups and All Report Card Students)`) %>% 
  # Join to existing Errors sheet (which contains categories of errors based on review)
  left_join(
    gs_read(gs_title("Report Card/STAR Application UAT"), ws = "Errors") %>% 
      group_by(`Metric Name`, Notes) %>% 
      summarize(`EK Notes` = first(`EK Notes`)) %>% 
      ungroup(),
    by = c("Metric Name", "Notes")
  ) %>% 
  # Filter out any issues that have been resolved (i.e., have the same "Notes" as an issue that has been reviewed and marked "Resolved")
  filter(!`EK Notes` %in% c("Zero-rate suppression", "Resolved"))

# Output errors (then add to Google sheet)
write_csv(
  errors_sheet, 
  str_c(
    "C:/Users/evan.kramer/Downloads/star_uat_errors (", 
    str_replace_all(now(), "[:-]", ""), 
    ").csv"
  ), 
  na = ""
)

# Extract errors from sheet before overwriting, just in case
Sys.sleep(6)
write_csv(
  gs_read(gs_title("Report Card/STAR Application UAT"), ws = "Errors"),
  str_c(
    "C:/Users/evan.kramer/Downloads/star_uat_errors_from_sheet (", 
    str_replace_all(now(), "[:-]", ""),
    ").csv"
  ), 
  na = ""
)

# Add additional rows to bottom of Errors sheet
# Sys.sleep(6)
# gs_edit_cells(
#   ss = gs_title("Report Card/STAR Application UAT"),
#   ws = "Errors",
#   input = filter(errors_sheet, is.na(`EK Notes`)) %>% 
#     group_by(Notes) %>% 
#     summarize_all(first) %>% 
#     ungroup(),
#   anchor = str_c("A", gs_read(gs_title("Report Card/STAR Application UAT"), ws = "Errors") %>% nrow() + 1),
#   col_names = F,
# )

# Summary? 
group_by(errors_sheet, `EK Notes`) %>% 
  summarize_all(n_distinct) %>% 
  ungroup() %>% 
  rename_all(funs(str_c("# Distinct ", .))) %>% 
  filter(!is.na(`# Distinct EK Notes`))

# Create Error Review sheet
arrange(errors_sheet, `LEA ID`, `School Name`, `Framework (or Report Card)`, `Metric Name`, `Subgroup Name (Limited to STAR subgroups and All Report Card Students)`) %>% 
  rename(Category = `EK Notes`) %>% 
  mutate(
    Reviewer = case_when(
      `LEA ID` == "1" & `School ID` %in% c("318", "442", "450") ~ sort(names_ws$`Primary UAT Analyst`)[1],
      `LEA ID` == "1" & `School ID` %in% c("454") ~ sort(names_ws$`Primary UAT Analyst`)[3],
      `LEA ID` == "1" & `School ID` %in% c("884") ~ sort(names_ws$`Primary UAT Analyst`)[4],
      `LEA ID` == "1" & `School ID` %in% c("LEA") & (str_detect(`Metric Name`, "ELA") | str_detect(`Metric Name`, "Math")) ~ sort(names_ws$`Primary UAT Analyst`)[5],
      `LEA ID` == "1" & `School ID` %in% c("LEA") & !(str_detect(`Metric Name`, "ELA") | str_detect(`Metric Name`, "Math")) ~ sort(names_ws$`Primary UAT Analyst`)[11],
      `LEA ID` == "119" & `School ID` %in% c("126") ~ sort(names_ws$`Primary UAT Analyst`)[6],
      `LEA ID` == "120" & `School ID` %in% c("LEA") ~ sort(names_ws$`Primary UAT Analyst`)[7],
      `LEA ID` %in% c("129") & `School ID` %in% c("LEA", "209") ~ sort(names_ws$`Primary UAT Analyst`)[7],
      `LEA ID` %in% c("133", "143", "145", "168")  ~ sort(names_ws$`Primary UAT Analyst`)[8],
      `LEA ID` == "178" ~ sort(names_ws$`Primary UAT Analyst`)[9],
      `LEA ID` == "186" & `School ID` %in% c("267") ~ sort(names_ws$`Primary UAT Analyst`)[10],
      `LEA ID` == "188" ~ sort(names_ws$`Primary UAT Analyst`)[11],
      T ~ "Evan"
    ),
    `Error Type` = NA # Data validation options will be Existing, New
  ) %>% 
  arrange(Reviewer) %>% 
  write_csv(
    str_c(
      "C:/Users/evan.kramer/Downloads/star_uat_error_review (", 
      str_replace_all(now(), "[:-]", ""),
      ").csv"
    ), 
    na = ""
  )





# Alternative way to add an Errors sheet -- also takes forever
# And how to make sure we don't overwrite existing data
# if("Errors (All)" %in% gs_ws_ls(gs_title("STAR Application UAT"))) {
#   gs_ws_delete(gs_title("Report Card/STAR Application UAT"), ws = "Errors (All)")
# }
# gs_ws_new(
#   gs_title("Report Card/STAR Application UAT"),
#   ws_title = "Errors (All)",
#   row_extent = group_by(star_uat_errors, Notes) %>%
#     summarize_all("first") %>%
#     ungroup() %>%
#     nrow(),
#   col_extent = ncol(star_uat_errors) + 1,
#   gs_edit_cells(
#     ss = gs_title("Report Card/STAR Application UAT"),
#     ws = "Errors",
#     input = group_by(star_uat_errors, Notes) %>%
#       summarize_all("first") %>%
#       ungroup() %>%
#       mutate(`EK Notes` = NA)
#     # input = head(star_uat_errors, 200)
#   )
# )


# Assign each one to one of the reviewers
# Data validate (known/unknown)



# Compare initial file with current errors
# anti_join(
#   star_uat_errors,
#   gs_read(gs_title("Report Card/STAR Application UAT"), ws = "Errors") %>% 
#     mutate_all("as.character") %>% 
#     select(-`EK Notes`),
#   by = names(star_uat_errors)
# )

# Add to Errors sheet -- takes forever
# gs_edit_cells(
#   gs_title("STAR Application UAT"),
#   ws = "Errors",
#   input = star_uat_errors,
#   trim = T
# )

