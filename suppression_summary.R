# Suppression Summary
# Evan Kramer

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(readxl)
ver = sprintf("%02d", 2)
setwd(str_c("X:/Accountability/School Year 2018-19/06 Analysis/Aggregations and Reporting/Public File/", ver, "_version", ver, "/Output"))

# Data
summary = tibble()
for(f in list.files()) {
  if(str_detect(f, ".xlsx")) {
    # for(s in excel_sheets(str_c(year(now()), " DC School Report Card Aggregate Public Data.xlsx"))) {
    for(s in excel_sheets(f)) {
      temp = read_excel(f, sheet = s)
      if(max(str_detect(names(temp), "Student Group")) & max(str_detect(names(temp), "Score"))) {
        # print(s)
        summary = select(temp, `Student Group`, contains("Score")) %>% 
          group_by(`Student Group`) %>% 
          summarize(
            file = f,
            sheet = s,
            n = n(),
            n_na = sum(is.na(.[2])),
            n_10 = sum(.[2] == "n<10", na.rm = T),
            n_ds = sum(.[2] == "DS", na.rm = T)
          ) %>% 
          bind_rows(summary, .)
      }
    } 
  }
}

mutate_at(
  summary,
  vars(n_na:n_ds),
  funs(100 * . / n)
)
