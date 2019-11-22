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
        print(f); print(s)
        summary = select(temp, `Student Group`, contains("Score"))
        names(summary) = c("Student Group", "score")
        group_by(summary, `Student Group`) %>% 
          summarize(
            file = f,
            sheet = s,
            n = n(),
            n_na = sum(is.na(score)),
            n_10 = sum(score == "n<10", na.rm = T),
            n_ds = sum(score == "DS", na.rm = T)
          ) %>%
          bind_rows(summary, .)
      }
    } 
  }
}

# mutate_at(
#   summary,
#   vars(n_na:n_ds),
#   funs(100 * . / n)
# ) %>% 
#   View()
