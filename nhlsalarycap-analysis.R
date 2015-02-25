library(dplyr)
library(tidyr)
library(lubridate)

nhl_salaries_tidy <- tbl_df(read.csv(file="nhl_salaries_tidy.csv", header=TRUE))

# Q: What is the average salary for 2014, by team?
nhl_2014_avgsalaries <- nhl_salaries_tidy %>%
  group_by(TEAM, SEASON) %>%
  summarise(AVGSALARYSEASON = mean(AVGSALARY, na.rm=TRUE)) %>%
  filter(SEASON == 2014) %>%
  ungroup() %>%
  arrange(desc(AVGSALARYSEASON))
View(nhl_2014_avgsalaries)
