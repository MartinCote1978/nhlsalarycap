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
#View(nhl_2014_avgsalaries)
nhl_2014_avgsalaries %>%
  write.csv(file="nhl_2014_avgsalaries.csv", row.names=FALSE)


# Q: What is the amount of money currently under contracts for each team in 2015?
nhl_2015_teamsalaries <- nhl_salaries_tidy %>%
  group_by(TEAM, SEASON) %>%
  summarise(TOTAL_SEASON = sum(AVGSALARY, na.rm=TRUE)) %>%
  filter(SEASON == 2015) %>%
  ungroup() %>%
  arrange(desc(TOTAL_SEASON))
View(nhl_2015_teamsalaries)

# Q: What is the average committed amount for salary for each season, by team?
nhl_2014_totsalarycommitted <- nhl_salaries_tidy %>%
  group_by(TEAM, SEASON) %>%
  summarise(TOTALSALARYCOMMITTED = sum(AMTPERYEAR_INT, na.rm=TRUE))
#View(nhl_2014_totsalarycommitted)
nhl_2014_totsalarycommitted %>%
  write.csv(file="nhl_2014_totsalarycommitted.csv", row.names=FALSE)
## Used Excel to format and bring the seasons as column headers.

# Q: How much money was saved versus invested by the seller/buyer team?


# Q: What is the average amount of salary committed by position
#    (group 1=(RW, C, LW), group 2=(D), group 3=(G)) as well as for how many
#    players by team, by year?
nhl_totsalarycommittedbyposition <- nhl_salaries_tidy %>%
  mutate(POS_GROUP = ifelse(POS %in% c("RW", "LW", "C", "D"), ifelse(POS %in% c("RW", "LW", "C"), "F", "D"), "G" )) %>%
  group_by(TEAM, POS_GROUP, SEASON) %>%
  mutate(PLAYERCOUNT = count(PLAYER) ) %>%
  summarise(TOTALSALARYCOMMITTEDBYPOS = sum(AMTPERYEAR_INT, na.rm=TRUE))
nhl_totsalarycommittedbyposition %>%
  write.csv(file="nhl_totsalarycommittedbyposition.csv", row.names=FALSE)

