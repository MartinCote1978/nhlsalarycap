# NHL Salary Cap - Data Preparation

library(dplyr)
library(tidyr)
library(lubridate)

## Constants
data_folder <- "data-"
prefix <- "nhlsalarycap-"
suffix_contracts <- "-contracts-"
suffix_peryear <- "-peryear-"
data_date <- "2015-02-23"

teams <- c("anaheim", "arizona", "boston", "buffalo", "calgary", "carolina", "chicago", "colorado",
           "columbus", "dallas", "detroit", "edmonton", "florida", "losangeles", "minnesota", "montreal",
           "nashville", "newjersey", "nyislanders", "nyrangers", "ottawa", "philadelphia", "pittsburgh",
           "sanjose", "stlouis", "tampabay", "toronto", "vancouver", "washington", "winnipeg")

readTeamSrcData <- function(team_name) {
  team_peryear <- tbl_df(read.csv(file=paste("./",
                                             data_folder,
                                             data_date,
                                             "/",
                                             prefix,
                                             team_name,
                                             suffix_peryear,
                                             data_date,
                                             ".csv",
                                             sep=""),
                                  header=TRUE,
                                  colClasses=c("character", # PLAYER
                                               "character", # POS
                                               "character", # 2014
                                               "character", # 2015
                                               "character", # 2016
                                               "character", # 2017
                                               "character", # 2018
                                               "character"), # X
                                  stringsAsFactors=FALSE)
  )
  team_contracts <- tbl_df(read.csv(file=paste("./",
                                               data_folder,
                                               data_date,
                                               "/",
                                               prefix,
                                               team_name,
                                               suffix_contracts,
                                               data_date,
                                               ".csv",
                                               sep=""),
                                    header=TRUE,
                                    colClasses=c("character", # PLAYER
                                                 "character", # POS
                                                 "integer", # AGE
                                                 "integer", # EXP.
                                                 "character", # CONTRACT TERMS
                                                 "character", # AVG. SALARY
                                                 "character"), # EXPIRES
                                    stringsAsFactors=FALSE)
                           )
  team_salaries <- team_peryear %>%
    left_join(team_contracts, by="PLAYER") %>%
    mutate(TEAM = team_name)
  team_salaries
}

nhl_salaries <- readTeamSrcData(teams[1])
for(i in 2:length(teams)) {
  nhl_salaries <- nhl_salaries %>% bind_rows(readTeamSrcData(teams[i]))
}

## Pre-Conditions steps:
### 1. To separate contract.terms, no NA is allowed.  For those, insert value "0 yr$0".
nhl_salaries[is.na(nhl_salaries$CONTRACT.TERMS), ]$CONTRACT.TERMS <- "0 yr$0"
### 2. Replace ' yr$' with '__' to use the dplyr:separate functions later
nhl_salaries$CONTRACT.TERMS <- sub(" yr\\$", "__", nhl_salaries$CONTRACT.TERMS)

## Clean/Tidy up
nhl_salaries_tidy <- nhl_salaries %>%
### 1. Separate contract terms  
  separate(col=CONTRACT.TERMS, into=c("CONTRACTLENGTH", "CONTRACTAMOUNT"), sep="__") %>%
  mutate(CONTRACTLENGTH = as.numeric(CONTRACTLENGTH)) %>%
  mutate(CONTRACTAMOUNT = extract_numeric(CONTRACTAMOUNT)) %>%
### 2. switch the expires to numeric type
  mutate(EXPIRES = as.numeric(EXPIRES)) %>%
### 3. Switch years (2014, 2015, 2016, 2017, 2018) variable as one variable ('problem 1')
  gather(SEASON, AMTPERYEAR, X2014:X2018, na.rm=FALSE) %>%
  mutate(SEASON = extract_numeric(SEASON)) %>%
### 4. Extract contract type and status at the end of the contract
  # necessary??
  #mutate(AMTPERYEAR = extract_numeric(AMTPERYEAR)) %>%
### 5. switch all dollars amount to actual numbers type
  mutate(AVGSALARY = extract_numeric(AVG..SALARY)) %>%
### 6. Add the years after 2018 indicated by contract expiration date-year, when applicable.
  # necessary??
### 7. Add the years before 2014 indicated by contract length, when applicable.
  # necessary??
  select(-c(POS..y, AVG..SALARY))


# Q: What is the average salary for 2014, by team?
nhl_2014_avgsalaries <- nhl_salaries_tidy %>%
  group_by(TEAM, SEASON) %>%
  summarise(AVGSALARYSEASON = mean(AVGSALARY, na.rm=TRUE)) %>%
  filter(SEASON == 2014) %>%
  ungroup() %>%
  arrange(desc(AVGSALARYSEASON))
View(nhl_2014_avgsalaries)
