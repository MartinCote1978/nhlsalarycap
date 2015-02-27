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
                                    stringsAsFactors=FALSE,
                                    fileEncoding="latin1")
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
### 3. Add the years after 2018 indicated by contract expiration date-year, when applicable.
  mutate(X2019 = ifelse(EXPIRES >= 2019, AVG..SALARY, "-")) %>%
  mutate(X2020 = ifelse(EXPIRES >= 2020, AVG..SALARY, "-")) %>%
  mutate(X2021 = ifelse(EXPIRES >= 2021, AVG..SALARY, "-")) %>%
  mutate(X2022 = ifelse(EXPIRES >= 2022, AVG..SALARY, "-")) %>%
  mutate(X2023 = ifelse(EXPIRES >= 2023, AVG..SALARY, "-")) %>%
  mutate(X2024 = ifelse(EXPIRES >= 2024, AVG..SALARY, "-")) %>%
  mutate(X2025 = ifelse(EXPIRES >= 2025, AVG..SALARY, "-")) %>%
  mutate(X2026 = ifelse(EXPIRES >= 2026, AVG..SALARY, "-")) %>%
  select(PLAYER, POS..x, X2014, X2015, X2016, X2017, X2018, X2019, X2020, X2021, X2022, X2023, X2024, X2025, X2026, X, POS..y, AGE, EXP., CONTRACTLENGTH, CONTRACTAMOUNT, AVG..SALARY, EXPIRES, TEAM) %>%
### 4. Switch years (2014, 2015, 2016, 2017, 2018) variable as one variable ('common problem 1')
  gather(SEASON, AMTPERYEAR, X2014:X2026, na.rm=FALSE) %>%
  mutate(SEASON = extract_numeric(SEASON)) %>%
### 5. Extract contract type and status at the end of the contract
  mutate(PLAYERSTATUS_TMP = ifelse(AMTPERYEAR == "UFA" | AMTPERYEAR == "RFA", AMTPERYEAR, NA) ) %>%
  group_by(PLAYER) %>%
  mutate(PLAYERSTATUS = PLAYERSTATUS_TMP[!is.na(PLAYERSTATUS_TMP)][1] ) %>%
  ungroup() %>%
  mutate(AMTPERYEAR_INT = extract_numeric(AMTPERYEAR)) %>%
### 6. switch all dollars amount to actual numbers type
  mutate(AVGSALARY = extract_numeric(AVG..SALARY)) %>%
### 8. Add the years before 2014 indicated by contract length, when applicable.
### necessary??  For visual display (i.e. on web), no.  For statistics & analytics, yes.  However, useless to have
### only at best a few years back before 2014.  Need to find another data source to obtain salaries prior to
### 2014 season.
### 7. Table Cleanup: Remove the duplicate column 'POS..y' and no longer necessary AVG..SALARY (replaced by step 4)
  select(-c(POS..y, AVG..SALARY, PLAYERSTATUS_TMP)) %>%
  rename(POS = POS..x) %>%
  filter(!is.na(AMTPERYEAR_INT))

### Export the data to a file for future use (i.e. analytics)
nhl_salaries_tidy %>%
  write.csv(file="nhl_salaries_tidy.csv", row.names=FALSE)
