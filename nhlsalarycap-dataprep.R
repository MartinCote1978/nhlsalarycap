# NHL Salary Cap - Data Preparation

library(dplyr)
library(tidyr)

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
  nhl_salaries <- nhl_salaries %>% union(readTeamSrcData(teams[i]))
}

#View(nhl_salaries)
nhl_salaries

## Clean/Tidy up
### 1. Separate contract terms
### 2. switch all dollars amount to actual numbers type
### 3. switch the expires to Date-Year type
### 4. Switch years (2014, 2015, 2016, 2017, 2018) variable as one variable ('problem 3')
### 5. Add the years after 2018 indicated by contract expiration date-year, when applicable.
### 6. Add the years before 2014 indicated by contract length, when applicable.


# Q: What is the average salary for 2014?
