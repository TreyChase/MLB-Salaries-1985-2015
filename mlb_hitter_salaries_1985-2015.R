library(readr)
library(tidyverse)
library(dplyr)
master_df <- read_csv("Downloads/archive (1) 2/Master.csv")
#matches player ID to biographical info

all_star_df <- read_csv("Downloads/archive (1) 2/AllstarFull.csv")
#shows all-star appearances

salaries_df <- read_csv("Downloads/Salaries.csv")
#shows salaries for players

batting_df <- read_csv("Downloads/Batting.csv")
#batting stats

View(master_df)
master_df %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  filter(debut>='1985-01-01') %>% 
  rename(first_name = nameFirst) %>%
  rename(last_name = nameLast) %>%
  arrange(debut) 
#8/23/23 selecting needed columns, filtering to get players who debuted during or after 1985, renaming columns, and sorting by debut.

str(batting_df)
head(batting_df)
colnames(batting_df)

sorted_batting_df = batting_df %>%
  select(playerID, yearID, teamID, AB, R, H, '2B','3B', HR, RBI) %>%
  filter(yearID >= 1985) %>%
  distinct() %>%
  arrange(yearID) %>%
  drop_na() %>%
  group_by(playerID, yearID)
#8/24/2023 checked structure of batting_df. datatypes are good.
#then selected relevant columns, filtered to get seasons in 1985 to later, arranged by year, got disticint values, and dropped na values to get the hitters.
#We then grouped by year and player to get all the players who played for multiple teams in a season.


deadline_df = sorted_batting_df %>%
  filter(n()>=2) %>% 
  summarize(
    playerID = first(playerID),
    yearID = first(yearID),
    teamID = last(teamID),
    AB = sum(AB),
    R = sum(R),
    H = sum(H),
    `2B` = sum(`2B`),
    `3B`= sum(`3B`),
    HR = sum(HR),
    RBI = sum(RBI)
  ) %>%
  arrange(yearID) %>%
  filter(AB >= 150) %>%
  group_by(playerID, yearID)
  
#8/24/2023
#filtered sorted data for players who played for multiple teams in a year.
#added their stats from each team that year, then filtered for hitter with 150 or more AB.
#named for players who got traded at the deadline.


non_deadline_df = batting_df %>%
  select(playerID, yearID, teamID, AB, R, H, '2B','3B', HR, RBI) %>%
  filter(yearID >= 1985) %>%
  distinct() %>%
  arrange(yearID) %>%
  drop_na() %>%
  group_by(playerID, yearID) %>%
  filter(n()==1)

#doing the same process, but for players who played for one team during the season.


