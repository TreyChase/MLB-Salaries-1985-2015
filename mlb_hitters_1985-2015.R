library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

batting_df <- read_csv("Downloads/Batting.csv")
#batting stats


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
  filter(AB >= 100) %>%
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
  filter(n()==1 & AB>=100)

#doing the same process, but for players who played for one team during the season.

hitter_df = rbind(deadline_df, non_deadline_df)
#combined the two data frames to get a comprehensive list of the hitter data frame.

unique(hitter_df$teamID)
#one thing noticed here is that Washington and Montreal are the same team, as well as Anaheim and California .

cleaned_hitter_df = hitter_df %>%
  mutate(teamID=gsub("MON", "WAS", teamID)) %>%
  mutate(teamID = gsub("CAL", "ANA", teamID))
#changed all "MON" values to "WAS" for continuity.
#changed "CAL" to "ANA" for continuity.
#distinct values, one row per season per player, data types good, no significant outliers,
#no null values, no mis-typed numbers or strings, data has been aggregated.

str(hitter_df)
colnames(hitter_df)
#datatypes are still good.

cleaned_hitter_df = hitter_df %>%
  mutate(avg = round(H/AB, digits=3)) %>%
  mutate(slg = ((H-(`2B`+`3B`+ HR)) + 2*`2B`+ 3*`3B`+ 4*`HR`)/AB) 
#created slugging and batting average to create two more metrics to value hitters.


  
         