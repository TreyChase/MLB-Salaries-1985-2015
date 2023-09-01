library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)

Teams <- read_csv("Downloads/Teams.csv")

unique(Teams$teamID)
Teams %>%
  select(yearID, teamID, name) %>%
  filter(yearID >= 1985) %>%
  arrange(yearID, teamID) %>%
  mutate(teamID = gsub("MON", "WAS", teamID)) %>%
  mutate(name = gsub("Montreal Expos", "Washington Nationals", name)) %>%
  mutate(teamID = gsub("CAL", "ANA", teamID)) %>%
  mutate(name = gsub("California Angels", "Anaheim Angels", name)) %>%
  distinct() %>%
  drop_na() %>%
 

#8/26/2023
#selected relevant columns, filtered to get teams with years after 1984. Then sorted to get year in ascending order, alphabetized by teamID.
#changed Montreal to Washington for continuity.
#changed California angels to Anaheim angels for continuity.
#got distinct values, with null values dropped.
#This data frame is now ready to be joined with the others.

