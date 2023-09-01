library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)

salaries_df <- read_csv("Downloads/Salaries.csv")
#shows salaries for players

View(salaries_df)
str(salaries_df)
colnames(salaries_df)
#data types are correct

ggplot(data=salaries_df) + geom_histogram(mapping=aes(x=salary)) + facet_wrap(~teamID)
#no outliers for salary.

cleaned_salary_df = salaries_df %>%
  select(yearID, teamID, playerID, salary) %>%
  mutate(teamID = gsub("MON", "WAS", teamID)) %>%
  mutate(teamID = gsub("CAL", "ANA", teamID)) %>%
  distinct() %>%
  drop_na() %>%
  arrange(yearID)
#8/24/2023
#selected appropriate columns, changed Montreal to Washington for continuity, dropped duplicates and null values, sorted by year.


unique(cleaned_salary_df$teamID)

ggplot(data=cleaned_salary_df) + geom_point(mapping=aes(x=teamID, y=salary, color=teamID))
  
  


