library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

master_df <- read_csv("Downloads/archive (1) 2/Master.csv")
#matches player ID to biographical info

View(master_df)
cleaned_master_df = master_df %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  filter(debut>='1985-01-01') %>%
  rename(first_name = nameFirst) %>%
  rename(last_name = nameLast) %>%
  arrange(debut) %>%
  select(playerID, first_name, last_name)
#8/23/23 selecting needed columns, filtering to get players who debuted during or after 1985, renaming columns, and sorting by debut.