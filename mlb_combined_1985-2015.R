library(ggplot2)
library(tidyverse)
install.packages("gridExtra")
library(gridExtra)

combined_df_I = merge(salaries_df, cleaned_hitter_df, by=c("playerID","yearID")) %>%
  select(-teamID.y) %>%
  rename(teamID = teamID.x) %>%
  arrange(yearID, playerID) %>%
  mutate(slg=round(slg, 3)) %>%
  mutate(iso = slg-avg) %>%
  mutate(tb = ((H-(`2B`+`3B`+ HR)) + 2*`2B`+ 3*`3B`+ 4*`HR`)) %>%
  mutate(teamID = gsub("MON", "WAS", teamID)) %>%
  mutate(teamID = gsub("CAL", "ANA", teamID)) %>%
  distinct() %>%
  drop_na()

#8/27/2023
#joined the cleaned and sorted salary and batting data frames.
#removed duplicate column name, arranged by year and player, rounded slugging to 3 decimal places.
#added 'iso' or isolated power as another metric to measure.
#had to change teamID names again for contiuity.
#got distinct and non null entries.

str(combined_df_I)
colnames(combined_df_I)
head(combined_df_I)

#checking data types and column names.

salary_by_year = combined_df_I %>%
  group_by(yearID) %>%
  summarize(mean(salary), sd(salary), max(salary), min(salary), sum(salary))
  

#summary of average salary by year for batters using mean, sd, max, min, and total.

salary_by_team = combined_df_I %>%
  group_by(teamID, yearID) %>%
  summarize(mean(salary), sd(salary), max(salary), min(salary), sum(salary))

giants_salary = combined_df_I %>%
  group_by(teamID, yearID) %>%
  filter(teamID=="SFN") %>%
  summarize(mean(salary), sd(salary), max(salary), min(salary), sum(salary))

#same summary of salary for just the San Francisco Giants.

avg_salary_p = ggplot(data=salary_by_year) + geom_line(mapping=aes(x=yearID, y=`mean(salary)`), color="blue") + geom_point(mapping=aes(x=yearID, y=`mean(salary)`), color="blue") + labs(title = "Average MLB Hitter Salary", subtitle="1985-2015", x= "Year", y= "Average Salary")  
#visualization for average salary by year for mlb hitters.

total_salary_p = ggplot(data=salary_by_year) + geom_line(mapping=aes(x=yearID, y=`sum(salary)`)) + geom_point(mapping=aes(x=yearID, y=`sum(salary)`)) + labs(title = "Total MLB Hitter Salary", subtitle="1985-2015", x= "Year", y= "Average Salary")
#visualization for total salary by year for mlb hitters.

giants_avg_salary_p = ggplot(data=giants_salary) + geom_line(mapping=aes(x=yearID, y=`mean(salary)`), color="orange") + geom_point(mapping=aes(x=yearID, y=`mean(salary)`), color="orange") + labs(title = "Average MLB Hitter Salary", subtitle="San Francisco Giants: 1985-2015", x= "Year", y= "Average Salary")
#visualization 
#significant drop in 2012, with a sharp spike back up.

giants_total_salary_p = ggplot(data=giants_salary) + geom_line(mapping=aes(x=yearID, y=`sum(salary)`), color="orange") + geom_point(mapping=aes(x=yearID, y=`sum(salary)`), color="orange") + labs(title="Total MLB Hitter Salary", subtitle="San Francisco Giants", x="Year", y="Salary")
#visualization of total salary for the San Francisco Giants.

legend_colors = c("MLB"="blue", "San Francisco Giants"="orange")

combined_pI = ggplot() +
  geom_smooth(data = salary_by_team, mapping = aes(yearID, `mean(salary)`, color = "MLB")) +
  geom_line(data = giants_salary, aes(yearID, `mean(salary)`, color = "San Francisco Giants")) +
  geom_point(data = giants_salary, aes(yearID, `mean(salary)`, color = "San Francisco Giants")) +
  labs(title = "MLB Hitter Average Salary", subtitle = "MLB vs. San Francisco Giants: 1985-2015", x = "Year", y = "Salary") +
  scale_color_manual(values = legend_colors, guide = guide_legend(title = "Teams"))

#constructed a plot overlapping MLB average salary and SFG average salary per year.
combined_pII = ggplot() +
  geom_smooth(data = salary_by_team, mapping = aes(yearID, `sum(salary)`, color = "MLB")) +
  geom_line(data = giants_salary, aes(yearID, `sum(salary)`, color = "San Francisco Giants")) +
  geom_point(data = giants_salary, aes(yearID, `sum(salary)`, color = "San Francisco Giants")) +
  labs(title = "MLB Hitter Total Salary", subtitle = "MLB vs. San Francisco Giants: 1985-2015", x = "Year", y = "Salary") +
  scale_color_manual(values = legend_colors, guide = guide_legend(title = "Teams"))

#constructed a plot overlapping MLB total salary and SFG total salary per year.

cor_matrix = cor(combined_df_I %>%
  select(-playerID, -teamID, -lgID))
#constructing a correlation matrix to enter into a heatmap function.

library(RColorBrewer)
cyanpurple = c("cyan", "purple")
salary_heatmap = heatmap(cor_matrix, col=colorRampPalette(cyanpurple)(100), main="MLB Hitter Correlation Heatmap: 1985-2015")
#constructing a heatmap to find correlation between all variables.

hitting_by_team = combined_df_I %>%
  group_by(teamID, yearID) %>%
  summarize(mean_avg = mean(avg), mean_slg = mean(slg), mean_hr = mean(HR), total_hr = sum(HR), avg_iso=mean(iso), total_bases = sum(tb), total_runs=sum(R))
#8/28/2023
#summarizing important hitting metrics by year and team.

giants_hitting = combined_df_I %>%
  group_by(teamID, yearID) %>%
  filter(teamID == "SFN") %>%
  summarize(mean_avg = mean(avg), mean_slg = mean(slg), mean_hr = mean(HR), total_hr = sum(HR), avg_iso=mean(iso), total_bases = sum(tb), total_runs=sum(R))
#doing the same but filtering just for the Giants.

avg_p = ggplot() +
  geom_smooth(data = hitting_by_team, mapping = aes(yearID, mean_avg, color = "MLB")) +
  geom_line(data = giants_hitting, aes(yearID, mean_avg, color = "San Francisco Giants")) +
  geom_point(data = giants_hitting, aes(yearID, mean_avg, color = "San Francisco Giants")) +
  labs(title = "MLB Hitter Batting Average Per Year", subtitle = "MLB vs. San Francisco Giants: 1985-2015", x = "Year", y = "Batting Average") +
  scale_color_manual(values = legend_colors, guide = guide_legend(title = "Teams"))
#batting average plot to compare SFG to MLB per year.

slugging_p = ggplot() +
  geom_smooth(data = hitting_by_team, mapping = aes(yearID, mean_slg, color = "MLB")) +
  geom_line(data = giants_hitting, aes(yearID, mean_slg, color = "San Francisco Giants")) +
  geom_point(data = giants_hitting, aes(yearID, mean_slg, color = "San Francisco Giants")) +
  labs(title = "MLB Hitter Average Slugging Percentage Per Year", subtitle = "MLB vs. San Francisco Giants: 1985-2015", x = "Year", y = "Slugging %") +
  scale_color_manual(values = legend_colors, guide = guide_legend(title = "Teams"))
#slugging plot comparing mlb avg slugging to giants avg slugging per year.

hr_p = ggplot() +
  geom_smooth(data = hitting_by_team, mapping = aes(yearID, total_hr, color = "MLB")) +
  geom_line(data = giants_hitting, aes(yearID, total_hr, color = "San Francisco Giants")) +
  geom_point(data = giants_hitting, aes(yearID, total_hr, color = "San Francisco Giants")) +
  labs(title = "MLB Hitter Total HR Per Year", subtitle = "MLB vs. San Francisco Giants: 1985-2015", x = "Year", y = "Home Runs") +
  scale_color_manual(values = legend_colors, guide = guide_legend(title = "Teams"))
#home run plot comparing mlb total hr to giants total hr per year.


total_bases_p = ggplot() +
  geom_smooth(data = hitting_by_team, mapping = aes(yearID, total_bases, color = "MLB")) +
  geom_line(data = giants_hitting, aes(yearID, total_bases, color = "San Francisco Giants")) +
  geom_point(data = giants_hitting, aes(yearID, total_bases, color = "San Francisco Giants")) +
  labs(title = "MLB Hitter Total Bases Per Year", subtitle = "MLB vs. San Francisco Giants: 1985-2015", x = "Year", y = "TB") +
  scale_color_manual(values = legend_colors, guide = guide_legend(title = "Teams"))
#total bases plot for comparing mlb to sfg per year.


runs_p = ggplot() +
  geom_smooth(data = hitting_by_team, mapping = aes(yearID, total_runs, color = "MLB")) +
  geom_line(data = giants_hitting, aes(yearID, total_runs, color = "San Francisco Giants")) +
  geom_point(data = giants_hitting, aes(yearID, total_runs, color = "San Francisco Giants")) +
  labs(title = "MLB Hitter Runs Per Season", subtitle = "MLB vs. San Francisco Giants: 1985-2015", x = "Year", y = "Runs") +
  scale_color_manual(values = legend_colors, guide = guide_legend(title = "Teams"))
#total runs plot comparing mlb to sfg per year.

iso_p = ggplot() +
  geom_smooth(data = hitting_by_team, mapping = aes(yearID, avg_iso, color = "MLB")) +
  geom_line(data = giants_hitting, aes(yearID, avg_iso, color = "San Francisco Giants")) +
  geom_point(data = giants_hitting, aes(yearID, avg_iso, color = "San Francisco Giants")) +
  labs(title = "MLB Hitter Isolated Power Per Year", subtitle = "MLB vs. San Francisco Giants: 1985-2015", x = "Year", y = "Isolated Power %") +
  scale_color_manual(values = legend_colors, guide = guide_legend(title = "Teams"))
#isolated power plot comparing MLB to SFG per year.

hitting_grid_p = grid.arrange(avg_p, runs_p, total_bases_p, hr_p, slugging_p, iso_p, ncol=2, widths=c(4,4), heights=c(4,4,4))
#making a grid to plot all hitting plots in one graphic, and easy for presentation.

# we will use .420 slugging, 100 total bases (1067/13), .420-.255,  
league_averages = combined_df_I %>%
  mutate(iso = slg-avg) %>%
  mutate(tb = ((H-(`2B`+`3B`+ HR)) + 2*`2B`+ 3*`3B`+ 4*`HR`)) %>%
  group_by(yearID) %>%
  filter(yearID >=2012) %>%
  summarize(
    mean_avg = round(mean(avg),3),
    mean_slg = round(mean(slg),3),
    mean_iso = round(mean(iso),3),
    mean_runs = round(mean(R),0),
    mean_tb = round(mean(tb), 0),
    mean_salary = round(mean(salary), 2)
  ) 
#constructing mean averages of hitting statistics from 2012-2015, in order to find hitters.

league_averages %>%
  gt() %>%
  tab_header(
    title = "League Hitting Averages", subtitle="2012-2015"
  ) %>%
  cols_label(
    yearID = "Year",
    mean_avg = "Mean Avg",
    mean_slg = "Mean SLG",
    mean_iso = "Mean ISO",
    mean_runs = "Mean Runs",
    mean_tb = "Mean TB",
    mean_salary = "Mean Salary"
  ) 
  
#constructed a table to visualize the league averages.

desired_hitters = combined_df_I %>%
  filter(yearID >= 2012 & avg >=0.253 & slg>=0.403 & iso >=0.149, R >= 49, tb >= 160, salary < 5335274) 
#8/29/2023
#finding hitters that have above average hitting metrics with a below average salary.
  
list_of_hitters = merge(desired_hitters, cleaned_master_df, by="playerID") %>%
  select(first_name, last_name) %>%
  distinct() %>%
  arrange(first_name)
#making a list of all hitter that the Giants should look at.
  

