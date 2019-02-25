library(readr)
library(dplyr)
library(ggplot2)
suicide_df <- read_csv("C:/Users/Richard/AppData/Local/GitHubDesktop/app-1.4.2/Statistics-Meeting/WHO_new_suicide_statistics.csv")
suicide_df <- suicide_df %>% mutate(suicide_pct_pop = suicides_no/population)

suicide_us_df <- suicide_df %>% filter(country=='United States')
# create data frame that shows female suicides as a percentage of the population
# by year since 1990 in the United States
suicide_us_fem_since1990_df <-  suicide_us_df %>% 
  filter(sex == 'female', year > 1989) %>%
  select(year, sex, population, suicide_pct_pop)%>% 
  group_by(year) %>% 
  summarise(total_pop = sum(population)/1000000,total_pct_pop = sum(suicide_pct_pop))  

ggplot(suicide_us_fem_since1990_df,aes(x=year,y=total_pct_pop))+geom_point()+
  ggtitle("Number of Female Suicides (in the U.S.) \n per Percent of Population by Year Since 1990")+ylab("Suicides as a Percentage of Population")+
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="blue", size=12, face="bold")
  )
ggplot(suicide_us_fem_since1990_df,aes(x=year,y=total_pop))+geom_point()+
  ggtitle("Female Population (in the U.S.) by Year Since 1990")+ylab("Total Population in Millions")+
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="blue", size=12, face="bold")
  )

suicide_gb_df <- suicide_df %>% filter(country=='United Kingdom')
# create data frame that shows female suicides as a percentage of the population
# by year since 1990 in Great Britain
suicide_gb_fem_since1990_df <-  suicide_gb_df %>% 
  filter(sex == 'female', year > 1989) %>%
  select(year, sex, population, suicide_pct_pop)%>% 
  group_by(year) %>% 
  summarise(total_pop = sum(population)/1000000,total_pct_pop = sum(suicide_pct_pop))  

ggplot(suicide_gb_fem_since1990_df,aes(x=year,y=total_pct_pop))+geom_point()+
  ggtitle("Number of Female Suicides (in Great Britain) \n per Percent of Population by Year Since 1990")+ylab("Suicides as a Percentage of Population")+
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="blue", size=12, face="bold")
  )
ggplot(suicide_gb_fem_since1990_df,aes(x=year,y=total_pop))+geom_point()+
  ggtitle("Female Population (in Great Britain) by Year Since 1990")+ylab("Total Population in Millions")+
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="blue", size=12, face="bold")
  )


