library(readr)
library(dplyr)
library(ggplot2)
setwd("C:/Users/Richard/AppData/Local/GitHubDesktop/app-1.4.2/Statistics-Meeting")
suicide_df <- read_csv("WHO_new_suicide_statistics.csv")
names(suicide_df)[7] <- "suicides_per_100K"
suicide_df <- suicide_df %>% mutate(suicide_pct_pop = suicides_no/population)

suicide_us_df <- suicide_df %>% filter(country=='United States')

# create data frame that shows female suicides as a percentage of the population
# by year since 1990 in the United States
suicide_us_fem_since1990_df <-  suicide_us_df %>% 
  filter(sex == 'female', year > 1989 & year < 2015) %>%
  select(year, sex, population, suicides_per_100K, suicide_pct_pop)%>% 
  group_by(year) %>% 
  summarise(total_pop = sum(population)/1000000,total_suicides_100K_pop = sum(suicides_per_100K))  

ggplot(suicide_us_fem_since1990_df,aes(x=year,y=total_suicides_100K_pop))+geom_point()+
  ggtitle("Number of Female Suicides (in the U.S.) \n per 100K Population by Year Since 1990")+ylab("Suicides per 100K Population")+
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

# An individual country can be entered as the develop_countries variable
develop_countries <- c('United Kingdom','Brazil')
suicide_devworld_df <- suicide_df %>% filter(country %in% 
                                         develop_countries)
suicide_devworld_df$suicides_per_100K <- suicide_devworld_df$suicides_per_100K/length(develop_countries)
suicide_devworld_fem_since1990_df <-  suicide_devworld_df %>% 
  filter(sex == 'female', year > 1989 & year < 2015) %>%
  select(year, sex, population, suicides_per_100K)%>% 
  group_by(year) %>% 
  summarise(total_pop = sum(population)/1000000,total_suicides_100K_pop = sum(suicides_per_100K))  

ggplot(suicide_devworld_fem_since1990_df,aes(x=year,y=total_suicides_100K_pop))+geom_point()+
  ggtitle(paste("Number of Female Suicides (country =",develop_countries,") \n per 100K Population by Year Since 1990"))+ylab("Suicides per 100K Population")+
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="blue", size=12, face="bold")
  )
ggplot(suicide_devworld_fem_since1990_df,aes(x=year,y=total_pop))+geom_point()+
  ggtitle(paste("Female Population (country =",as.character(develop_countries),") by Year Since 1990"))+ylab("Total Population in Millions")+
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="blue", size=12, face="bold")
  )
#Inferential Statistics
mean_us <- mean(suicide_us_fem_since1990_df$total_suicides_100K_pop)
mean_devworld <- mean(suicide_devworld_fem_since1990_df$total_suicides_100K_pop)
shapiro.test(suicide_us_fem_since1990_df$total_suicides_100K_pop)
shapiro.test(suicide_devworld_fem_since1990_df$total_suicides_100K_pop)
t.test(suicide_us_fem_since1990_df$total_suicides_100K_pop,suicide_devworld_fem_since1990_df$total_suicides_100K_pop)
             