library(readr)
library(dplyr)
library(ggplot2)
suicide_df <- read_csv("C:/Users/Richard/AppData/Local/GitHubDesktop/app-1.4.2/Statistics-Meeting/WHO_new_suicide_statistics.csv")
suicide_sum_df <- suicide_df %>% group_by(year,sex,age) %>% summarise(sum(suicides_no,sum(population)))
suicide_us_df <- suicide_df %>% filter(country=='United States')
suicide_us_df <- suicide_us_df %>% mutate(suicide_pct_pop = suicides_no/population)


# create data frame that shows female suicides as a percentage of the population
# by year since 2000
suicide_us_fem_since2000_df <-  suicide_us_df %>% 
  filter(sex == 'female', year > 1999) %>%
  select(year, sex, population, suicide_pct_pop)%>% 
  group_by(year) %>% 
  summarise(total_pop = sum(population)/1000000,total_pct_pop = sum(suicide_pct_pop))  

ggplot(suicide_us_fem_since2000_df,aes(x=year,y=total_pct_pop))+geom_point()+
  ggtitle("Number of Female Suicides (in the U.S.) \n per Percent of Population by Year Since 2000")+ylab("suicides by percent of population")+
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="blue", size=12, face="bold")
  )
ggplot(suicide_us_fem_since2000_df,aes(x=year,y=total_pop))+geom_point()+
  ggtitle("Female Population (in the U.S.) by Year Since 2000")+ylab("Total Population in Millions")+
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="blue", size=12, face="bold")
  )


