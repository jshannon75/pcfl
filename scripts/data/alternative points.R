#Alternative points framework

library(tidyverse)

results21<-read_csv("data/2021 season/results_wk17.csv")
teams<-read_csv("data/2021 season/teaminfo1.csv")

results21_home<-results21 %>%
  select(hometeam,homepoints,home_result,week) %>%
  rename(team=hometeam,points=homepoints,result=home_result)

results21_away<-results21 %>%
  select(awayteam,awaypoints,away_result,week) %>%
  rename(team=awayteam,points=awaypoints,result=away_result)

results21_long<-bind_rows(results21_home,results21_away) %>%
  mutate(result_dummy=if_else(result=="W",1,0)) %>%
  group_by(week) %>%
  mutate(points_median=median(points),
    points_dummy=if_else(points>median(points),1,0),
    points_all=result_dummy+points_dummy) %>%
  rename(teamId=team) %>%
  filter(week<16)

standings_alt<-results21_long %>%
  group_by(teamId) %>%
  summarise(points_toal=sum(points_all),
            wins=sum(result_dummy),
            points_median=sum(points_dummy),
            points_all=sum(points)) %>%
  left_join(teams)
