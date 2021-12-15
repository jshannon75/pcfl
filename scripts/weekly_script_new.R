#Weekly file download

library(httr)
library(tidyverse)
library(lubridate)
library(knitr)

#ReadData
year<-2021
league<-1403922

weeks<-read_csv("data/week_dates.csv") %>%
  filter(mdy(Date) < today()) %>%
  filter(mdy(Date)==max(mdy(Date)))

week_sel<-weeks$Week


url_match<-paste("https://fantasy.espn.com/apis/v3/games/ffl/seasons/",
                 year,"/segments/0/leagues/",league,"?view=mMatchup",sep="")
match_raw<-GET(url_match)

MatchRaw <- rawToChar(match_raw$content)
Match <- jsonlite::fromJSON(MatchRaw)

# url_teams<-paste("https://fantasy.espn.com/apis/v3/games/ffl/seasons/",
#             year,"/segments/0/leagues/",league,sep="")
# teams_raw<-GET(url_teams)
# TeamsRaw <- rawToChar(teams_raw$content)
# Teams <- jsonlite::fromJSON(TeamsRaw)

url_standing<-paste("https://fantasy.espn.com/apis/v3/games/ffl/seasons/",
                    year,"/segments/0/leagues/",league,"?view=mStandings",sep="")
standing_raw<-GET(url_match)

StandingRaw <- rawToChar(standing_raw$content)
standings <- jsonlite::fromJSON(StandingRaw)

# url_sched<-paste("https://fantasy.espn.com/apis/v3/games/ffl/seasons/",
#           year,"/segments/0/leagues/",league,"?view=mSchedule",sep="")
# sched_raw<-GET(url_sched)
# 
# SchedRaw <- rawToChar(sched_raw$content)
# sched <- jsonlite::fromJSON(SchedRaw)

##Download schedule
# schedule<-tibble(
#   week=Match$schedule$matchupPeriodId,
#   away_id=Match$schedule$away$teamId,
#   home_id=Match$schedule$home$teamId
# )
# write_csv(schedule,"data/league_schedule.csv")
schedule<-read_csv("data/leaguesched.csv")

##Download team list 
# team_list<-Teams$teams %>%
#   mutate(teamid=row_number(),
#          fullname=paste(location,nickname))
# write_csv(team_list %>%
#             select(-owners),"league_roster.csv")
team_list<-read_csv("data/teaminfo.csv")


away<-tibble(
  teamId=standings$schedule$away$teamId,
  totalPoints=standings$schedule$away$totalPoints) %>%
  filter(totalPoints>0) %>%
  #select(-gamesPlayed) %>%
  rename(awayteam=teamId,awaypoints=totalPoints)
home<-tibble(
  teamId=standings$schedule$home$teamId,
  totalPoints=standings$schedule$home$totalPoints)  %>%
  filter(totalPoints>0) %>%
  #select(-gamesPlayed) %>%
  rename(hometeam=teamId,homepoints=totalPoints)
results<-bind_cols(away,home) %>%
  mutate(away_result=if_else(awaypoints>homepoints,"W","L"),
         home_result=if_else(homepoints>awaypoints,"W","L"),
         week=ceiling(row_number()/5))
write_csv(results,paste("data/results_wk",week_sel,".csv",sep=""))

#Create standings
names<-c("teamId","points_scored","points_allowed","result","week")
away_sel<-results %>%
  select(awayteam,awaypoints,homepoints,away_result,week)
names(away_sel)<-names

home_sel<-results %>%
  select(hometeam,homepoints,awaypoints,home_result,week)
names(home_sel)<-names

stand_tbl1<-bind_rows(away_sel,home_sel) %>%
  group_by(teamId) %>%
  summarise(points_scored=sum(points_scored),
            points_allowed=sum(points_allowed))
stand_tbl<-bind_rows(away_sel,home_sel) %>%
  count(teamId,result) %>% 
  left_join(team_list %>% 
              select(teamId,fullname,division)) %>%
  pivot_wider(names_from=result,
              values_from=n,
              values_fill=0) %>%
  left_join(stand_tbl1) %>%
  select(fullname,division,W,L,points_scored,points_allowed) %>%
  arrange(-points_scored) %>%
  mutate(points_rank=1:n()) %>%
  arrange(-W,-points_scored) %>% 
  mutate(playoff_rank = 1:n())
write_csv(stand_tbl,paste("data/stand_tbl_wk",week_sel,".csv",sep=""))

#Top points
toppoints<-bind_rows(away_sel,home_sel) %>%
  left_join(team_list) %>%
  select(fullname,week,points_scored,result) %>%
  top_n(5,points_scored) %>%
  arrange(-points_scored) 
write_csv(toppoints,paste("data/toppoints_wk",week_sel,".csv",sep=""))

bottom_points<-bind_rows(away_sel,home_sel) %>%
  left_join(team_list) %>%
  select(fullname,week,points_scored,result) %>%
  top_n(-5,points_scored) %>%
  arrange(points_scored) 

#Last week's games
player_extract<-function(team_slot){
  df<-Match$teams$roster$entries[[team_slot]]
  add_date<-as.Date(as.POSIXct(df$acquisitionDate/1000, origin="1970-01-01"))
  name<-df$playerPoolEntry$player$fullName
  points<-df$playerPoolEntry$appliedStatTotal
  slot<-df$lineupSlotId
  
  tibble(add_date,name,points,slot) %>%
    mutate(bench=if_else(slot<20,0,1),
           teamId=team_slot)
}

players<-map_df(1:10,player_extract) %>%
  left_join(team_list)
write_csv(players,paste("data/players_wk",week_sel,".csv",sep=""))

benchpoints<-players %>%
  group_by(abbrev,bench) %>%
  summarise(points=sum(points)) %>%
  filter(bench==1) %>%
  left_join(team_list) %>%
  ungroup() %>%
  select(fullname,points) %>%
  distinct() 
write_csv(benchpoints,paste("data/benchpoints_wk",week_sel,".csv",sep=""))


recent_adds<-players %>%
  mutate(stay=time_length(interval(start=add_date,end=today()),"day")) %>%
  filter(stay<7)  
write_csv(recent_adds,paste("data/recent_adds_wk",week_sel,".csv",sep=""))

#Last week's games
last_week<-results %>%
  filter(week==max(week)) %>%
  select(-away_result:-week) %>%
  left_join(team_list %>%
              select(teamId,fullname) %>%
              rename(awayteam=teamId)) %>%
  rename(away_team=fullname) %>%
  left_join(team_list %>%
              select(teamId,fullname) %>%
              rename(hometeam=teamId)) %>%
  rename(home_team=fullname) %>%
  select(away_team,awaypoints,home_team,homepoints)
write_csv(last_week,paste("data/wk",week_sel,".csv",sep=""))
########
#Weekly file download

library(httr)
library(tidyverse)
library(lubridate)
library(knitr)

#ReadData
year<-2021
league<-1403922

weeks<-read_csv("data/week_dates.csv") %>%
  filter(mdy(Date) < today()) %>%
  filter(mdy(Date)==max(mdy(Date)))

week_sel<-weeks$Week


url_match<-paste("https://fantasy.espn.com/apis/v3/games/ffl/seasons/",
                 year,"/segments/0/leagues/",league,"?view=mMatchup",sep="")
match_raw<-GET(url_match)

MatchRaw <- rawToChar(match_raw$content)
Match <- jsonlite::fromJSON(MatchRaw)

# url_teams<-paste("https://fantasy.espn.com/apis/v3/games/ffl/seasons/",
#             year,"/segments/0/leagues/",league,sep="")
# teams_raw<-GET(url_teams)
# TeamsRaw <- rawToChar(teams_raw$content)
# Teams <- jsonlite::fromJSON(TeamsRaw)

url_standing<-paste("https://fantasy.espn.com/apis/v3/games/ffl/seasons/",
                    year,"/segments/0/leagues/",league,"?view=mStandings",sep="")
standing_raw<-GET(url_match)

StandingRaw <- rawToChar(standing_raw$content)
standings <- jsonlite::fromJSON(StandingRaw)

# url_sched<-paste("https://fantasy.espn.com/apis/v3/games/ffl/seasons/",
#           year,"/segments/0/leagues/",league,"?view=mSchedule",sep="")
# sched_raw<-GET(url_sched)
# 
# SchedRaw <- rawToChar(sched_raw$content)
# sched <- jsonlite::fromJSON(SchedRaw)

##Download schedule
# schedule<-tibble(
#   week=Match$schedule$matchupPeriodId,
#   away_id=Match$schedule$away$teamId,
#   home_id=Match$schedule$home$teamId
# )
# write_csv(schedule,"data/league_schedule.csv")
schedule<-read_csv("data/leaguesched.csv")

##Download team list 
team_list<-Teams$teams %>%
  mutate(teamid=row_number(),
         fullname=paste(location,nickname))
# write_csv(team_list %>%
#             select(-owners),"league_roster.csv")
team_list<-read_csv("data/teaminfo1.csv",lazy = FALSE)


away<-tibble(
  teamId=standings$schedule$away$teamId,
  totalPoints=standings$schedule$away$totalPoints) %>%
  filter(totalPoints>0) %>%
  #select(-gamesPlayed) %>%
  rename(awayteam=teamId,awaypoints=totalPoints)
home<-tibble(
  teamId=standings$schedule$home$teamId,
  totalPoints=standings$schedule$home$totalPoints)  %>%
  filter(totalPoints>0) %>%
  #select(-gamesPlayed) %>%
  rename(hometeam=teamId,homepoints=totalPoints)
results<-bind_cols(away,home) %>%
  mutate(away_result=if_else(awaypoints>homepoints,"W","L"),
         home_result=if_else(homepoints>awaypoints,"W","L"),
         week=ceiling(row_number()/5))
write_csv(results,paste("data/results_wk",week_sel,".csv",sep=""))

#Create standings
names<-c("teamId","points_scored","points_allowed","result","week")
away_sel<-results %>%
  select(awayteam,awaypoints,homepoints,away_result,week)
names(away_sel)<-names

home_sel<-results %>%
  select(hometeam,homepoints,awaypoints,home_result,week)
names(home_sel)<-names

stand_tbl1<-bind_rows(away_sel,home_sel) %>%
  group_by(teamId) %>%
  summarise(points_scored=sum(points_scored),
            points_allowed=sum(points_allowed))
stand_tbl<-bind_rows(away_sel,home_sel) %>%
  count(teamId,result) %>% 
  left_join(team_list %>% 
              select(teamId,fullname,division)) %>%
  pivot_wider(names_from=result,
              values_from=n,
              values_fill=0) %>%
  left_join(stand_tbl1) %>%
  select(fullname,division,W,L,points_scored,points_allowed) %>%
  arrange(-points_scored) %>%
  mutate(points_rank=1:n()) %>%
  arrange(-W,-points_scored) %>% 
  mutate(playoff_rank = 1:n())
write_csv(stand_tbl,paste("data/stand_tbl_wk",week_sel,".csv",sep=""))

#Top points
toppoints<-bind_rows(away_sel,home_sel) %>%
  left_join(team_list) %>%
  select(fullname,week,points_scored,result) %>%
  top_n(5,points_scored) %>%
  arrange(-points_scored) 
write_csv(toppoints,paste("data/toppoints_wk",week_sel,".csv",sep=""))

bottom_points<-bind_rows(away_sel,home_sel) %>%
  left_join(team_list) %>%
  select(fullname,week,points_scored,result) %>%
  top_n(-5,points_scored) %>%
  arrange(points_scored) 

#Last week's games
player_extract<-function(team_slot){
  df<-Match$teams$roster$entries[[team_slot]]
  add_date<-as.Date(as.POSIXct(df$acquisitionDate/1000, origin="1970-01-01"))
  name<-df$playerPoolEntry$player$fullName
  points<-df$playerPoolEntry$appliedStatTotal
  slot<-df$lineupSlotId
  
  tibble(add_date,name,points,slot) %>%
    mutate(bench=if_else(slot<20,0,1),
           teamId=team_slot)
}

players<-map_df(1:10,player_extract) %>%
  rename(teamId1=teamId) %>%
  left_join(team_list)
write_csv(players,paste("data/players_wk",week_sel,".csv",sep=""))

benchpoints<-players %>%
  group_by(abbrev,bench) %>%
  summarise(points=sum(points)) %>%
  filter(bench==1) %>%
  left_join(team_list) %>%
  ungroup() %>%
  select(fullname,points) %>%
  distinct() 
write_csv(benchpoints,paste("data/benchpoints_wk",week_sel,".csv",sep=""))


recent_adds<-players %>%
  mutate(stay=time_length(interval(start=add_date,end=today()),"day")) %>%
  filter(stay<7)
write_csv(recent_adds,paste("data/recent_adds_wk",week_sel,".csv",sep=""))

#Last week's games
last_week<-results %>%
  filter(week==max(week)) %>%
  select(-away_result:-week) %>%
  left_join(team_list %>%
              select(teamId,fullname) %>%
              rename(awayteam=teamId)) %>%
  rename(away_team=fullname) %>%
  left_join(team_list %>%
              select(teamId,fullname) %>%
              rename(hometeam=teamId)) %>%
  rename(home_team=fullname) %>%
  select(away_team,awaypoints,home_team,homepoints)
write_csv(last_week,paste("data/wk",week_sel,".csv",sep=""))

#Next week's games
points3wk<-results %>%
  select(awayteam,awaypoints,week) %>%
  rename(teamId=awayteam,
         points=awaypoints) %>%
  bind_rows(results %>%
              select(hometeam,homepoints,week) %>%
              rename(teamId=hometeam,
                     points=homepoints)) %>%
  group_by(teamId) %>% 
  top_n(3,week) %>%
  summarise(points3wk=sum(points))

week_next<-week_sel+1
nextweek_long<-schedule %>%
  filter(week==week_next) %>%
  mutate(gameid=row_number()) %>%
  pivot_longer(hometeam:awayteam,
               names_to="awayhome",
               values_to="teamId") %>%
  left_join(team_list %>% 
              select(teamId,fullname)) %>%
  left_join(points3wk) %>%
  select(-teamId) 

nextweek<-nextweek_long %>%
  filter(awayhome=="awayteam") %>%
  rename(away_team=fullname,
         away_points=points3wk) %>%
  left_join(nextweek_long %>%
              filter(awayhome=="hometeam") %>%
              select(-awayhome) %>%
              rename(home_team=fullname,
                     home_points=points3wk)) %>%
  mutate(range=abs(away_points-home_points)) %>%
  select(-gameid,-awayhome) 

write_csv(nextweek,paste("data/nextweek_wk",week_sel,".csv",sep=""))
