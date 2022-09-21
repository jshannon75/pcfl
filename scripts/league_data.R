#install.packages("ffscrapr")
#Instructions for ESPN here: https://ffscrapr.ffverse.com/articles/espn_basics.html

library(ffscrapr)
library(tidyverse)

pcfl <- espn_connect(season = 2022, league_id = 1403922)
#league_info<-ff_league(pcfl)
teams<-read_csv("data/teams22.csv")
  
roster<-ff_rosters(pcfl)
schedule<-ff_schedule(pcfl)

lastweek_results<-schedule %>%
  filter(week==week_sel & result=="W") %>%
  select(-result) %>%
  left_join(teams) %>%
  rename(Winner=franchise_name) %>%
  select(-franchise_id,-week) %>%
  rename(franchise_id=opponent_id,
         `Winner's score`=franchise_score,
         `Loser's score`=opponent_score) %>%
  left_join(teams %>% select(franchise_id,franchise_name)) %>%
  rename(Loser=franchise_name) %>%
  select(Winner,`Winner's score`,Loser,`Loser's score`)

#Best starters
starters<-ff_starters(pcfl) 

starters_10<-starters %>%
  filter(week==week_sel & lineup_slot!="BE") %>%
  rename(Player=player_name,Position=lineup_slot,
         Points=player_score,`PCFL Team`=franchise_name,
         `NFL Team`=team) %>%
  select(Player,Position,`NFL Team`,Points,`PCFL Team`)


#Best player per team
starter_team <-starters %>%
  filter(week==week_sel) %>%
  group_by(franchise_name) %>%
  filter(player_score==max(player_score)) %>%
  rename(Player=player_name,Position=lineup_slot,
         Points=player_score,`PCFL Team`=franchise_name,
         `NFL Team`=team) %>%
  select(Player,Position,`NFL Team`,Points,`PCFL Team`) %>%
  arrange(`PCFL Team`)

#Best bench

starter_benchteam <-starters %>%
  filter(week==week_sel & lineup_slot=="BE") %>%
  group_by(franchise_name) %>%
  summarise(`Bench points`=sum(player_score))

starter_benchteam_all <-starters %>%
  filter(lineup_slot=="BE") %>%
  group_by(franchise_name) %>%
  summarise(`Bench points`=sum(player_score))

#New adds
getlastdate <- function(day,pos) {
  library(lubridate)
  dates <- seq((Sys.Date()-10), (Sys.Date()-pos), by="days")
  dates[wday(dates, label=T)==day]
}

lastadd_date<-getlastdate("Tue",3)

newadds<-roster %>%
  filter(acquisition_date<Sys.Date() & acquisition_date>lastadd_date) %>%
  left_join(starters %>%
              filter(week==week_sel)) %>%
  select(franchise_name,player_name,team,pos,player_score) %>%
    arrange(-player_score)

names(newadds)<-c("PCFL Team","Player","NFL Team","Position","Points")

#Standings
team_points<-schedule %>%
  filter(week <= week_sel) %>%
  group_by(week) %>%
  mutate(team_points=case_when(franchise_score > median(franchise_score) & result=="W"~2,
                               franchise_score > median(franchise_score)~1,
                               franchise_score < median(franchise_score) & result=="W"~1,
                               franchise_score < median(franchise_score) ~ 0)) %>%
  group_by(franchise_id) %>%
  summarise(`Team points`=sum(team_points))

standings<-schedule %>%
  filter(week <= week_sel) %>%
  count(franchise_id,result)%>% 
  left_join(teams %>% select(franchise_id,franchise_name,Division)) %>%
  pivot_wider(names_from=result,values_from=n,values_fill=0) %>%
  left_join(schedule %>%
              filter(week <= week_sel) %>%
              group_by(franchise_id) %>%
              summarise(`Points scored`=sum(franchise_score))) %>%
  mutate(`Avg points`=`Points scored`/(W+L)) %>%
  left_join(team_points) %>%
  arrange(-`Team points`) %>%
  select(franchise_name,Division,`Points scored`,`Avg points`,W,L,`Team points`)


# #hist<-ff_playerscores(pcfl)
# starters<-ff_starters(pcfl)

#Info on NFL players
nfl<-nflfastr_rosters(seasons = 2022) %>%
  mutate(player_id=as.integer(espn_id)) %>%
  rename(nflteam=team) %>%
  select(-week)

nfl_top10<-starters %>%
  filter(week==week_sel) %>%
  count(team) %>%
  top_n(10) %>%
  arrange(-n) %>%
  rename(`NFL Team`=team,`# of players`=n)

college<-starters %>%
  left_join(nfl) %>%
  filter(is.na(college)==FALSE) %>%
  count(college) %>%
  top_n(10) %>%
  arrange(-n) %>%
  rename(`College Team`=college,`# of players`=n)

exp<-starters %>%
  left_join(nfl) %>%
  filter(is.na(years_exp)==FALSE) %>%
  group_by(franchise_id) %>%
  summarise(`Mean NFL experience`=round(mean(years_exp),2)) %>%
  arrange(-`Mean NFL experience`) %>%
  left_join(teams) %>%
  select(franchise_name,`Mean NFL experience`) %>%
  rename(Team=franchise_name)


#Next week 

nextwk<-schedule %>% 
  filter(week==week_sel+1)

nextwk1<-nextwk %>%
  left_join(teams %>% select(franchise_id,franchise_name)) %>%
  select(-franchise_id) %>%
  rename(TeamA=franchise_name,
         franchise_id=opponent_id) %>%
  left_join(teams %>% select(franchise_id,franchise_name)) %>%
  rename(TeamB=franchise_name) %>%
  select(TeamA,TeamB) %>%
  mutate(matchup=row_number()) %>%
  pivot_longer(TeamA:TeamB,names_to="team",values_to="team_name") %>%
  group_by(team_name) %>%
  summarise(matchup=min(matchup)) %>%
  group_by(matchup) %>%
  mutate(team=paste("Team",row_number(),sep=" ")) %>%
  left_join(standings %>%
              rename(team_name=franchise_name) %>%
              select(team_name,`Avg points`)) %>%
  mutate(team_points=paste(team_name," (Avg. points: ",`Avg points`,")",sep="")) %>%
  select(-team_name,-`Avg points`) %>%
  pivot_wider(names_from=team,values_from=team_points) %>%
  ungroup() %>%
  select(-matchup)



# starters_nfl<-starters %>%
#   left_join(nfl,by="player_id")