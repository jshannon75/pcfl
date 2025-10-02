library(tidyverse)

#remotes::install_github("k5cents/fflr")
library(fflr)

ffl_id(leagueId = "1403922")
league_info()


# teams<-league_teams(leagueId = "1403922")
# write_csv(teams,"data/teams25.csv")
teams<-read_csv("data/teams25.csv")

draft<-draft_recap()

roster_get<-function(week_sel){
  roster_sel<-team_roster(scoringPeriodId=week_sel)
  bind_rows(roster_sel)
}

roster_all<-map_df(1:week_sel,roster_get) %>%
  left_join(teams)

roster_draft<-roster_all %>%
  inner_join(draft %>% select(teamId,playerId)) %>%
  filter(lineupSlot!="BE") %>%
  group_by(abbrev,name) %>%
  summarise(draftpoints=sum(actualScore))

roster_pickup<-roster_all %>%
  anti_join(draft %>% select(teamId,playerId)) %>%
  filter(lineupSlot!="BE") %>%
  group_by(abbrev,name) %>%
  summarise(draftpoints=sum(actualScore))

roster<-roster_all %>%
  filter(scoringPeriodId==week_sel)

schedule<-tidy_scores()

lastweek_winners<-schedule %>%
  filter(matchupPeriodId==week_sel&isWinner==TRUE) %>%
  left_join(teams) %>%
  select(matchupId,name,totalPoints) %>%
  rename(Winner=name,
         franchise_score=totalPoints) 


lastweek_results<-schedule %>%
  filter(matchupPeriodId==week_sel&isWinner==FALSE) %>%
  left_join(teams) %>%
  select(matchupId,name,totalPoints) %>%
  rename(Loser=name,
         opponent_score=totalPoints)   %>%
  left_join(lastweek_winners) %>%
  select(Winner,franchise_score,Loser,opponent_score) %>%
  rename(`Winner's score`=franchise_score,
         `Loser's score`=opponent_score)

#Best starters
starters<-roster_all  %>%
  filter(scoringPeriodId==week_sel) %>%
  left_join(teams) %>%
  mutate(Player=paste(firstName,lastName),
         Position=as.character(lineupSlot)) %>%
  rename(Points=actualScore,
         `PCFL Team`=name,
         `NFL Team`=proTeam) %>%
  select(Player,Position,`NFL Team`,Points,`PCFL Team`)

# #Points contribution
# starters_pct<-starters %>%
#   filter(!lineup_slot %in% c("BE","IR")) %>%
#   group_by(name,lineup_slot) %>%
#   summarise(points=sum(player_score)) %>%
#   group_by(name) %>%
#   mutate(totpoints=sum(points),
#          points_pct=points/totpoints*100)

# #Optimal lineups each week
# bestline_qb<-starters %>%
#   filter(Position=="QB") %>%
#   group_by(name,week) %>%
#   filter(player_score==max(player_score))
# 
# bestline_rb<-starters %>%
#   filter(pos=="RB") %>%
#   group_by(name,week) %>%
#   top_n(wt=player_score,2)
# 
# bestline_wr<-starters %>%
#   filter(pos=="WR") %>%
#   group_by(name,week) %>%
#   top_n(wt=player_score,2)
# 
# bestline_te<-starters %>%
#   filter(pos=="TE") %>%
#   group_by(name,week) %>%
#   top_n(wt=player_score,1)
# 
# bestline_k<-starters %>%
#   filter(pos=="K") %>%
#   group_by(name,week) %>%
#   top_n(wt=player_score,1)
# 
# bestline_d<-starters %>%
#   filter(pos=="DST") %>%
#   group_by(name,week) %>%
#   top_n(wt=player_score,1)
# 
# bestline_temp<-bind_rows(bestline_qb,bestline_rb,bestline_wr,bestline_k,bestline_d,bestline_te) 
# bestline_flex<-starters %>%
#   anti_join(bestline_temp) %>%
#   filter(pos %in% c("RB","WR","TE")) %>%
#   group_by(name,week) %>%
#   top_n(wt=player_score,1)
# 
# bestline_all<-bind_rows(bestline_temp,bestline_flex) %>%
#   ungroup() %>%
#   group_by(name,week) %>%
#   summarise(bestline_points=sum(player_score))
# 
# actualpoints<-starters %>%
#   filter(lineup_slot!="BE") %>%
#   group_by(name,week) %>%
#   summarise(actual_points=sum(player_score)) %>%
#   left_join(bestline_all) %>%
#   mutate(optimize_pct=round(actual_points/bestline_points*100,0)) %>%
#   arrange(-optimize_pct) %>%
#   mutate(`Optimization rate`=paste(optimize_pct,"%",sep="")) %>%
#   rename(`PCFL Team`=name,
#          Week=week,
#          `Actual points`=actual_points,
#          `Best lineup`=bestline_points) %>%
#     select(-optimize_pct)
# 
# actualpoints_season<-actualpoints %>%
#   group_by(`PCFL Team`) %>%
#   summarise(`Actual points`=sum(`Actual points`),
#             `Optimized points`=sum(`Best lineup`)) %>%
#   arrange(-`Actual points`) %>%
#   mutate(rank=row_number()) %>%
#   pivot_longer(`Actual points`:`Optimized points`,names_to="Type",values_to="Points")

#Best player per team
starter_team <-starters %>%
  filter(Position!="BE") %>%
  group_by(`PCFL Team`) %>%
  filter(Points==max(Points)) %>%
  arrange(`PCFL Team`)

#Best bench
starter_benchteam <-starters %>%
  filter(Position=="BE") %>%
  group_by(`PCFL Team`) %>%
  summarise(`Bench points`=sum(Points,na.rm=TRUE))

starter_benchteam_all <-roster_all%>%
  filter(lineupSlot=="BE") %>%
  group_by(name) %>%
  summarise(`Bench points`=sum(actualScore,na.rm=TRUE))

#New adds
activity<-recent_activity(scoringPeriodId = week_sel) %>%
  filter(type=="FREEAGENT")
activity_info<-bind_rows(activity$items) %>%
  filter(type=="ADD") %>%
  left_join(roster %>% dplyr::select(playerId,firstName,lastName,lineupSlot,proTeam)) %>%
  filter(is.na(firstName)==FALSE) %>%
  left_join(roster %>% select(playerId,actualScore))%>%
  rename(teamId=toTeamId) %>%
  left_join(teams %>% select(teamId,name)) %>%
  mutate(player_name=paste(firstName,lastName,sep=" ")) %>%
  select(name,player_name,proTeam,lineupSlot,actualScore)

names(activity_info)<-c("PCFL Team","Player","NFL Team","Position","Points")


# library(lubridate)
# 
# getlastdate <- function(day,pos) {
#   dates <- seq((Sys.Date()-10), (Sys.Date()-pos), by="days")
#   dates[wday(dates, label=T)==day]
# }
# 
# lastadd_date<-getlastdate("Tue",1)
# 
# newadds<-roster %>%
#   filter(acquisition_date>lastadd_date[1] & acquisition_date<lastadd_date[2]) %>%
#   left_join(starters %>%
#               filter(week==week_sel)) %>%
#   select(name,player_name,team,pos,player_score) %>%
#     arrange(-player_score)


#Standings
team_points<- schedule %>%
  rename(week=matchupPeriodId,
         franchise_score=totalPoints) %>%
  filter(week <= week_sel) %>%
  group_by(week) %>%
  mutate(team_points=case_when(franchise_score > median(franchise_score) & isWinner==TRUE~2,
                               franchise_score > median(franchise_score)~1,
                               franchise_score < median(franchise_score) & isWinner==TRUE~1,
                               franchise_score < median(franchise_score) ~ 0)) %>%
  group_by(teamId) %>%
  summarise(`Team points`=sum(team_points))

last3avg<-schedule %>%
  rename(week=matchupPeriodId,
         franchise_score=totalPoints) %>%
  filter(week <= week_sel & week>week_sel-3) %>%
  group_by(teamId) %>%
  summarise(weeks=max(row_number()),
            `Avg last 3`=round(sum(franchise_score)/weeks,2)) %>%
  select(-weeks)

standings<-schedule %>%
  rename(week=matchupPeriodId,
         franchise_score=totalPoints) %>%
  mutate(result=if_else(isWinner==TRUE,"W","L")) %>%
  filter(week <= week_sel) %>%
  count(teamId,result)%>% 
  left_join(teams %>% select(teamId,name)) %>%
  pivot_wider(names_from=result,values_from=n,values_fill=0) %>%
  left_join(schedule %>%
              filter(matchupPeriodId <= week_sel) %>%
              group_by(teamId) %>%
              summarise(`Points scored`=sum(totalPoints))) %>%
  mutate(`Avg points`=round(`Points scored`/(W+L),2)) %>%
  left_join(last3avg) %>%
  left_join(team_points) %>%
  arrange(-`Team points`,-`Points scored`) %>%
  select(name,`Points scored`,`Avg points`,`Avg last 3`,W,L,`Team points`) %>%
  rename(`PCFL Team`=name)

stargazer::stargazer(standings,type="text",summary=F)

# #hist<-ff_playerscores(pcfl)
# starters<-ff_starters(pcfl)

#Info on NFL players
# nfl<-nflfastr_rosters(seasons = 2022) %>%
#   mutate(player_id=as.integer(espn_id)) %>%
#   rename(nflteam=team) %>%
#   select(-week)
# 
# nfl_top10<-starters %>%
#   filter(week==week_sel) %>%
#   count(team) %>%
#   top_n(10) %>%
#   arrange(-n) %>%
#   rename(`NFL Team`=team,`# of players`=n)
# 
# college<-starters %>%
#   left_join(nfl) %>%
#   filter(is.na(college)==FALSE) %>%
#   count(college) %>%
#   top_n(10) %>%
#   arrange(-n) %>%
#   rename(`College Team`=college,`# of players`=n)
# 
# exp<-starters %>%
#   left_join(nfl) %>%
#   filter(is.na(years_exp)==FALSE) %>%
#   group_by(franchise_id) %>%
#   summarise(`Mean NFL experience`=round(mean(years_exp),2)) %>%
#   arrange(-`Mean NFL experience`) %>%
#   left_join(teams) %>%
#   select(name,`Mean NFL experience`) %>%
#   rename(Team=name)


#Next week 

nextwk<-schedule %>% 
  filter(matchupPeriodId==week_sel+1)

nextwk1<-nextwk %>%
  left_join(teams) %>%
  left_join(teams %>% select(teamId,name)) %>%
  select(-teamId) %>%
  # rename(TeamA=name,
  #        franchise_id=opponent_id) %>%
  # left_join(teams %>% select(franchise_id,name)) %>%
  # rename(TeamB=name) %>%
  # select(TeamA,TeamB) %>%
  # mutate(matchup=row_number()) %>%
  #pivot_longer(TeamA:TeamB,names_to="team",values_to="team_name") %>%
  #group_by(team_name) %>%
  #summarise(matchup=min(matchup)) %>%
  #group_by(matchup) %>%
  #mutate(team=paste("Team",row_number(),sep=" ")) %>%
  left_join(standings %>%
              rename(name=`PCFL Team`) %>%
              select(name,`Avg last 3`)) %>%
  mutate(team_points=paste(name," (Avg. last 3: ",round(`Avg last 3`,2),")",sep="")) %>%
  select(matchupId,team_points) %>%
  group_by(matchupId) %>%
  mutate(rownum=row_number()) %>%
  pivot_wider(names_from=rownum,values_from=team_points) %>%
  ungroup() %>%
  select(-matchupId) 

names(nextwk1)<-c("Team 1","Team 2")

toppoints<-schedule %>%
  top_n(5,totalPoints) %>%
  left_join(teams) %>%
  mutate(result=if_else(isWinner==TRUE,"W","L")) %>%
  select(name,matchupPeriodId,totalPoints,result) %>%
  rename(`PCFL Team`=name,
         Week=matchupPeriodId,
         Score=totalPoints,
         Result=result)

# starters_nfl<-starters %>%
#   left_join(nfl,by="player_id")

