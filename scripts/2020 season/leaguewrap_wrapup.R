#End of year
#Run weekly script first

#Margin of victory
margin<-results %>%
  mutate(margin=abs(awaypoints-homepoints))

#Ending roster
player_files<-paste("data/",list.files(path="data/",pattern="players_w"),sep="")
read_player<-function(df){
  read_csv(df) %>%
    mutate(file=df)
}

players_all<-map_df(player_files,read_player)
players_sum<-players_all %>%
  group_by(name,fullname) %>%
  summarise(totpoints=sum(points))

players_top <-players_sum %>%
  group_by(fullname) %>%
  filter(totpoints==max(totpoints))

players_bottom<-players_all %>%
  group_by(name,fullname) %>%
  summarise(totpoints=sum(points),
            weeks=n()) %>%
  group_by(fullname) %>%
  filter(weeks>7) %>%
  filter(totpoints==min(totpoints))

#Draft recap
draft_recap<-read_csv("data/draft_recap.csv") %>%
  rename(name=Player1) %>%
  left_join(players_sum)
 
write_csv(draft_recap,"data/draft_recap_points.csv")

round_win<-draft_recap %>%
  group_by(Round) %>%
  filter(totpoints==max(totpoints)) %>%
  select(Round,Pick,Team,name,totpoints)

undrafted<-players_all %>%
  filter(add_date>"2020-09-08") %>%
  group_by(name,fullname) %>%
  summarise(totpoints=sum(points))
  anti_join(draft_recap)

#Points SD
teamtrend<-results %>%
    select(awayteam,awaypoints,week) %>%
    rename(teamId=awayteam,
           points=awaypoints) %>%
    bind_rows(results %>%
                select(hometeam,homepoints,week) %>%
                rename(teamId=hometeam,
                       points=homepoints)) %>%
    mutate(week=factor(week,levels=c(1,2,3,4,5,6,7,8,9,10,11, 12,13,14,15,16))) %>%
    left_join(team_list %>%
                select(teamId,fullname))

volatile<-teamtrend %>%
  group_by(fullname) %>%
  summarise(mean=round(mean(points),1),
            sd=round(sd(points),1))
