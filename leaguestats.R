library(httr)
library(tidyverse)
library(lubridate)

year<-2020
league<-1403922

url<-paste("https://fantasy.espn.com/apis/v3/games/ffl/seasons/",
          year,"/segments/0/leagues/",league,"?view=mMatchup",sep="")
league_raw<-GET(url)

ESPNRaw <- rawToChar(league_raw$content)
Match <- jsonlite::fromJSON(ESPNRaw)

url1<-paste("https://fantasy.espn.com/apis/v3/games/ffl/seasons/",
            year,"/segments/0/leagues/",league,sep="")
league_raw1<-GET(url1)
ESPNRaw1 <- rawToChar(league_raw1$content)
ESPNFromJSON1 <- jsonlite::fromJSON(ESPNRaw1)

teams<-ESPNFromJSON1$teams %>%
  mutate(teamid=row_number())

awaypoints<-ESPNFromJSON$schedule$away$totalPoints
awayteam<-ESPNFromJSON$schedule$away$totalPoints
awayroster<-ESPNFromJSON$schedule$away$rosterForCurrentScoringPeriod$entries[[12]]

player_extract<-function(team_slot){
  df<-ESPNFromJSON$teams$roster$entries[[team_slot]]
  add_date<-as.Date(as.POSIXct(df$acquisitionDate/1000, origin="1970-01-01"))
  name<-df$playerPoolEntry$player$fullName
  points<-df$playerPoolEntry$appliedStatTotal
  slot<-df$lineupSlotId
  
  tibble(add_date,name,points,slot) %>%
    mutate(bench=if_else(slot<20,0,1),
           teamid=team_slot)
}
players<-map_df(1:10,player_extract) %>%
  left_join(teams)

benchpoints<-players %>%
  group_by(abbrev,bench) %>%
  summarise(points=sum(points))

recent_adds<-players %>%
  mutate(stay=time_length(interval(start=add_date,end=today()),"day")) %>%
  filter(stay<7)

         