##League setup

library(httr)
library(tidyverse)
library(lubridate)

year<-2021
league<-1403922

url<-paste("https://fantasy.espn.com/apis/v3/games/ffl/seasons/",
           year,"/segments/0/leagues/",league,"?view=mMatchup",sep="")
league_raw<-GET(url)

ESPNRaw <- rawToChar(league_raw$content)
Match <- jsonlite::fromJSON(ESPNRaw)

#Get teams
url1<-paste("https://fantasy.espn.com/apis/v3/games/ffl/seasons/",
            year,"/segments/0/leagues/",league,sep="")
league_raw1<-GET(url1)
ESPNRaw1 <- rawToChar(league_raw1$content)
ESPNFromJSON1 <- jsonlite::fromJSON(ESPNRaw1)

teams<-ESPNFromJSON1$teams %>%
  mutate(teamId=row_number(),
         fullname=paste(location,nickname))

write_csv(teams,"data/teaminfo.csv") ##Need to manually add division

#Get schedule
sched_away<-Match$schedule$away %>%
  select(teamId) %>%
  mutate(week=ceiling(row_number()/5),
         game=row_number()-((week-1)*5)) %>%
  rename(awayteam=teamId)

sched_home<-Match$schedule$home %>%
  select(teamId) %>%
  mutate(week=ceiling(row_number()/5),
         game=row_number()-((week-1)*5)) %>%
  rename(hometeam=teamId)

sched_all<-sched_away %>%
  left_join(sched_home) %>%
  select(week,game,hometeam,awayteam)

write_csv(sched_all,"data/leaguesched.csv")
