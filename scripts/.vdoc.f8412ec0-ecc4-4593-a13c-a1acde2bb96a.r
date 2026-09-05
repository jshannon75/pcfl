#
#
#
#
#
#
#
#
#
#
#
#
#
#
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(knitr)
library(lubridate)
library(svglite)
library(kableExtra)
#library(hrbrthemes)
library(showtext)

setwd(here::here())

#font_add_google("IBM Plex Sans", "ibm")
extrafont::loadfonts()
week_sel=3

source("scripts/league_data.R")

#
#
#
#
#
#
#
kable(lastweek_results)%>%
  kable_styling(bootstrap_options = c("striped","hover"), full_width = F, position = "left")
#
#
#
#
#
#
#
#
#
#
kable(top_n(starters,10,Points) %>%
        arrange(-Points))%>%
  kable_styling(bootstrap_options = c("striped","hover"), full_width = F, position = "left")
#
#
#
#
#
#
#
#
kable(starter_team)%>%
  kable_styling(bootstrap_options = c("striped","hover"), full_width = F, position = "left")
#
#
#
#
#
#
kable(starter_benchteam %>% 
        arrange(-`Bench points`) %>%
  rename(Team=`PCFL Team`))%>%
  kable_styling(bootstrap_options = c("striped","hover"), full_width = F, position = "left")
#
#
#
#
# Here's the season's bench standings:
# 
# kable(starter_benchteam_all %>%
#         arrange(-`Bench points`) %>%
# rename(Team=franchise_name))%>%
#   kable_styling(bootstrap_options = c("striped","hover"), full_width = F, position = "left")
#
#
#
#
#
#
# kable(activity_info %>% arrange(-Points)) %>%
#   kable_styling(bootstrap_options = c("striped","hover"), full_width = F, position = "left")
#
#
#
#
#
#
#
# kable(arrange(toppoints,-Score))%>%
#   kable_styling(bootstrap_options = c("striped","hover"), full_width = F, position = "left")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
kable(standings)%>%
  kable_styling(bootstrap_options = c("striped","hover"), full_width = F, position = "left")

#
#
#
#
#
#
#
#
#
kbl(nextwk1) %>%
  kable_styling(bootstrap_options = c("striped","hover"), full_width = F, position = "left")
#
#
#
#
#
