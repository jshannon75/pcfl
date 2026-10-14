library(tidyverse)
library(readxl)

scores1<-read_xlsx("espn_legacy_games_2014_2018.xlsx") %>%
  select(-`Home Team`,-`Away Team`,-`Winner`) %>%
  pivot_longer(
    cols = c(`Home Name`, `Home Score`, `Away Name`, `Away Score`),
    names_to = c("Location", ".value"),
    names_pattern = "(Home|Away) (Name|Score)"
  ) %>%
  select(-Location) %>%
  rename(Team=Name)

scores2<-read_csv("espn_game_by_game_history.csv") %>%
  select(-`Home Owner`,-`Away Owner`,-Winner) %>%
  pivot_longer(
    cols = c(`Home Team`, `Home Score`, `Away Team`, `Away Score`),
    names_to = c("Location", ".value"),
    names_pattern = "(Home|Away) (Team|Score)"
  ) %>%
  select(-Location)

scores_all<-bind_rows(scores1,scores2) %>%
  filter(!(Year == 2026 & Week > 5))

write_csv(scores_all,"scores_alltime.csv")
