
# ADD LIBRARIES ----
library(tidyverse)
library(here)
library(fitzRoy)


# GET STATS FROM PREVIOUS 2 YEARS ----

player_stats_raw <- fetch_player_stats(season = 2019:2021,
                                       comp = "AFLM",
                                       source = "afltables")

player_stats <- player_stats_raw %>% 
  mutate(Name = paste0(First.name," ",Surname)) %>% 
  filter(Season == 2021) %>% 
  select(ID, Name, Goals) %>% 
  group_by(ID, Name) %>% 
  summarise(gamesPlayed = n(),
            goalsScored = sum(Goals, na.rm = TRUE),
            goalsPerGame = goalsScored / gamesPlayed) %>% 
  filter(goalsScored >= 10) %>% 
  mutate(idx_raw = gamesPlayed + 22*goalsPerGame) %>% 
  arrange(-idx_raw)

write.csv(player_stats, file = here("draft_material","defaultDraftOrder.csv"))
