
# ADD LIBRARIES ----
library(tidyverse)
library(here)
library(fitzRoy)


# GET STATS FROM PREVIOUS SEASON ----

player_stats_raw <- fetch_player_stats(season = 2024,
                                       comp = "AFLM",
                                       source = "footywire")

player_stats <- player_stats_raw %>% 
  mutate(Goals = G) %>%
  select(Player, Team, Goals) %>% 
  group_by(Team, Player) %>% 
  summarise(gamesPlayed = n(),
            goalsScored = sum(Goals, na.rm = TRUE),
            goalsPerGame = goalsScored / gamesPlayed) %>% 
  filter(goalsScored >= 10) %>% 
#  mutate(idx_raw = gamesPlayed + 22*goalsPerGame) %>% 
  ungroup() %>% 
  select(-Team) %>% 
  arrange(-goalsScored)

write.csv(player_stats, file = here("draft_material","defaultDraftOrder.csv"), row.names = F)
