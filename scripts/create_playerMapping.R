
# ADD LIBRARIES ----
library(tidyverse)
library(here)
library(fitzRoy)


teamList <- c("Adelaide", "Brisbane Lions", "Carlton", "Collingwood", "Essendon", 
              "Fremantle", "GWS", "Geelong", "Gold Coast", "Hawthorn", "Melbourne", 
              "North Melbourne", "Port Adelaide", "Richmond", "St Kilda", "Sydney", 
              "West Coast", "Western Bulldogs", "Footscray")


get_team_abrev_footywire <- function(team) 
{
  team_abr <- dplyr::case_when(team == "Adelaide" ~ "adelaide-crows", 
                               team == "Brisbane Lions" ~ "brisbane-lions", 
                               team == "Carlton" ~ "carlton-blues", 
                               team == "Collingwood" ~ "collingwood-magpies", 
                               team == "Essendon" ~ "essendon-bombers", 
                               team == "Fremantle" ~ "fremantle-dockers", 
                               team == "GWS" ~ "greater-western-sydney-giants", 
                               team == "Geelong" ~ "geelong-cats", 
                               team == "Gold Coast" ~ "gold-coast-suns", 
                               team == "Hawthorn" ~ "hawthorn-hawks", 
                               team == "Melbourne" ~ "melbourne-demons", 
                               team == "North Melbourne" ~ "kangaroos", 
                               team == "Port Adelaide" ~ "port-adelaide-power", 
                               team == "Richmond" ~ "richmond-tigers", 
                               team == "St Kilda" ~ "st-kilda-saints", 
                               team == "Sydney" ~ "sydney-swans", 
                               team == "West Coast" ~ "west-coast-eagles", 
                               team == "Western Bulldogs" ~ "western-bulldogs", 
                               TRUE ~ "")
  return(team_abr)
}

rlang::env_unlock(env = asNamespace('fitzRoy'))
rlang::env_binding_unlock(env = asNamespace('fitzRoy'))
assign('get_team_abrev_footywire', get_team_abrev_footywire, envir = asNamespace('fitzRoy'))
rlang::env_binding_lock(env = asNamespace('fitzRoy'))
rlang::env_lock(asNamespace('fitzRoy'))


for (i in 1:18){  
  
temp <- fetch_player_details_footywire(teamList[i], current = TRUE) %>% mutate(Team = teamList[i])

if (i == 1) { playerDetails_raw <- temp }
if (i > 1) { playerDetails_raw <- rbind(playerDetails_raw,temp) }

}

playerDetails <- playerDetails_raw %>% 
  mutate(Forward = case_when(Position_1 == "Forward" ~ "*", Position_2 == "Forward" ~ "*", TRUE ~ ""),
         Age = stringr::str_extract(Age, '\\w*'),
         Name = paste0(first_name," ",surname),
         Draft = "") %>% 
  select(No,Name,Team)





