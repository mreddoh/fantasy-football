
# REMOVE THOSE PESKY "WARNINGS"...
options(dplyr.summarise.inform=F)

# LOAD PACKAGES
library(tidyverse)
library(fitzRoy)
library(lubridate)
library(here)
library(stringdist)

ssn = 2023

load(here("data",paste0("squadLists_",ssn,".Rdata")))

draftSelections <- read.csv(here("data","draft.csv"), na.strings = "") %>% 
  rename("SelectionOrder"="Pick","Selection"="Player") %>% 
  select(SelectionOrder,Coach,Selection,Team)

draftSelections <- draftSelections %>% mutate(Team = replace_teams(Team),
                                              Selection = ifelse(is.na(Team)==T,replace_teams(Selection),Selection))

#TRADES (in form: tradeDt, coach, out, in, team)
Trades <- read.csv(here("data","trades.csv"), na.strings = "", check.names = F) %>% mutate_all(.,trimws)

liveScores_Temp <- fetch_player_stats(season = ssn, source = "footywire")

# maxRound <- liveScores_Temp %>% 
#   mutate(Round = as.integer(regmatches(Round, gregexpr("[[:digit:]]+", Round)))) %>% 
#   summarise(max(Round)) %>% pull()
         
roundDetails <- fetch_fixture(season = ssn, source = "footywire") %>%
  mutate(Season = year(Date),
         Date = as.Date(Date)) %>% 
  group_by(Season, Round) %>% 
  summarise(roundStart = min(Date))

maxRound <- roundDetails %>% filter(roundStart <= today()) %>% summarise(max(Round)) %>% pull()

if(exists("Trades") == T) {
  
    for (r in 1:maxRound){  
      
      if(r == 1) { tempTrades <- Trades }
      
      minDate <- roundDetails %>% filter(Round == r) %>% mutate(roundStart = as.character(roundStart)) %>% pull(roundStart)
      if(r > 1) { prvDate <- roundDetails %>% filter(Round == r - 1) %>% mutate(roundStart = as.character(roundStart)) %>% pull(roundStart)}
      if(r == 1) { prvDate <- "2022-01-01" }
        
      roundTrades <- tempTrades %>% filter(tradeDt <= minDate & tradeDt > prvDate)
      
      tempTrades <- setdiff(tempTrades, roundTrades)
      
      #APPLY THE TRADES
      if(r == 1) { tempTeam <- draftSelections %>% select(Coach, Selection, Team) }
      
      tempTeam <- tempTeam %>% 
        left_join(.,roundTrades,by=c("Selection"="out", "Coach"="coach")) %>% 
        mutate(tmp = ifelse(is.na(`in`)==FALSE, `in`, Selection),
               tmpTeam = ifelse(is.na(`in`)==FALSE, `team`, Team),
               Round = r) %>% 
        select(Round, Coach, tmp, tmpTeam) %>% 
        rename("Selection" = "tmp", "Team" = "tmpTeam")
      
      if(r == 1) { Teams <- tempTeam }
      if(r > 1) { Teams <- rbind(Teams,tempTeam) }
      
    }

  currentTeams <- Teams %>% filter(Round == max(Teams$Round))
  
}

if(exists("Trades") == F | nrow(Trades) == 0) {
  
  if(length(maxRound)==0) { maxRound <- 1 }
  
  Teams <- draftSelections %>% 
    left_join(.,tibble(Round = 1:maxRound),by=character())
  
  currentTeams <- draftSelections

}

playerScore <- liveScores_Temp %>% 
  select(Round,Player,Team,G) %>% 
  mutate(Team = replace_teams(Team),
         Round = as.integer(regmatches(Round, gregexpr("[[:digit:]]+", Round)))) %>% 
  rename(Points = G) %>% 
  left_join(., squadLists, by="Team") %>% 
  mutate(Player.y = gsub("[a-z]*(?=\\-)","",Player.y, perl=TRUE)) %>% 
  mutate(correct = stringdist(Player.x, Player.y, method = c("lv"))) %>% 
  #filter(correct <= 7) %>% 
  group_by(Round, Player.x, Team) %>% 
  slice(which.min(correct)) %>%
  ungroup() %>% 
  select(Round, Player.y, Team, Points) %>% 
  left_join(Teams %>% filter(is.na(Team)==F),.,by=c("Round","Selection"="Player.y","Team")) %>% 
  group_by(Round, Coach) %>% 
  mutate(best = rank(-Points,ties.method = "random")) %>% 
  ungroup() %>% 
  filter(best != 6) %>% 
  select(-best)

playerScore.current <- liveScores_Temp %>% 
  select(Round,Player,Team,G) %>% 
  mutate(Team = replace_teams(Team),
         Round = as.integer(regmatches(Round, gregexpr("[[:digit:]]+", Round)))) %>% 
  rename(Points = G) %>% 
  left_join(., squadLists, by="Team") %>% 
  mutate(Player.y = gsub("[a-z]*(?=\\-)","",Player.y, perl=TRUE)) %>% 
  mutate(correct = stringdist(Player.x, Player.y, method = c("lv"))) %>% 
  #filter(correct <= 7) %>% 
  group_by(Round, Player.x, Team) %>% 
  slice(which.min(correct)) %>%
  ungroup() %>% 
  select(Round, Player.y, Team, Points) %>% 
  left_join(Teams %>% filter(is.na(Team)==F),.,by=c("Round","Selection"="Player.y","Team")) %>% filter(Round == maxRound)

teamScore <- liveScores_Temp %>% 
  group_by(Round, Opposition) %>% 
  summarise(G = sum(G)) %>% 
  mutate(Team = replace_teams(Opposition),
         Round = as.integer(regmatches(Round, gregexpr("[[:digit:]]+", Round))),
         Points = case_when(G < 6 ~ 8, 
                            G < 10 ~ 4, 
                            G < 15 ~ 2, 
                            G > 20 ~ -2,
                            TRUE ~ 0)) %>% 
  select(Round, Team, Points) %>% 
  left_join(Teams %>% filter(is.na(Team)==T),.,by=c("Round","Selection"="Team"))

Scores <- bind_rows(playerScore, teamScore) %>% 
  filter(is.na(Points)==F) %>% 
  mutate(Points = ifelse(is.na(Points)==T,0,Points)) %>% 
  group_by(Round, Coach) %>% 
  summarise(Points = sum(Points)) %>% 
  ungroup()

Ladder <- bind_rows(playerScore, teamScore) %>% 
  filter(is.na(Points)==F) %>% 
  mutate(Points = ifelse(is.na(Points)==T,0,Points)) %>% 
  group_by(Coach) %>% 
  summarise(Points = sum(Points))

top_players <- liveScores_Temp %>% 
  select(Round,Player,Team,G) %>% 
  group_by(Player) %>% 
  summarise(Played = n(),
            Points = sum(G)) %>% 
  arrange(-Points) %>% 
  head(10)

top_teams <- liveScores_Temp %>% 
  group_by(Round, Opposition) %>% 
  summarise(G = sum(G)) %>% 
  mutate(Team = replace_teams(Opposition),
         Round = as.integer(regmatches(Round, gregexpr("[[:digit:]]+", Round))),
         Points = case_when(G < 6 ~ 8, 
                            G < 10 ~ 4, 
                            G < 15 ~ 2, 
                            G > 20 ~ -2,
                            TRUE ~ 0)) %>% 
  select(Team, Points) %>% 
  group_by(Team) %>% 
  summarise(Points=sum(Points)) %>% 
  rename(`Team Defence` = "Team") %>% 
  arrange(-Points) %>% 
  head(10)

top_fa_players <- liveScores_Temp %>% 
  select(Round,Player,Team,G) %>% 
  mutate(Team = replace_teams(Team),
         Round = as.integer(regmatches(Round, gregexpr("[[:digit:]]+", Round)))) %>% 
  rename(Points = G) %>% 
  left_join(., squadLists, by="Team") %>% 
  mutate(Player.y = gsub("[a-z]*(?=\\-)","",Player.y, perl=TRUE)) %>% 
  mutate(correct = stringdist(Player.x, Player.y, method = c("lv"))) %>% 
  group_by(Round, Player.x, Team) %>% 
  slice(which.min(correct)) %>%
  ungroup() %>% 
  select(Player.y, Points, Team) %>% 
  left_join(.,currentTeams %>% filter(is.na(Team)==F),by=c("Player.y"="Selection","Team")) %>% 
  filter(is.na(Coach)==T) %>% 
  rename(Player = Player.y) %>% 
  group_by(Player) %>% 
  summarise(Played = n(),
            Points = sum(Points),
            PointsPerGame = Points / Played) %>% 
  select(Player,Played,Points) %>% 
  arrange(-Points) %>% 
  head(10)

top_fa_teams <- liveScores_Temp %>% 
  group_by(Round, Opposition) %>% 
  summarise(G = sum(G)) %>% 
  mutate(Team = replace_teams(Opposition),
         Round = as.integer(regmatches(Round, gregexpr("[[:digit:]]+", Round))),
         Points = case_when(G < 6 ~ 8, 
                            G < 10 ~ 4, 
                            G < 15 ~ 2, 
                            G > 20 ~ -2,
                            TRUE ~ 0)) %>% 
  select(Team, Points) %>% 
  ungroup() %>% 
  group_by(Team) %>% 
  summarise(Points = sum(Points)) %>% 
  left_join(.,currentTeams %>% filter(is.na(Team)==T),by=c("Team"="Selection")) %>% 
  filter(is.na(Coach)==T) %>%
  arrange(-Points) %>% 
  rename(`Team Defence`=Team) %>% 
  select(`Team Defence`, Points)

playerStatus <- squadLists %>% 
  mutate(Player.link = gsub("[a-z]*(?=\\-)","",Player, perl=TRUE)) %>% 
  unique() %>% 
  left_join(., currentTeams, by=c("Player.link"="Selection","Team")) %>% 
  mutate(Status = ifelse(is.na(Coach)==F, paste0("Not Available (",Coach,")"), "Free Agent (Available)")) %>% 
  select(Player, Team, Status)


### PRINT SCORES
Scores %>% filter(Round == maxRound) %>% select(-Round) %>% arrange(-Points)
Ladder %>% arrange(-Points)
#Scores %>% filter(Round %in% c(4,5,6,7)) %>% group_by(Coach) %>% summarise(Points = sum(Points)) %>% arrange(-Points)
#Scores %>% filter(Round %in% c(8,9,10,11)) %>% group_by(Coach) %>% summarise(Points = sum(Points)) %>% arrange(-Points)
#Scores %>% filter(Round %in% c(17,18,19,20)) %>% group_by(Coach) %>% summarise(Points = sum(Points)) %>% arrange(-Points)
#Scores %>% filter(Round %in% c(21,22,23,24)) %>% group_by(Coach) %>% summarise(Points = sum(Points)) %>% arrange(-Points)
