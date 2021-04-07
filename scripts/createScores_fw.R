
# REMOVE THOSE PESKY "WARNINGS"...
options(dplyr.summarise.inform=F)

# LOAD PACKAGES
library(tidyverse)
library(fitzRoy)
library(lubridate)
library(here)
library(stringdist)

load(here("data","squadLists.Rdata"))

draftSelections <- read.csv(here("data","draft.csv"), na.strings = "") %>% 
  rename("SelectionOrder"="Pick","Selection"="Player") %>% 
  select(SelectionOrder,Coach,Selection,Team)

draftSelections <- draftSelections %>% mutate(Team = replace_teams(Team),
                                              Selection = ifelse(is.na(Team)==T,replace_teams(Selection),Selection))

#TRADES
trade1 <- c("2021-03-25","Thomas","Mitch Lewis","Orazio Fantasia","Port Adelaide")
trade2 <- c("2021-03-25","Thomas","Jake Stringer","Sam Reid","Sydney")
trade3 <- c("2021-03-25","Chantel","Marcus Bontempelli","Isaac Heeney","Sydney")
trade4 <- c("2021-03-25","Michael","Ben Brown","Tom McDonald","Melbourne")
trade5 <- c("2021-03-25","Lachie","Nick Larkey","Lance Franklin","Sydney")

trade6 <- c("2021-04-01","Chantel","Marc Murphy","Josh Bruce","Footscray")
trade7 <- c("2021-04-01","Peter","Cam Rayner","Shane McAdam","Adelaide")
trade8 <- c("2021-04-01","Marcus","Jack Martin","Michael Gibbons","Carlton")
trade9 <- c("2021-04-01","Cath","Jamie Cripps","Logan McDonald","Sydney")
trade10 <- c("2021-04-01","Cath","Gary Rohan","Marcus Bontempelli","Footscray")



Trades <- as.data.frame(do.call(rbind, mget(ls()[substr(ls(),1,5)=="trade"])),stringsAsFactors = FALSE) %>% 
  mutate(V5 = replace_teams(V5)) %>% arrange(V1)

names(Trades) = c("tradeDt","coach","out","in","team")

rm(list=ls(pattern="trade"))

liveScores_Temp <- fetch_player_stats(season = 2021, source = "footywire")

maxRound <- liveScores_Temp %>% 
  mutate(Round = as.integer(regmatches(Round, gregexpr("[[:digit:]]+", Round)))) %>% 
  summarise(max(Round)) %>% pull()
         
roundDetails <- fetch_fixture(season = 2021, source = "footywire") %>%
  mutate(Season = year(Date),
         Date = as.Date(Date)) %>% 
  group_by(Season, Round) %>% 
  summarise(roundStart = min(Date))


if(exists("Trades") == T) {
  
    for (r in 1:nrow(roundDetails)){  
      
      if(r == 1) { tempTrades <- Trades }
      
      minDate <- roundDetails %>% filter(Round == r) %>% pull()
      
      roundTrades <- tempTrades %>% filter(tradeDt <= minDate)
      
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

if(exists("Trades") == F) {
  
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
  mutate(correct = stringdist(Player.x, Player.y, method = c("lv"))) %>% 
  group_by(Round, Player.x, Team) %>% 
  slice(which.min(correct)) %>%
  ungroup() %>% 
  select(Round, Player.y, Team, Points) %>% 
  left_join(Teams %>% filter(is.na(Team)==F),.,by=c("Round","Selection"="Player.y","Team"))

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
  summarise(Points = sum(Points))

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
  mutate(correct = stringdist(Player.x, Player.y, method = c("lv"))) %>% 
  group_by(Round, Player.x) %>% 
  slice(which.min(correct)) %>%
  ungroup() %>% 
  select(Player.y, Points) %>% 
  left_join(.,currentTeams %>% filter(is.na(Team)==F),by=c("Player.y"="Selection")) %>% 
  filter(is.na(Coach)==T) %>% 
  rename(Player = Player.y) %>% 
  group_by(Player) %>% 
  summarise(Played = n(),
            Points = sum(Points)) %>% 
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
  unique() %>% 
  left_join(., currentTeams, by=c("Player"="Selection","Team")) %>% 
  mutate(Status = ifelse(is.na(Coach)==F, paste0("Not Available (",Coach,")"), "Free Agent (Available)")) %>% 
  select(Player, Team, Status)
