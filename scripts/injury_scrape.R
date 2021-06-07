
library(rvest)
library(tidyverse)
library(fitzRoy)
library(splitstackshape)

injury_page <- read_html("https://www.footywire.com/afl/footy/injury_list")

tempInjury <- injury_page %>%
  html_table()

injuryList.players <- tempInjury[[4]] %>% 
  as_tibble() %>% 
  filter(X1 != "") %>% 
  filter(X1 != "Player") %>% 
  filter(!str_detect(X1,"Players")) %>% 
  select(X1,X2,X3) %>% 
  rename(Player = X1,
         Injury = X2,
         Length = X3)
  
injuryList.teams <- tempInjury[[4]] %>% 
  as_tibble() %>% 
  filter(str_detect(X1,"Players")) %>% 
  filter(row_number()>2) %>% 
  mutate(Team = replace_teams(trimws(str_extract(X1,"^[^\\(]+"))),
         N = as.integer(str_extract(X1,"\\d+"))) %>% 
  select(Team,N) %>% 
  expandRows(., count=2, count.is.col = T) %>% 
  select(Team)

injuryList <- cbind(injuryList.players,injuryList.teams)

