
## SET PACKAGES
library("reshape2")
library("tidyverse")
library("lubridate")
library("fitzRoy")
library("here")
library("stringdist")
library("rvest")

webpage1 <- read_html("https://www.afc.com.au/teams/afl")
webpage2 <- read_html("https://www.lions.com.au/teams/afl")
webpage3 <- read_html("https://www.carltonfc.com.au/teams/afl")
webpage4 <- read_html("https://www.collingwoodfc.com.au/teams/afl")
webpage5 <- read_html("https://www.essendonfc.com.au/teams/afl")
webpage6 <- read_html("https://www.fremantlefc.com.au/teams/afl")
webpage7 <- read_html("https://www.geelongcats.com.au/teams/afl")
webpage8 <- read_html("https://www.goldcoastfc.com.au/teams/afl/players")
webpage9 <- read_html("https://www.gwsgiants.com.au/teams/afl")
webpage10 <- read_html("https://www.hawthornfc.com.au/teams/afl")
webpage11 <- read_html("https://www.melbournefc.com.au/teams/afl")
webpage12 <- read_html("https://www.nmfc.com.au/teams/afl/players")
webpage13 <- read_html("https://www.portadelaidefc.com.au/teams/afl")
webpage14 <- read_html("https://www.richmondfc.com.au/football/afl/squad")
webpage15 <- read_html("https://www.saints.com.au/afl/squad")
webpage16 <- read_html("https://www.sydneyswans.com.au/teams/afl")
webpage17 <- read_html("https://www.westcoasteagles.com.au/teams/afl")
webpage18 <- read_html("https://www.westernbulldogs.com.au/teams/afl")

aflTeams <- c(
  "Adelaide",
  "Brisbane Lions",
  "Carlton",
  "Collingwood",
  "Essendon",
  "Fremantle",
  "Geelong",
  "Gold Coast",
  "GWS",
  "Hawthorn",
  "Melbourne",
  "North Melbourne",
  "Port Adelaide",
  "Richmond",
  "St Kilda",
  "Sydney",
  "West Coast",
  "Footscray"
)

for (i in 1:18){  

  tempPlayers <- get(paste0("webpage", i)) %>%
    html_nodes("h1") %>%
    html_text() %>% 
    as.data.frame() %>% 
    rename(Player = names(.[1])) %>% 
    mutate(cnt = str_count(Player,"\\n"),
           cnt2 = str_count(Player,"\\t")) %>% 
    filter(cnt >= 3 & cnt2 == 0) %>% 
    select(-c(cnt,cnt2)) %>% 
    mutate(Player = gsub("[\t\n]","",str_squish(Player))) %>% 
    mutate(Player = trimws(gsub("Key|Midfielder|Forward|Defender|Ruck","",Player))) %>% 
    mutate(Team = aflTeams[i]) %>% 
    filter(Player != "AFLW Ladder AFLW Ladder")
  
  if(i == 1) { squadLists <- tempPlayers }
  
  if(i > 1) { squadLists <- rbind(squadLists, tempPlayers) }
  
}


# Checks

squadLists %>% group_by(Team) %>% summarise(n=n())


### Output to file.
save(squadLists, file = here("data",paste0("squadLists_",year(today()),".Rdata")))



