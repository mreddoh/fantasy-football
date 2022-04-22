
# REMOVE THOSE PESKY "WARNINGS"...
options(dplyr.summarise.inform=F)

# LOAD PACKAGES
library(tidyverse)
library(fitzRoy)
library(lubridate)
library(here)
library(stringdist)

ssn = 2022

load(here("data",paste0("squadLists_",ssn,".Rdata")))

draftSelections <- read.csv(here("data","draft.csv"), na.strings = "") %>% 
  rename("SelectionOrder"="Pick","Selection"="Player") %>% 
  select(SelectionOrder,Coach,Selection,Team)

draftSelections <- draftSelections %>% mutate(Team = replace_teams(Team),
                                              Selection = ifelse(is.na(Team)==T,replace_teams(Selection),Selection))

unmatched <- squadLists %>% 
  unique() %>% 
  anti_join(draftSelections, ., by=c("Selection"="Player","Team")) %>% 
  filter(is.na(Team)==F)
