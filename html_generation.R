
# LOAD PACKAGES ----
library(knitr)
library(here)
library(tidyverse)

# CREATE DATASETS ----
source(here("scripts","createScores_fw.R"))

### PRINT SCORES
Scores %>% filter(Round == maxRound) %>% select(-Round) %>% arrange(-Points)
Ladder %>% arrange(-Points)


# CREATE HTML FILES ----
knit(here("trades.Rhtml"), here("html","trades.html"))
knit(here("teams.Rhtml"), here("html","teams.html"))
knit(here("availability.Rhtml"), here("html","availability.html"))

knit(here("ladder.Rhtml"), here("html","ladder.html"))
knit(here("scores.Rhtml"), here("html","scores.html"))
knit(here("stats.Rhtml"), here("html","stats.html"))
knit(here("motm.Rhtml"), here("html","motm.html"))

#knit(here("draft.Rhtml"), here("html","draft.html"))
