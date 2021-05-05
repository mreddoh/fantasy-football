
# LOAD PACKAGES ----
library(knitr)
library(here)
library(tidyverse)

# CREATE DATASETS ----
source(here("scripts","createScores_fw.R"))

# CREATE HTML FILES ----
knit(here("ladder.Rhtml"), here("html","ladder.html"))
knit(here("scores.Rhtml"), here("html","scores.html"))
knit(here("trades.Rhtml"), here("html","trades.html"))
knit(here("stats.Rhtml"), here("html","stats.html"))
knit(here("teams.Rhtml"), here("html","teams.html"))
knit(here("availability.Rhtml"), here("html","availability.html"))
knit(here("motm.Rhtml"), here("html","motm.html"))

