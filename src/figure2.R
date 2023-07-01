# Replication of Danz et al 2022 ----
# Dario Trujano-Ochoa

rm(list=ls())

library(pacman)
p_load(tidyverse)

# import data
source("src/import_data.R")

data.bsr.qsr <-
data.bsr.qsr %>%
  mutate(false_report = (belief1 != pur))

data.bsr.qsr %>% 
  filter(scoringrule == 1, 
         treatment == 1) %>% 
  group_by(period) %>% 
  summarise(prop_false_report = mean(false_report))

data.bsr.qsr %>% 
  filter(scoringrule == 1, 
         treatment == 1) %>% 
  group_by(pur) %>% 
  summarise(prop_false_report = mean(false_report))
