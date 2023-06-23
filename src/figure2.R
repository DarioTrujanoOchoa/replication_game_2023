# Replication of Danz et al 2022 ----
# Dario Trujano-Ochoa

library(pacman)
p_load(tidyverse)

# Import data ----
data.bsr.qsr <- read.csv("data/data-bsr-qsr.csv")

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
