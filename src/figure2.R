# Replication of Danz et al 2022 ----
# Dario Trujano-Ochoa

rm(list=ls())

library(pacman)
p_load(tidyverse)
p_load(ggplot2)
p_load(cowplot)

# import data
source("src/import_data.R")

# calculate rates of false reports
data.bsr.qsr <-
data.bsr.qsr %>%
  mutate(false_report = (belief1 != pur))

# data for BSR and information

data.bsr.qsr %>% 
  filter(scoringrule == 1, 
         treatment == 1) %>% 
  distinct(subjectid)

bsr_info <-
data.bsr.qsr %>% 
  filter(scoringrule == 1, 
         treatment == 1)

# there are 60 participant in BSR-info condition
bsr_info %>% distinct(subjectid)

# figure 2A
bsr_info %>% 
  group_by(period) %>% 
  summarise(prop_false_report = mean(false_report)) %>% 
  ggplot() + 
  geom_line(aes(x = period,y = prop_false_report)) +
  ylim(0,1) +
  xlab("Period") +
  ylab("Fraction of false reports") + 
  scale_x_continuous(breaks = 1:10 ) +
  theme_minimal_hgrid()

# figure 2B
bsr_info %>% 
  group_by(pur) %>% 
  summarise(prop_false_report = mean(false_report))%>% 
  ggplot() + 
  geom_col(aes(x = factor(pur),y = prop_false_report)) +
  ylim(0,1) +
  xlab("Known prior of Red Urn") +
  ylab("Fraction of false reports") +
  theme_minimal_hgrid()



# Learning for the 50% prior ----

## Assign the relative presentation order within each prior considered. ----
bsr_info <- 
  bsr_info %>% 
  group_by(subjectid,pur) %>% 
  arrange(subjectid, period) %>% 
  mutate(order_prior = 1:n()) 


# The number of rounds per prior is different for each prior.
# 50% has a larger number of repetitions.
bsr_info %>% 
  group_by(subjectid, pur) %>% 
  summarise(n_rounds = n()) %>% 
  group_by(pur) %>% 
  summarise(N_rounds = mean(n_rounds), sd(n_rounds))

# Proportion of false report for the prior 50% across order of presentation
# There was no trend, it seems that the false report rate is constant.
bsr_info %>% 
  filter(pur == 50) %>% 
  group_by(order_prior) %>% 
  summarise(prop_false_report = mean(false_report)) %>% 
  ggplot() + 
  geom_line(aes(x = order_prior,y = prop_false_report)) +
  ylim(0,1)

# Posterior Belief elicited.
# The posterior belief should be around 50, however, there is clear trend downwards
bsr_info %>% 
  filter(pur == 50) %>% 
  group_by(order_prior) %>% 
  summarise(prior_stated = mean(belief1)) %>% 
  ggplot() + 
  geom_line(aes(x = order_prior,y = prior_stated))

# Filter for the first round per prior ----
# figure 2A
bsr_info %>% 
  filter(order_prior == 1) %>% 
  group_by(period) %>% 
  summarise(prop_false_report = mean(false_report)) %>% 
  ggplot() + 
  geom_line(aes(x = period,y = prop_false_report)) +
  ylim(0,1) +
  xlab("Period") +
  ylab("Fraction of false reports") + 
  scale_x_continuous(breaks = 1:10 ) +
  theme_minimal_hgrid()

# figure 2B
bsr_info %>% 
  filter(order_prior == 1) %>% 
  group_by(pur) %>% 
  summarise(prop_false_report = mean(false_report))%>% 
  ggplot() + 
  geom_col(aes(x = factor(pur),y = prop_false_report)) +
  ylim(0,1) +
  xlab("Known prior of Red Urn") +
  ylab("Fraction of false reports") +
  theme_minimal_hgrid()

