# Replication of Danz et al. 2022 ----
# All treatments
# Dario Trujano-Ochoa

rm(list=ls())

library(pacman)
p_load(tidyverse)
p_load(ggplot2)
p_load(cowplot)

# import data
source("src/import_data.R")

# calculate rates of false reports and Learning for the 50% prior ----

## Assign the relative presentation order within each prior considered. ----

data.bsr.qsr <-
  data.bsr.qsr %>%
  mutate(false_report = (belief1 != pur),
         treatment = factor(treatment)) %>% 
  group_by(subjectid,pur) %>% 
  arrange(subjectid, period) %>% 
  mutate(order_prior = 1:n())

## figure 2A ----
data.bsr.qsr %>% 
  filter(scoringrule == 1) %>% 
  group_by(period, treatment) %>% 
  summarise(prop_false_report = mean(false_report)) %>% 
  ggplot() + 
  geom_point(aes(x = period, 
                y = prop_false_report,
                color = treatment,
                shape = treatment)) +
  geom_line(aes(x = period, 
                 y = prop_false_report,
                 color = treatment)) +
#  ylim(0,1) +
  xlab("Period") +
  ylab("Fraction of false reports") + 
  scale_x_continuous(breaks = 1:10 ) +
  theme_minimal_hgrid()

ggsave("results/all_treatments_periods.pdf")

## figure 2B ----
data.bsr.qsr %>% 
  filter(scoringrule == 1) %>% 
  group_by(pur,treatment) %>% 
  summarise(prop_false_report = mean(false_report)) %>% 
  ggplot() + 
  geom_col(aes(x =  factor(treatment),
               y = prop_false_report,
               fill = factor(pur) ),
           position = "dodge") +
  ylim(0,1) +
  xlab("Known prior of Red Urn") +
  ylab("Fraction of false reports") +
  theme_minimal_hgrid()

ggsave("results/all_treatments_priors.pdf")

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

# Modify data to filter ----
# Posterior Belief elicited.
# The posterior belief should be around 50, however, there is clear trend downwards
bsr_info %>% 
  filter(pur == 50) %>% 
  group_by(order_prior) %>% 
  summarise(prior_stated = mean(belief1)) %>% 
  ggplot() + 
  geom_line(aes(x = order_prior,y = prior_stated))

# Filter for the first round per prior ----

bsr_treatmens_first_round <- 
  data.bsr.qsr %>% 
  filter(scoringrule == 1,
         order_prior == 1)

## figure 2A ----
bsr_treatmens_first_round %>% 
  group_by(period, treatment) %>% 
  summarise(prop_false_report = mean(false_report)) %>% 
  ggplot() + 
  geom_point(aes(x = period, 
                 y = prop_false_report,
                 color = treatment,
                 shape = treatment)) +
  geom_line(aes(x = period, 
                y = prop_false_report,
                color = treatment)) +
  #  ylim(0,1) +
  xlab("Period") +
  ylab("Fraction of false reports") + 
  scale_x_continuous(breaks = 1:10 ) +
  theme_minimal_hgrid()

ggsave("results/all_treatments_periods_round_one.pdf")

## figure 2B ----
bsr_treatmens_first_round %>% 
  group_by(pur,treatment) %>% 
  summarise(prop_false_report = mean(false_report)) %>% 
  ggplot() + 
  geom_col(aes(x =  factor(treatment),
               y = prop_false_report,
               fill = factor(pur) ),
           position = "dodge") +
  ylim(0,1) +
  xlab("Known prior of Red Urn") +
  ylab("Fraction of false reports") +
  theme_minimal_hgrid()

ggsave("results/all_treatments_priors_round_one.pdf")
