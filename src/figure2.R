# Replication of Danz et al 2022 ----
# Figure 2
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

# data for BSR (elicitation method) and information treatment
bsr_info <-
data.bsr.qsr %>% 
  filter(scoringrule == 1, 
         treatment == 1)
# there are 60 participant in BSR-info condition
bsr_info %>% distinct(subjectid)

## figure 2A ----
period_prop_false_report <-
bsr_info %>% 
  group_by(period) %>% 
  summarise(prop_false_report = mean(false_report))

period_prop_false_report %>% 
  ggplot() + 
  geom_line(aes(x = period,y = prop_false_report)) +
  ylim(0,1) +
  xlab("Period") +
  ylab("Fraction of false reports") + 
  scale_x_continuous(breaks = 1:10 ) +
  theme_minimal_hgrid()

ggsave("results/fig2A_original.pdf")

## figure 2B ----
prior_prop_false_report <- 
bsr_info %>% 
  group_by(pur) %>% 
  summarise(prop_false_report = mean(false_report))

prior_prop_false_report %>% 
  ggplot() + 
  geom_col(aes(x = factor(pur),y = prop_false_report)) +
  ylim(0,1) +
  xlab("Known prior of Red Urn") +
  ylab("Fraction of false reports") +
  theme_minimal_hgrid()

ggsave("results/fig2B_original.pdf")

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

# Prior Belief elicited.
# The prior belief should be around 50, however, there is clear trend downwards
bsr_info %>% 
  filter(pur == 50) %>% 
  group_by(order_prior) %>% 
  summarise(prior_stated = mean(belief1)) %>% 
  ggplot() + 
  geom_line(aes(x = order_prior,y = prior_stated))

# Filter for the first round per prior ----

## figure 2A ----
# filter the first period
period_prop_false_report_r1 <-
bsr_info %>% 
  filter(order_prior == 1) %>% 
  group_by(period) %>% 
  summarise(prop_false_report_r1 = mean(false_report))
#join both porportion in a single table
period_prop_false_report_table <-
full_join(period_prop_false_report, 
          period_prop_false_report_r1) %>% 
  rename(all_rounds = prop_false_report,
         round_1 = prop_false_report_r1) %>% 
  pivot_longer(-period, 
               names_to = "rounds", 
               values_to = "prop_false_report")

period_prop_false_report_table %>% 
  ggplot() + 
  geom_line(aes(x = period,
                y = prop_false_report,
                color = rounds, 
                group = rounds)) +
  ylim(0,1) +
  labs(
    title = "Comparisson with the first round",
    x = "Period", 
    y = "Fraction of false reports") + 
  scale_x_continuous(breaks = 1:10 ) +
  theme_minimal_hgrid() +
  scale_color_discrete(name = "Rounds", 
                      breaks = c("all_rounds", 
                                "round_1"),
                      labels = c("All Rounds",
                                 "Round 1")) 


ggsave("results/fig2A_round_one.pdf")

## figure 2B ----
prior_prop_false_report_r1 <-
bsr_info %>% 
  filter(order_prior == 1) %>% 
  group_by(pur) %>% 
  summarise(prop_false_report_r1 = mean(false_report))
#join both porportion in a single table
prior_prop_false_report_table <-
  full_join(prior_prop_false_report, 
            prior_prop_false_report_r1) %>% 
  rename(all_rounds = prop_false_report,
         round_1 = prop_false_report_r1) %>% 
  pivot_longer(-pur, 
               names_to = "rounds", 
               values_to = "prop_false_report")

prior_prop_false_report_table %>% 
  ggplot() + 
  geom_col(aes(x = factor(pur),
               y = prop_false_report,
               fill = rounds),
           position=position_dodge()) +
  ylim(0,1) +
  labs(
    title = "Comparisson with the first round",
    x = "Known prior of Red Urn",
    y = "Fraction of false reports") +
  theme_minimal_hgrid() +
  scale_fill_discrete(name = "Rounds", 
                       breaks = c("all_rounds", 
                                  "round_1"),
                       labels = c("All Rounds",
                                  "Round 1")) 

ggsave("results/fig2B_round_one.pdf")

