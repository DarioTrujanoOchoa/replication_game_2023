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

# calculate rates of false reports
data.bsr.qsr <-
  data.bsr.qsr %>%
  mutate(false_report = (belief1 != pur))

# data for BSR (elicitation method) and information treatment
bsr_info <-
  data.bsr.qsr %>% 
  filter(scoringrule == 1, 
         treatment == 1)
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

ggsave("results/all_treatments_periods.pdf",
       width=s*g, height=s, pointsize = s)

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
  xlab("Treatment") +
  ylab("Fraction of false reports") +
  theme_minimal_hgrid() +
  scale_fill_discrete(name = "Prior") 

ggsave("results/all_treatments_priors.pdf",
       width=s*g, height=s, pointsize = s)

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

ggsave("results/all_treatments_periods_round_one.pdf",
       width=s*g, height=s, pointsize = s)

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
  xlab("Treatment") +
  ylab("Fraction of false reports") +
  theme_minimal_hgrid() +
  scale_fill_discrete(name = "Prior") 

ggsave("results/all_treatments_priors_round_one.pdf",
       width=s*g, height=s, pointsize = s)


