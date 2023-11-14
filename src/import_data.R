## Import data

library(pacman)
p_load(haven)

# Import data ----

## 1. Objective Prior and Posterior Beliefs ----
data.bsr.qsr <- read.csv("data/data-bsr-qsr.csv")

## 2. Subjective Beliefs ----
data.nv <- read.csv("data/data-nv.csv")

## 3. 3. Incentives Only ----
data.incentives.only <- read.csv("data/data-incentives-only.csv")

## 4. Holt and Smith 2016
read_dta("data/data-HoltSmith2016.dta")

# for the graph sizes ----
g = 1.618034
s = 5