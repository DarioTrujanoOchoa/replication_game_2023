#the false rates per prior in each period

rm(list = ls(all.names = TRUE))
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(readr)
library(haven)



# Initialize environment
setwd("C:/Users/gagyeah/OneDrive - University of Arkansas/Uark/Summer 2023/Replication/Replication Package/data")  # Set base directory

#-----------------------------------------------------------------------
#### Generate core variables for BSR study
#-----------------------------------------------------------------------

# Read data
data <- read.csv("data-bsr-qsr.csv", header = TRUE)

# Create indicator for false report
#First round per prior 
#Learning effect by round of each prior including 50 (2a)
#Distance with average average 
data$false <- data$belief1 != data$pur

# Create labels for false indicator
data$false <- factor(data$false, levels = c(FALSE, TRUE), labels = c("True-report", "False-report"))

# Create indicator for centered prior
data$center <- data$pur == 50

# Create labels for centered indicator
data$center <- factor(data$center, levels = c(FALSE, TRUE), labels = c("Non-Centered", "Centred"))

# Define false report types
data$f_cent <- (data$pur < data$belief1 & data$belief1 <= 50) | (data$pur > data$belief1 & data$belief1 >= 50) #to-center
data$f_extr <- (data$pur < 50 & data$belief1 < data$pur) | (data$pur > 50 & data$belief1 > data$pur) #to-nearest-extreme
data$f_dist <- (data$pur < 50 & 50 < data$belief1) | (data$pur > 50 & 50 > data$belief1) #to-distant-extreme

data$false_type <- 0
data$false_type <- ifelse(data$false==TRUE & data$f_cent==TRUE, 1,data$false_type)
data$false_type<- ifelse(data$false==TRUE & data$f_extr==TRUE, 2, data$false_type)
data$false_type <- ifelse(data$false==TRUE & data$f_dist==TRUE, 3,data$false_type)

# Create labels for false report types
data$false_type <- factor(data$false_type,
                          levels = c(0, 1, 2, 3),
                          labels = c("True-report", "Center", "Near-extreme", "Far-extreme"))

# Create treatment categories for Table 2
data$t_cats <- NA
data$t_cats[data$treatment == 1] <- 10  # Information
data$t_cats[data$treatment == 2] <- 20  # RCL
data$t_cats[data$treatment == 3] <- 30  # No-Information
data$t_cats[data$treatment == 4 & data$period < 3] <- 41  # Feedback (t=1,2)
data$t_cats[data$treatment == 4 & data$period > 8] <- 49  # Feedback (t=9,10)
data$t_cats[data$treatment == 5] <- 50  # Description

# Create labels for treatment categories
data$t_cats <- factor(data$t_cats,
                      levels = c(10, 20, 30, 41, 49, 50),
                      labels = c("Information", "RCL", "No-Information", "Feedback (t=1,2)", "Feedback (t=9,10)", "Description"))

TreatmentList <- c(10, 20, 30, 41, 49, 50)
ComparisonTreatmentList <- c(20, 30, 41, 49, 50)  # Excluding Information

# Generate subject specific rate of prior false reports

data$false_bool <- ifelse(data$false=="True-report",0,1)
data1<- aggregate(data$false_bool, by=list(subjectid=data$subjectid), FUN="sum")
names(data1)[2] <- "num_not_correct"
data1$got_all <- ifelse(data1$num_not_correct==0,1,0)


###Join to main table 
Main_data.df<-left_join(data,data1,  by=c("subjectid"="subjectid") )



###### P=0.5 

#the false rates per prior in each period

# Select prior reports from BSR Information treatment
data_panel_a_50 <- data[Main_data.df$scoringrule == 1 & data$treatment == 1 & data$pur==50, ] ### 

data_panel_a_50 <- data_panel_a_50[, c("period", "false_bool")]

# Get period averages and confidence intervals
data_panel_a_summary_50.df <- aggregate(false_bool ~ period, data = data_panel_a_50, FUN = function(x) {
  c(mean = mean(x), sd = sd(x), count = length(x))
})

write.csv(data_panel_a_summary_50.df, "Panel1_sum_50.csv") ### The format was acting up

data_panel_a_summary1__50 <- read.csv("Panel1_sum_50.csv", header = TRUE)

View(data_panel_a_summary1__50)
data_panel_a_summary1__50$up <- data_panel_a_summary1__50$false_bool.mean + qt(0.025, data_panel_a_summary1__50$false_bool.count - 1) * (data_panel_a_summary1__50$false_bool.sd / sqrt(data_panel_a_summary1__50$false_bool.count))

data_panel_a_summary1__50$low <- data_panel_a_summary1__50$false_bool.mean - qt(0.025, data_panel_a_summary1__50$false_bool.count - 1) * (data_panel_a_summary1__50$false_bool.sd / sqrt(data_panel_a_summary1__50$false_bool.count))

# Create the figure
library(ggplot2)
figure2_A_50 <- ggplot(data_panel_a_summary1__50, aes(x = period, y = false_bool.mean, ymin = low, ymax = up)) +
  geom_point(shape = 3, size = 2) +
  geom_errorbar(width = 0.1) +
  theme_minimal() +
  labs(
    x = "Period",
    y = "Fraction of false reports for 0.50",
    title = "Figure 2 Panel A [P=0.50]"
  )

figure2_A_50 + scale_x_continuous(breaks=seq(0,10,by=1))

###### P=0.7 

#the false rates per prior in each period

# Select prior reports from BSR Information treatment
data_panel_a_70 <- data[Main_data.df$scoringrule == 1 & data$treatment == 1 & data$pur==70, ] ### 
data_panel_a_70 <- data_panel_a_70[, c("period", "false_bool")]

# Get period averages and confidence intervals
data_panel_a_summary_70.df <- aggregate(false_bool ~ period, data = data_panel_a_70, FUN = function(x) {
  c(mean = mean(x), sd = sd(x), count = length(x))
})

write.csv(data_panel_a_summary_70.df, "Panel1_sum_70.csv") ### The format was acting up

data_panel_a_summary1__70 <- read.csv("Panel1_sum_70.csv", header = TRUE)

View(data_panel_a_summary1__70)
data_panel_a_summary1__70$up <- data_panel_a_summary1__70$false_bool.mean + qt(0.025, data_panel_a_summary1__70$false_bool.count - 1) * (data_panel_a_summary1__70$false_bool.sd / sqrt(data_panel_a_summary1__70$false_bool.count))

data_panel_a_summary1__70$low <- data_panel_a_summary1__70$false_bool.mean - qt(0.025, data_panel_a_summary1__20$false_bool.count - 1) * (data_panel_a_summary1__70$false_bool.sd / sqrt(data_panel_a_summary1__70$false_bool.count))

# Create the figure
library(ggplot2)
figure2_A_70 <- ggplot(data_panel_a_summary1__70, aes(x = period, y = false_bool.mean, ymin = low, ymax = up)) +
  geom_point(shape = 3, size = 2) +
  geom_errorbar(width = 0.1) +
  theme_minimal() +
  labs(
    x = "Period",
    y = "Fraction of false reports for 0.7",
    title = "Figure 2 Panel A [P=0.7]"
  )

figure2_A_70 + scale_x_continuous(breaks=seq(0,10,by=1))

###### P=0.8 

#the false rates per prior in each period

# Select prior reports from BSR Information treatment
data_panel_a_80 <- data[Main_data.df$scoringrule == 1 & data$treatment == 1 & data$pur==80, ] ### 
data_panel_a_80 <- data_panel_a_80[, c("period", "false_bool")]

# Get period averages and confidence intervals
data_panel_a_summary_80.df <- aggregate(false_bool ~ period, data = data_panel_a_80, FUN = function(x) {
  c(mean = mean(x), sd = sd(x), count = length(x))
})

write.csv(data_panel_a_summary_80.df, "Panel1_sum_20.csv") ### The format was acting up

data_panel_a_summary1__80 <- read.csv("Panel1_sum_20.csv", header = TRUE)

View(data_panel_a_summary1__80)
data_panel_a_summary1__80$up <- data_panel_a_summary1__80$false_bool.mean + qt(0.025, data_panel_a_summary1__80$false_bool.count - 1) * (data_panel_a_summary1__80$false_bool.sd / sqrt(data_panel_a_summary1__80$false_bool.count))

data_panel_a_summary1__80$low <- data_panel_a_summary1__80$false_bool.mean - qt(0.025, data_panel_a_summary1__80$false_bool.count - 1) * (data_panel_a_summary1__80$false_bool.sd / sqrt(data_panel_a_summary1__80$false_bool.count))

# Create the figure
library(ggplot2)
figure2_A_80 <- ggplot(data_panel_a_summary1__80, aes(x = period, y = false_bool.mean, ymin = low, ymax = up)) +
  geom_point(shape = 3, size = 2) +
  geom_errorbar(width = 0.1) +
  theme_minimal() +
  labs(
    x = "Period",
    y = "Fraction of false reports for 0.8",
    title = "Figure 2 Panel A [P=0.8]"
  )

figure2_A_80 + scale_x_continuous(breaks=seq(0,10,by=1))

par(mfrow = c(2, 2))

figure2_A_20 + scale_x_continuous(breaks=seq(0,10,by=1))

figure2_A_30 + scale_x_continuous(breaks=seq(0,10,by=1))

figure2_A_50 + scale_x_continuous(breaks=seq(0,10,by=1))

figure2_A_70 + scale_x_continuous(breaks=seq(0,10,by=1))

figure2_A_80 + scale_x_continuous(breaks=seq(0,10,by=1))


###### P=0.5 

#the false rates per prior in each period

# Select prior reports from BSR Information treatment
data_panel_a_50 <- data[Main_data.df$scoringrule == 1 & data$treatment == 1 & data$pur==50, ] ### 

table(data_panel_a_50$subjectid)

data_panel_a_50 <- data_panel_a_50 %>% 
  arrange(subject_id, period) %>% 
  group_by(subject_id) %>% 
  mutate(rank = row_number())

data_panel_a_50 <- data_panel_a_50 %>% 
  arrange(subject_id, period) %>% 
  mutate(time = case_when(
    period == 1 ~ "first_time",
    period == 2 ~ "second_time",
    period == 3 ~ "third_time",
    period == 4 ~ "fourth_time",
    TRUE ~ as.character(period)  # Use the period number as the name for other periods
  ))
