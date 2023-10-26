
rm(list = ls(all.names = TRUE))
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(readr)
library(haven)



# Set Working Directory
setwd("C:/Users/gagyeah/OneDrive - University of Arkansas/Uark/Summer 2023/Replication/Replication Package/data")  # Set base directory

#-----------------------------------------------------------------------
#### Generate core variables 
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


#-----------------------------------------------------------------------
#### Figure 2 Panel A
#-----------------------------------------------------------------------
library(ggplot2)

# Select prior reports from BSR Information treatment
data_panel_a <- data[Main_data.df$scoringrule == 1 & data$treatment == 1, ] ### 
data_panel_a <- data_panel_a[, c("period", "false_bool")]

# Get period averages and confidence intervals
data_panel_a_summary.df <- aggregate(false_bool ~ period, data = data_panel_a, FUN = function(x) {
  c(mean = mean(x), sd = sd(x), count = length(x))
})

write.csv(data_panel_a_summary.df, "Panel1_sum.csv") ### The format was acting up

data_panel_a_summary1 <- read.csv("Panel1_sum.csv", header = TRUE)


data_panel_a_summary1$up <- data_panel_a_summary1$false_bool.mean + qt(0.025, data_panel_a_summary1$false_bool.count - 1) * (data_panel_a_summary1$false_bool.sd / sqrt(data_panel_a_summary1$false_bool.count))

data_panel_a_summary1$low <- data_panel_a_summary1$false_bool.mean - qt(0.025, data_panel_a_summary1$false_bool.count - 1) * (data_panel_a_summary1$false_bool.sd / sqrt(data_panel_a_summary1$false_bool.count))

# Create the figure
figure2_A <- ggplot(data_panel_a_summary1, aes(x = period, y = false_bool.mean, ymin = low, ymax = up)) +
  geom_point(shape = 3, size = 2) +
  geom_line()+
  geom_errorbar(width = 0.1) +
  theme_minimal() +
  labs(
    x = "Period",
    y = "Fraction of false reports",
    title = "Figure 2 Panel A: Fraction of False Reports by Period"
  )

figure2_A + scale_x_continuous(breaks=seq(0,10,by=1))

#-----------------------------------------------------------------------
#### Figure 2 Panel B
#-----------------------------------------------------------------------
library(ggplot2)

# Select prior reports from BSR Information treatment
data_panel_b <- data[Main_data.df$scoringrule == 1 & data$treatment == 1, ] ### 
data_panel_b <- data_panel_b[, c("treatment", "false_bool", "pur")]

table(data_panel_b$treatment,data_panel_b$pur)

# Get period averages and confidence intervals
data_panel_b_summary.df <- aggregate(false_bool ~ treatment + pur, data = data_panel_b, FUN = function(x) {
  c(mean = mean(x), sd = sd(x), count = length(x))
})

View(data_panel_b_summary.df)
write.csv(data_panel_b_summary.df, "Panel2_sum.csv") ### The format was acting up

data_panel_b_summary1 <- read.csv("Panel2_sum.csv", header = TRUE)
View(data_panel_b_summary1)


data_panel_b_summary1$up <- data_panel_b_summary1$false_bool.mean + qt(0.025, data_panel_b_summary1$false_bool.count - 1) * (data_panel_b_summary1$false_bool.sd / sqrt(data_panel_b_summary1$false_bool.count))

data_panel_b_summary1$low <- data_panel_b_summary1$false_bool.mean - qt(0.025, data_panel_b_summary1$false_bool.count - 1) * (data_panel_b_summary1$false_bool.sd / sqrt(data_panel_b_summary1$false_bool.count))




# Create the figure
figure2_B <- ggplot(data_panel_b_summary1, aes(x = as.factor(pur), y = false_bool.mean, ymin = low, ymax = up)) +
  geom_bar(stat = "identity", fill = "gray", color = "white", width = 0.7) +
  geom_errorbar(width = 0.1) +
  theme_minimal() +
  labs(
    x = "Known prior of Red Urn",
    y = "Fraction of false reports",
    title = "Figure 2 Panel B"
  ) 

figure2_B
+
  scale_x_discrete(labels = c("0.2", "0.3", "0.5", "0.7", "0.8"))

ggsave("figures/pdf/Figure2_B.pdf", plot = figure2_B, width = 6, height = 6)



                 