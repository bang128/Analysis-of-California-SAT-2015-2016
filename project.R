# HW3
# 1.
library(tidyverse)
setwd("/Users/bangpham/Documents/Spring 2023/COMP 293A/project")
dataset <- read_csv("SAT Report 2015-2016.csv")

# 2.
str(dataset)

# 3.
summary(dataset)

# HW4
# Since the first row is not included, I drop it
dataset <- dataset[-1,]
head(dataset)

# Since some numeric variables are intially character, I change them to numeric
dataset$AvgScrRead <- as.numeric(dataset$AvgScrRead)
dataset$AvgScrMath <- as.numeric(dataset$AvgScrMath)
dataset$AvgScrWrit <- as.numeric(dataset$AvgScrWrit)
dataset$NumGE1500 <- as.numeric(dataset$NumGE1500)
dataset$PctGE1500 <- as.numeric(dataset$PctGE1500)

dataset <- mutate(dataset, AvgScr = (AvgScrRead + AvgScrMath + AvgScrWrit)/3)
View(dataset)

# After the above change, I redo str() and summary()
str(dataset)
summary(dataset)

# 1.
# Scatterplot between average SAT and percentage of test takers 
# whose total SAT score >= 1500, colored by record type
ggplot(dataset, aes(AvgScr, PctGE1500, color = rtype)) + 
  geom_point() + geom_smooth(method=lm, color = "red") + ylim(c(0,100)) + 
  labs(title="Scatterplot: AvgScr vs PctGE1500")
ggsave("scatter_plot.png")

# 2.
ggplot(dataset, aes(cname, AvgScr)) +
  geom_boxplot() + labs(title="Boxplot: cname vs AvgScr")
ggsave("boxplot1.png")

cname_data <- summarize(group_by(dataset, cname), count = n())
ggplot(cname_data, aes(cname, count)) + 
  geom_bar(fill = "dodgerblue",stat="identity") +
  labs(title="Barchart: cname vs AvgScr")
ggsave("barchart1.png")

cname50 <- filter(summarize(grouped_data, count = n()), count >= 50)
ggplot(cname50, aes(cname, count)) + 
  geom_bar(fill = "dodgerblue",stat="identity") +
  labs(title="Barchart: cname vs AvgScr")
ggsave("barchart2.png")
# Interpretation:
# The first barchart shows that most of the counties have less than 50 schools 
# that have at least 1 students in SAT exam 2015-2016. 
# Therefore, I plot the second barchart to look at the counties with more than 
# 50 schools in the SAT exam.
# As shown in the second barchart, the county with the significantly large 
# number of schools in the SAT exam is Los Angeles (nearly 500 students)

dataset %>%
  filter(cname %in% cname50$cname) %>%
  ggplot(aes(cname, AvgScr)) + geom_boxplot() + 
  labs(title="Boxplot: cname vs AvgScr")
ggsave("boxplot2.png")
# Interpretation:
# As explained above, I am focusing on counties with more than 50 schools in 
# the SAT exam 2015-2016. Therefore, I re-make the boxplot with only these 
# schools

cor.test(dataset$NumTstTakr, dataset$AvgScr)
# Interpretation:
# The p-value is 0.2071 and correlation coefficient is around 0.03 (very small).
# Given alpha = 0.05, we do not have enough evidence to reject the null 
# hypothesis. Therefore, there is no true correlation between AvgScr and 
# NumTstTakr. In other words, we know that no matter how many test takers from 
# a certain county attends in the SAT exam, it does not affect (increase/decrease) 
# the AvgScr of that county

model <- lm(PctGE1500 ~ AvgScr, data = dataset)
summary(model)
# Interpretation:
# The p-value is very small (< 2.2e-16), so it indicates 
# that there is a correlation between AgvScr and PctGE1500
# R-squared = 0.9565, meaning that 95.65% of the variability in PctGE1500 
# is explained by AvgScr
# Standard error = 4.713, which is smalled compared to the range of PctGE1500
# (0, 100). This demonstrates that the observed values are very closed to 
# the regression line. 
# The equation of the regression line: PctGE1500 = -142.15990 + 0.38032*AvgScr


t.test(AvgScr ~ cname, 
       data = filter(dataset, cname == "Los Angeles" | cname == "San Diego"))
# Interpretation:
# p-value is 6.026e-12, which is very small, so we can reject the null 
# hypothesisindicates that there is a true difference in mean of AvgScr between 
# Los Angeles and San Diego. Specifically, the mean in Los Angeles is 450.7574, 
# while the mean in San Diego is 491.3416. This shows that although there are 
# more schools in Los Angeles that have students in the SAT exam, the average 
# SAT score in San Diego is still higher.


t.test(NumTstTakr ~ cname, 
       data = filter(dataset, cname == "Los Angeles" | cname == "San Diego"))
# Interpretation:
# This p-value is 0.7256. Given alpha = 0.05, we cannot reject the null 
# hypothesis, so there is no true difference in mean of number of test takers 
# (NumTstTakr) between Los Angeles and San Diego. Specifically, the mean in Los 
# Angeles is 374.6116, and the mean in San Diego is 313.3931. This shows that 
# although there are much more schools in Los Angeles that have students attend 
# in the SAT exam than San Diego, the average number of test takers from each 
# school of the two counties are approximately similar


