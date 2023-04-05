library(dplyr)
library(stringr)
library(ggplot2)
library(car)
library(rstatix)

clicks_df <- read.csv("data.csv", header=TRUE)
clicks_df$Factor <- factor(clicks_df$Factor)
clicks_df$Age <- factor(clicks_df$Age)
clicks_df$Gender <- factor(clicks_df$Gender)

#Summarize
group_by(clicks_df, Factor) %>%
  summarise(
    count = n(),
    mean = mean(Clicks, na.rm = TRUE),
    sd = sd(Clicks, na.rm = TRUE)
  )


#Anova for gender
oneway <- aov(Clicks ~ Factor, data = clicks_df)

oneway_female <- clicks_df %>%
  filter(Gender=="female") %>%
  aov(Clicks ~ Factor, data=.)

age_anova <- aov(Clicks ~ Age, data=clicks_df)
summary(age_anova)

gender_anova <- aov(Clicks ~ Gender, data=clicks_df)
summary(gender_anova)


#Anova for age
oneway_age_2534 <-clicks_df %>%
  filter(Age == "25-34") %>%
  aov(Clicks ~ Factor, data=.)


#Tukey
tukey_oneway <- TukeyHSD(oneway)
tukey_oneway_female <- TukeyHSD(oneway_female)
tukey_oneway_age_2534 <- TukeyHSD(oneway_age_2534)

#Levene's test
leveneTest(Clicks ~ Factor, data=clicks_df, center=mean)

#Welch anova
clicks_df %>%
  welch_anova_test(Clicks ~ Factor)

#Games - howell
clicks_df %>%
  filter(Age == "25-34") %>%
  games_howell_test(data=., Clicks ~ Factor, conf.level=0.95, detailed = FALSE)


