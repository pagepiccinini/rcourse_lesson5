## READ IN DATA ####
source("scripts/rcourse_lesson5_cleaning.R")


## LOAD PACKAGES ####
library(tidyr)
library(ez)


## ORAGNIZE DATA ####
# Make data for statistics
data_stats = data_clean %>%
  # Change order of variable
  mutate(civil_war = factor(civil_war, levels = c("union", "confederacy"))) %>%
  # Average over years
  group_by(state, incumbent_party, civil_war) %>%
  summarise(perc_incumbent_mean = mean(perc_votes_incumbent, na.rm = T)) %>%
  ungroup()

# Check if incumbent party is within-state
xtabs(~state+incumbent_party, data_stats)

# Check if civil war is within-state
xtabs(~state+civil_war, data_stats)

# Prepare data for t-test
data_union_stats = data_stats %>%
  # Only include Union states
  filter(civil_war == "union") %>%
  # Spread out dependent variable into two columns
  spread(incumbent_party, perc_incumbent_mean)

data_confederacy_stats = data_stats %>%
  # Only include Confederacy states
  filter(civil_war == "confederacy") %>%
  # Spread out dependent variable into two columns
  spread(incumbent_party, perc_incumbent_mean)

data_democrat_stats = data_stats %>%
  # Only include Democrat incumbents
  filter(incumbent_party == "democrat")

data_republican_stats = data_stats %>%
  # Only include Republican incumbents
  filter(incumbent_party == "republican")
  

## BUILD MODELS ####
# ANOVA (base R)
incumbent.aov = aov(perc_incumbent_mean ~ incumbent_party * civil_war + Error(state/incumbent_party), data = data_stats)

incumbent.aov_sum = summary(incumbent.aov)
incumbent.aov_sum

# ezANOVA
incumbent.ezanova = ezANOVA(data.frame(data_stats),
                            dv = perc_incumbent_mean,
                            wid = state,
                            within = incumbent_party,
                            between = civil_war,
                            type = 3)

incumbent.ezanova


## FOLLOW-UP T-TESTS ####
# Effect of incumbent party, separated by civil war
incumbent_union.ttest = t.test(data_union_stats$democrat, data_union_stats$republican, paired = T)
incumbent_union.ttest

incumbent_confederacy.ttest = t.test(data_confederacy_stats$democrat, data_confederacy_stats$republican, paired = T)
incumbent_confederacy.ttest

# Effect of incumbent party, separated by civil war
incumbent_democrat.ttest = t.test(perc_incumbent_mean ~ civil_war, paired = F, data = data_democrat_stats)
incumbent_democrat.ttest

incumbent_republican.ttest = t.test(perc_incumbent_mean ~ civil_war, paired = F, data = data_republican_stats)
incumbent_republican.ttest


