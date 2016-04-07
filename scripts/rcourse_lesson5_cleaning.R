## LOAD PACKAGES ####
library(dplyr)
library(purrr)


## READ IN DATA ####
# Full data on election results
data_election_results = list.files(path = "data/elections", full.names = T) %>%
  map(read.table, header = T, sep = "\t") %>%
  reduce(rbind)

# Read in extra data about specific elections
data_elections = read.table("data/rcourse_lesson5_data_elections.txt", header=T, sep="\t")

# Read in extra data bout specific states
data_states = read.table("data/rcourse_lesson5_data_states.txt", header=T, sep="\t")

# See how many states in union versus confederacy
xtabs(~civil_war, data_states)


## CLEAN DATA ####
# Drop union states besides the first 11 that entered the union
data_states_clean = data_states %>%
  filter(!is.na(civil_war)) %>%
  group_by(civil_war) %>%
  arrange(order_enter) %>%
  filter(row_number() <= 11) %>%
  ungroup()

# Double check balanced for 'civil_war' variable
xtabs(~civil_war, data_states_clean)

# Combine three data sets
data_clean = data_election_results %>%
  inner_join(data_elections) %>%
  inner_join(data_states_clean) %>%
  mutate(state = factor(state))

# Double check all of numbers are balanced
xtabs(~year+civil_war, data_clean)
xtabs(~incumbent_party+civil_war, data_clean)


## CLEAN DATA - UNBALANCED DATA SET ####
# Drop union states besides the first 11 that entered the union
data_states_unbal_clean = data_states %>%
  filter(!is.na(civil_war))

# Double check balanced for 'civil_war' variable
xtabs(~civil_war, data_states_unbal_clean)

# Combine three data sets
data_unbal_clean = data_election_results %>%
  inner_join(data_elections) %>%
  inner_join(data_states_unbal_clean) %>%
  mutate(state = factor(state))

# Double check all of numbers are balanced
xtabs(~year+civil_war, data_unbal_clean)
xtabs(~incumbent_party+civil_war, data_unbal_clean)









