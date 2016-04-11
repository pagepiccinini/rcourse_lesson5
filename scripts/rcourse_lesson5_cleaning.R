## LOAD PACKAGES ####
library(dplyr)
library(purrr)


## READ IN DATA ####
# Full data on election results
data_election_results = list.files(path = "data/elections", full.names = T) %>%
  # Run read.table call on all files
  map(read.table, header = T, sep = "\t") %>%
  # Combine all data frames into a single data frame by row
  reduce(rbind)

# Read in extra data about specific elections
data_elections = read.table("data/rcourse_lesson5_data_elections.txt", header=T, sep="\t")

# Read in extra data about specific states
data_states = read.table("data/rcourse_lesson5_data_states.txt", header=T, sep="\t")

# See how many states in union versus confederacy
xtabs(~civil_war, data_states)


## CLEAN DATA ####
# Make data set balanced for Union and Confederacy states
data_states_clean = data_states %>%
  # Drop any data from states that were not in the US during the Civil War
  filter(!is.na(civil_war)) %>%
  # Drop any data besides the first 11 states in the Union or Confederacy based on date of US entry
  group_by(civil_war) %>%
  arrange(order_enter) %>%
  filter(row_number() <= 11) %>%
  ungroup()

# Double check balanced for 'civil_war' variable
xtabs(~civil_war, data_states_clean)

# Combine three data frames
data_clean = data_election_results %>%
  # Combine with election specific data
  inner_join(data_elections) %>%
  # Combine with state specific data
  inner_join(data_states_clean) %>%
  # Drop unused states
  mutate(state = factor(state))

# Double check independent variables are balanced
xtabs(~incumbent_party+civil_war, data_clean)



