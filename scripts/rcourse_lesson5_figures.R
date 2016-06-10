## READ IN DATA ####
source("scripts/rcourse_lesson5_cleaning.R")


## LOAD PACKAGES ####
library(ggplot2)


## ORGANIZE DATA ####
data_figs = data_clean %>%
  # Change order and text of labels
  mutate(civil_war = factor(civil_war, levels = c("union", "confederacy"),
                                       labels = c("Union", "Confederacy"))) %>%
  # Change order and text of labels
  mutate(incumbent_party = factor(incumbent_party, levels = c("democrat", "republican"),
                                                   labels = c("Democrat", "Republican")))

# Data averaged over years but not states
data_figs_state_sum = data_figs %>%
  # Get percentages of incumbent vote for each state by independent variables 
  group_by(state, incumbent_party, civil_war) %>%
  summarise(perc_incumbent_mean = mean(perc_votes_incumbent, na.rm = T)) %>%
  ungroup()

# Data averaged over year and states for barplot
data_figs_sum = data_figs_state_sum %>%
  # Get percentages of incumbent vote independent variables
  group_by(incumbent_party, civil_war) %>%
  summarise(mean = mean(perc_incumbent_mean, na.rm = T),
            sd = sd(perc_incumbent_mean, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  # Create variable for standard error
  mutate(se = sd / sqrt(n)) %>%
  # Create variable for top of error bar
  mutate(se_high = mean + se) %>%
  # Create variable for bottomf of error bar
  mutate(se_low = mean - se)


## MAKE FIGURES ####
# Histogram of full data set
incumbent_histogram_full.plot = ggplot(data_figs, aes(x = perc_votes_incumbent,
                                                      fill = incumbent_party)) +
  geom_histogram(bins = 10) +
  # Split the data to make separate histograms
  facet_grid(incumbent_party ~  civil_war) +
  # Manually set the colors for the histograms
  scale_fill_manual(values = c("blue", "red")) +
  # Add a title
  ggtitle("Percentage of Votes for Incumbent\nby Country in Civil War and Party of Incumbent") +
  # Customize the x-axis
  xlab("Civil War country and incumbent party") +
  # Customize the y-axis
  ylab("Count") +
  # Remove dark background
  theme_classic() +
  # Additional paramaters for displaying plot
  theme(text=element_text(size=18), title=element_text(size=18),
        legend.position="none",
        strip.background = element_rect(color="white", fill="white"))

# Write figure to a pdf in the 'figures' folder
pdf("figures/incumbent_histogram_full.pdf")
incumbent_histogram_full.plot
# Close pdf call
dev.off()

# Histogram of data averaged over years
incumbent_histogram_sum.plot = ggplot(data_figs_state_sum, aes(x = perc_incumbent_mean,
                                                               fill = incumbent_party)) +
  geom_histogram(bins = 10) +
  # Split the data to make separate histograms
  facet_grid(incumbent_party ~  civil_war) +
  # Manually set the colors for the histograms
  scale_fill_manual(values = c("blue", "red")) +
  # Add a title
  ggtitle("Percentage of Votes for Incumbent\nby Country in Civil War and Party of Incumbent") +
  # Customize the x-axis
  xlab("Civil War country and incumbent party") +
  # Customize the y-axis
  ylab("Count") +
  # Remove dark background
  theme_classic() +
  # Additional paramaters for displaying plot
  theme(text=element_text(size=18), title=element_text(size=18),
        legend.position="none",
        strip.background = element_rect(color="white", fill="white"))

# Write figure to a pdf in the 'figures' folder
pdf("figures/incumbent_histogram_sum.pdf")
incumbent_histogram_sum.plot
# Close pdf call
dev.off()

# Boxplot
incumbent_boxplot.plot = ggplot(data_figs_state_sum, aes(x = civil_war, y = perc_incumbent_mean,
                                                         fill = incumbent_party)) +
  geom_boxplot() +
  # Set y-axis to range from 0 to 100
  ylim(0, 100) +
  # Add line for chance (50%)
  geom_hline(yintercept = 50) +
  # Manually set the colors for the boxes
  scale_fill_manual(values = c("blue", "red")) +
  # Add a title
  ggtitle("Percentage of Votes for Incumbent\nby Country in Civil War and Party of Incumbent") +
  # Customize the x-axis
  xlab("Civil War country") +
  # Customize the y-axis
  ylab("Percentage of vote for incumbent") +
  # Get rid of title for legend
  labs(fill = "") +
  # Remove dark background
  theme_classic() +
  # Additional paramaters for displaying plot
  theme(text=element_text(size=18), title=element_text(size=18),
        legend.position="top")

# Write figure to a pdf in the 'figures' folder
pdf("figures/incumbent_boxplot.pdf")
incumbent_boxplot.plot
# Close pdf call
dev.off()

# Barplot
incumbent_barplot.plot = ggplot(data_figs_sum, aes(x = civil_war, y = mean,
                                                   fill = incumbent_party)) +
  geom_bar(stat = "identity", position = "dodge") +
  # Add error bars to plot
  geom_errorbar(aes(ymin = se_low, ymax = se_high),
                width = 0.2,
                position = position_dodge(0.9)) +
  # Set y-axis to range from 0 to 100
  ylim(0, 100) +
  # Add line for chance (50%)
  geom_hline(yintercept = 50) +
  # Manually set the colors for the boxes
  scale_fill_manual(values = c("blue", "red")) +
  # Add a title
  ggtitle("Percentage of Votes for Incumbent\nby Country in Civil War and Party of Incumbent") +
  # Customize the x-axis
  xlab("Civil War country") +
  # Customize the y-axis
  ylab("Percentage of vote for incumbent") +
  # Get rid of title for legend
  labs(fill = "") +
  # Remove dark background
  theme_classic() +
  # Additional paramaters for displaying plot
  theme(text=element_text(size=18), title=element_text(size=18),
        legend.position="top")

# Write figure to a pdf in the 'figures' folder
pdf("figures/incumbent_barplot.pdf")
incumbent_barplot.plot
# Close pdf call
dev.off()






