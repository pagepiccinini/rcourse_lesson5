## LOAD PACKAGES ####
library(ggplot2)
library(maps)
library(dplyr)


## READ IN DATA ####
state_sizes = map_data("state")

state_info = read.table("test2.txt", header=T, sep="\t") %>%
  mutate(region = tolower(state))

data = inner_join(all_states, state_info)

us_empty.plot = ggplot(data, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color="black") +
  theme_bw()  +
  xlab("") +
  ylab("") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank())

pdf("us_empty.pdf")
us_empty.plot
dev.off()

us_conf.plot = ggplot(subset(data, civil_war == "confederacy"), aes(x = long, y = lat, group = group)) +
  #geom_polygon(fill = "white", color="black") +
  geom_polygon(aes(fill = civil_war), color = "black") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw()  +
  xlab("") +
  ylab("") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA), legend.position = "none")

png("us_conf.png", bg = "transparent")
us_conf.plot
dev.off()

us_union.plot = ggplot(subset(data, civil_war != "confederacy" | is.na(civil_war)), aes(x = long, y = lat, group = group)) +
  #geom_polygon(fill = "white", color="black") +
  geom_polygon(aes(fill = civil_war), color = "black") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_bw()  +
  xlab("") +
  ylab("") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.position = "none")

png("us_union.png", bg = "transparent", width=800,height=500,units="px")
us_union.plot
dev.off()


us_civilwar.plot = ggplot(data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = civil_war), color = "black") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw()  +
  xlab("") +
  ylab("") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank(), legend.position = "none")


pdf("us_civilwar.pdf")
us_civilwar.plot
dev.off()


