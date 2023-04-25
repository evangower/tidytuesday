library(tidyverse)
library(treemapify)
library(showtext)

# Load data
tuesdata <- tidytuesdayR::tt_load(2023, week = 17)
winners <- tuesdata$winners

# Add custom font
font_add_google("Sora", "sora")
showtext_auto()

# Summarize total wins per category by nationality
df1 <- winners %>%
  group_by(Nationality, Category) %>%
  summarise(count = n())

# Mutate total wins per nationality
df2 <- df1 %>%
  group_by(Nationality) %>%
  mutate(total_wins = sum(count))

# Plot data
ggplot(df2, aes(are = count, fill = Category, label = Nationality, subgroup = Category)) +
  geom_treemap(color = "black") +
  geom_treemap_subgroup_border(color = "black", size = 1) +
  geom_treemap_subgroup_text(place = "bottomleft", family = "sora", fontface = "bold", color = "black", alpha = 0.5, size = 20) +
  geom_treemap_text(place = "topleft", grow = TRUE, family = "sora", fontface = "bold", color = "white") +
  scale_fill_manual(values = c("dodgerblue2", "seagreen3", "lightslateblue", "#eb2266")) +
  labs(title = "London Marathon Winners by Nation",
       subtitle = "Number of wins from each nation by category in the London Marathon.\n\n The host nation United Kingdom has the most wins overall (44) and in both wheelchair events\n(16 in wm and 15 in ww). Kenya meanwhile has won the most in the mens (16) and womens (14) category.\n",
       caption = "Data: LondonMarathon R Package by Nicola Rennie | Viz: Evan Gower") +
  theme(text = element_text(font = "sora"),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(color = "grey40", size = 7),
        legend.position = "none",
        plot.margin = unit(c(0.2, 0.7, 0.2, 0.7), "cm"))