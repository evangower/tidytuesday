library(tidyverse)
library(showtext)
library(ggtext)

# Load data
tuesdata <- tidytuesdayR::tt_load('2022-01-25')
bgratings <- tuesdata$ratings

# Add custom font
font_add_google("Rubik", "rubik")
showtext_opts(dpi = 300)
showtext_auto()

# Preparing data
decade1 <- bgratings %>%
  filter(between(year, 1900, 2021)) %>%
  mutate(decade = floor(year/10)*10, decade = glue::glue("{decade}s"))

decade2 <- decade1 %>%
  group_by(decade) %>%
  summarise(n = n(), min_rating = min(average), max_rating = max(average), min_rank = min(rank), max_rank = max(rank)) %>%
  pivot_longer(min_rating:max_rating) %>%
  mutate(ylab = glue::glue("<span style = 'color:white'>**{decade}** </span> (nb={scales::comma(n, accuracy = 1)})"))

labels <- decade1 %>%
  group_by(decade) %>%
  filter(average == min(average) | average == max(average)) %>%
  left_join(decade2, by = "decade") %>%
  select(name.x, ylab, average, decade) %>%
  distinct

# Plot data
decade2 %>%
  ggplot(aes(value, ylab)) +
  geom_line(aes(group = ylab), color = "#f0c808") +
  geom_point(aes(color = name), show.legend = F, size = 2.5) +
  geom_text(data = labels, aes(average, ylab, label = name.x), size = 2.5, vjust = -1, family = "rubik", fontface = "italic", color = "white") +
  scale_color_manual(values = c("#64d079", "#fe7d0f")) +
  scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(mult = c(.05, .07))) +
  coord_cartesian(clip = "off") +
  cowplot::theme_minimal_grid(9.5, line_size = 0.3) +
  theme(text = element_text(family = "rubik"),
        plot.title = element_text(size = 12, hjust = 0.5, color = "white"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(size = 9, hjust = 0.5, margin = margin(b = 7), color = "white"),
        plot.caption = element_text(size = 4, margin = margin(t = 7), color = "white"),
        axis.title = element_text(size = 8, color = "white"),
        axis.text.x = element_text(size = 6, color = "white"),
        axis.text.y = element_markdown(size = 6, hjust = 0, color = "white"),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        plot.background = element_rect(fill = "black"),
        plot.margin = margin(0.5, 0.75, 0.5, 0.5)) +
  labs(title = "Board games ratings by decade", 
       subtitle = "Board games with the <span style ='color:#fe7d0f'>**lowest**</span> and <span style = 'color:#64d079'>**highest**</span> average rating by decade, published between 1900 and 2021",
       caption = "Data: Board Game Geek | Viz: Evan Gower",
       x = "Average rating", y = "Decade")

# Save plot
ggsave('boardgames.png', width = 7, height = 5)
