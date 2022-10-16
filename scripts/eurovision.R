library(tidyverse)
library(showtext)

# Load data
tuesdata <- tidytuesdayR::tt_load('2022-05-17')
eurovision <- tuesdata$eurovision

# Add custome font
font_add_google("Cairo", "cario")
showtext_opts(dpi = 300)
showtext_auto()

# Data wrangling 
gf <- eurovision %>%
  filter(section == "grand-final", year != 2020) %>%
  group_by(artist_country) %>%
  mutate(n = n_distinct(year))

host <- gf %>%
  group_by(year, host_country) %>%
  filter(host_country == artist_country)

top3 <- gf %>%
  filter(rank %in% c(1, 2, 3))

last <- gf %>%
  group_by(year) %>%
  slice_max(rank, n = 1)

lvls <- gf %>%
  group_by(artist_country) %>%
  summarise(highest_rank = min(rank), n = n_distinct(year)) %>%
  ungroup() %>%
  arrange(n, desc(highest_rank)) %>%
  pull(artist_country)

# Plot data
gf %>%
  mutate(artist_country = factor(artist_country, levels = lvls)) %>%
  ggplot(aes(year, artist_country)) +
  geom_line(aes(group = artist_country), size = 0.3, color = "grey70") +
  geom_text(data = gf %>% select(artist_country, n) %>% distinct(), aes(2022.5, fct_rev(artist_country), label = glue::glue("App = {n}")), family = "cario", size = 3.5, color = "grey55", hjust = 0) +
  geom_point(shape = 21, size = 2.5, fill = "white") +
  geom_point(data = top3, aes(fill = rank_ordinal), shape = 21, size = 2.5) +
  scale_fill_manual(values = c("#fed766", "#20a4f3", "#ef476f"), guide = guide_legend(order = 1)) +
  ggnewscale::new_scale_fill() +
  geom_point(data = last, aes(fill = "Last"), shape = 21, size = 2.5) +
  scale_fill_manual(values = ("grey65")) +
  geom_point(data = host, aes(color = "Host"), shape = 3, size = 2.5) +
  scale_color_manual(values = c("black")) +
  scale_x_continuous(position = "top", breaks = seq(2005, 2020, 5), expand = expansion(mult = c(0.02, NA ))) +
  cowplot::theme_minimal_vgrid(12) +
  theme(text = element_text(family = "cario"),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "top",
        plot.title = element_text(size = 14),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, color = "grey65", size = 9, margin = margin(t = 13))) +
  labs(title = "Eurovision Grand Final Rankings",
       subtitle = "From 2004 to 2022, artist countries arranged in descending order by total years in grand final\n and highest rank achieved",
       caption = "Data: Eurovision | Viz: Evan Gower")

# Save plot
ggsave("eurovision-rankings.png", width = 8, height = 7)
  
  