library(tidyverse)
library(treemapify)
library(showtext)

# Load data
tuesdata <- tidytuesdayR::tt_load('2022-01-18')
chocolate <- tuesdata$chocolate

# Add custom font
font_add_google(name = "Oswald", family = "Oswald")
showtext_auto()

# Prep data
data <- chocolate %>%
  group_by(country_of_bean_origin) %>%
  summarise(n = n(), rating = mean(rating)) %>%
  arrange(desc(n))

# Treemap plot
ggplot(data, aes(area = n, fill = rating, label = country_of_bean_origin)) +
  geom_treemap(color = "white") +
  geom_treemap_text(family = "Oswald",
                    fontface = "italic",
                    color = "white",
                    place = "centre",
                    grow = TRUE,
                    padding.x = grid::unit(3, "mm"),
                    padding.y = grid::unit(3, "mm")) +
  scale_fill_material(palette = "brown", 
                      name = "",
                      limits = c(2.7, 3.7),
                      breaks = c(2.7, 3.7),
                      guide = "none") +
  labs(title = "Where do cacao beans come from?",
       subtitle = "\nCacao beans from countries which are used by a larger number of manufacturers tend to result in higher rated\nchocolate. The exception is blended beans which are commonly used but score lower.\n\nDarker color = Higher rating\n",
       caption = "\nData : Flavors of Cacao | Viz: Evan Gower") +
  theme_void() +
  theme(plot.title = element_text(color = "grey20", family = "Oswald", face = "bold", size = 22),
        plot.subtitle = element_text(color = "grey20", family = "Oswald", face = "bold", size = 14),
        plot.caption = element_text(color = "grey40", family = "Oswald", size = 10),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
