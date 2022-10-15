library(tidyverse)
library(showtext)

# Load data
tuesdata <- tidytuesdayR::tt_load('2022-03-22')
babynames <- tuesdata$babynames

# Add custom font
font_add_google("Chivo", "chivo")
showtext_auto()

# Preparing data
data <- babynames %>%
  group_by(year) %>%
  filter(year >= 1900) %>%
  summarise(count = sum(n[name == "Evan"]), total = sum(n)) %>%
  mutate(perc_total = count/total)

# Plot data
ggplot(data, aes(year, perc_total)) +
  geom_line(size = 0.8, color = "#f7f7fc") +
  scale_x_continuous(breaks = seq(1900, 2017, 10)) +
  scale_y_continuous(limits = c(0, 0.003), labels = scales::label_percent(accuracy = 0.01)) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#05192d"),
        panel.grid = element_blank(),
        plot.caption = element_text(size = 10, family = "chivo", color = "#f7f7fc", margin = margin(t = 10)),
        axis.title.y = element_text(size = 14, family = "chivo", color = "#f7f7fc", margin = margin(r = 10)),
        axis.text = element_text(size = 14, family = "chivo", color = "#f7f7fc")) +
  labs(x = element_blank(), y = "Percent share in the total count of child name",
       caption = "Data: Babynames | Viz: Evan Gower") +
  annotate(geom = "point", x = 2009, y = max(data$perc_total + 0.00002), size = 1.25, color = "#03ef62") +
  annotate(geom = "text", x = 2004, y = max(data$perc_total + 0.00012), label = "Peak year at 2009", size = 5, family = "chivo", color = "#03ef62") +
  annotate(geom = "text", x = 1900, y = 0.00242, label = "How popular is the name Evan?", size = 12, hjust = 0, vjust = 0, family = "chivo", color = "#f7f7fc") +
  annotate(geom = "text", x = 1900, y = 0.0020, label = "The percent share in the total yearly childbirth starts to rise\nduring the late 70's and soars in popularity during the\n80's and 90's. The popularity of the name peaked in the 2000's.",
           size = 6, lineheight = 0.3, hjust = 0, vjust = 0, family = "chivo", color = "#f7f7fc")

# Save plot
ggsave("babynames.png", width = 4, height = 3)
  