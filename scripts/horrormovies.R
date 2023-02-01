library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ggtext)

# Load data
tuesdata <- tidytuesdayR::tt_load(2022, week = 44)
horror_movies <- tuesdata$horror_movies

# Add custom fonts
font_add_google(name = "Creepster", family = "Creep")
font_add_google(name = "Freckle Face", family = "Freckle")
showtext_ops(dpi = 3000)
showtext_auto()

# Prepare data
movies_per_day <- horror_movies %>%
  mutate(new_date = substring(as.character(release_date), 6) %>% as.factor()) %>%
  group_by(new_date) %>%
  summarise(n = n()) %>%
  filter(new_date != "01-01") %>%
  sperate(new_date, c('month', 'day'), remove = FALSE) %>%
  mutate(is_october = month == "10")

month_nums <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
month_breakpoints <- c(paste0(month_nums, "-", "01"), "12-31")
month_breaks <- data.frame(start = month_breakpoints[1:12], end = month_breakpoints[2:13],
                           mid = paste0(month_nums, "-", "03"),
                           month = c("January", "February", "March", "April", "May", "June",
                                     "July", "August", "September", "October", "Noveber", "December")) %>%
  mutate(month = factor(month, levels = c("January", "February", "March", "April", "May", "June",
                                          "July", "August", "September", "October", "November", "December")))

# Visualize data
ggplot() +
  geom_point(movies_per_day, new_date, n, fill = is_october, color = "black", shape = 21, size = 3) +
  geom_point(data = filter(movies_per_day, new_date == "10-31"), aes(new_date, n), fill = NA, color = "grey60", shape = 21, size = 5) +
  geom_text(month_breaks, aes(x = mid, y = -20, label = month), color = "grey60", family = "Creep", size = 3.5, hjust = 0, angle = 0) +
  annotate(geom = "text", x = "10-25", y = 525, family = "Freckle", color = "grey60", hjust = 1, size = 3.5, label = str_wrap("Halloween >")) +
  scale_x_discrete(breaks = month_breakpoints, drop = FALSE) +
  scale_y_continuous(breaks = seq(0, 500, 100)) +
  scale_fill_manual(values = c("grey60", "#bc0000")) +
  labs(title = "HORROR MOVIES!", 
       subtitle = str_wrap("Number of horror movies released by day. Not surprisingly, there is a definite spike in <br> horror movie released in <i style = 'color:#bc0000'>October</i>, with the most popular release-day being Halloween.", width = 100),
                           caption = "Data: The Movie Database, via Tanya Shapiro | Viz: Evan Gower",
                           y = "Number of movies released") +
  coord_cartesian(ylim = c(0, 530), clip = "off") +
  theme_minimal() +
  theme(text = element_text(color = "grey60", family = "Freckle", size = 12),
        plot.title = element_markdown(color = "#bc0000", family = "Creep", size = 40),
        plot.subtitle = element_markdown(family = "Freckle", size = 15),
        plot.caption = element_text(size = 6, margin = margin(t = 20)),
        legend.position = "none",
        axis.text = element_text(color = "grey60"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "grey20"),
        plot.margin = margins(25, 20, 20, 20))

# Save plot image
ggsave("horrormovies.png", width = 10, height = 6)
  