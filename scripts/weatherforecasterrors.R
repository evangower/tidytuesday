library(tidyverse)
library(lubridate)
library(usmap)
library(patchwork)
library(showtext)

# Load data
tuesdata <- tidytuesdayR::tt_load(2022, week = 51)
weather_forecasts <- tuesdata$weather_forecasts
cities <- tuesdata$cities
outlook_meanings <- tuesdata$outlook_meanings

# Add custom font
font_add_google(name = "Cairo", family = "cairo")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Prepare data
way_off <- weather_forecasts %>%
  filter(forecasts_hours_before == 12) %>%
  select(date, city, state, high_or_low, forecast_temp) %>%
  mutate(error = observed_temp - forecast_temp) %>%
  filter(abs(error) >= 10) %>%
  count(city, state)

us <- map_data("state")

kcc <- c("A" = "Tropical", "B" = "Arid", "C" = "Temperate", "D" = "Continental")

df <- cities %>%
  left_join(way_off, by = c("city", "state")) %>%
  replace_na(list(n = 0)) %>%
  filter(!state %in% c("AK", "HI", "PR", "VI")) %>%
  mutate(binned = cut(n, c(-1, 1, 10, 20, 30, 40, 50, 60)),
         koppen = str_stub(koppen, 1, 1),
         koppen = kcc[koppen])

# Create visualization
ggplot(df) +
  geom_map(data = us, map = us, aes(x = long, y = lat, map_id = region), fill = "#ffffff", color = "#123456", size = 0.15) +
  geom_point(aes(x = lon, y = lat, color = koppen, size = n, alpha = n)) +
  scale_color_manual(vaules = c("#6e0808", "#f6bd3a", "#034c3e", "#2b50aa")) +
  scale_alpha_continuous(range = c(0.5, 1)) +
  coord_map("albers", lat0 = 39, lat1 = 45, xlim = c(-119.22, -75.8)) +
  guides(color = guide_legend("Köppen Climate"), size = guide_legend("Number of major forecasts errors"), alpha = guide_legend("Number of major forecasts errors")) +
  labs(title = "Temperatue Suprises", 
       subtitle = "Across America, the number of forecasts that had a major error when the high and low was off by 10 degrees Fahrenheit or more between\n January 30, 2021 and June 1, 2022. Helena, Montana has by far the most temperature suprises in the United States",
       caption = "Data: Weather Forecast Capstone Project | Viz: Evan Gower") +
  theme_void() +
  theme(text = element_text(family = "cairo"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 6, hjust = 0.5),
        plot.caption = element_text(size = 3),
        legend.position = "right",
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 4))

# Save plot
ggsave("weatherforecasterrors.png", width = 6, height = 4)