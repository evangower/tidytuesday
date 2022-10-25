library(tidyverse)
library(ggtext)
library(ggbump)

# Load data
tuesdata <- tidytuesdayR::tt_load(2022, week = 22)

# Prepare data
data <- poll %>%
  mutate(year = 2022) %>%
  distinct(company, industry, year, rank = '2022_rank', rq = '2022_rq') %>%
  bind_rows(select(poll, company, industry, year, rank, rq)) %>%
  arrange(company, year) %>%
  group_by(company) %>%
  mutate(change = rank - lag(rank, default = NA)) %>%
  ungroup()

# Create faang companies dataframe
faang <- c("Facebook", "Amazon.com", "Apple", "Netflix", "Google")

# Filter for faang companies
data_faang <- data %>%
  filter(company %in% faang) %>%
  mutate(company = ifelse(company == "Amazon.com", "Amazon", company)) %>%
  filter(!is.na(rank))

# Plot data
ggplot(data_faang, aes(year, rank, color = company)) +
  geom_bump(size = 1) +
  geom_point(shape = 21, size = 5, stroke = 0.75, fill = "white") +
  geom_text(aes(label = rank), size = 2) +
  geom_text(data = ~subset(., year == max(year | year == min(year)),
                           aes(x = ifelse(year == max(year), year + 0.15, year - 0.15),
                                          y = ifelse(company == "Netflix" & year == max(year), 0, 1)),
                               label = company, hjust = ifelse(year == max(year), 0, 1)), size = 3.5) +
  scale_x_continuous(position = "top") +
  scale_y_reverse() +
  scale_color_manual(values = c("ff9900", "#000000", "0668e1", "0f9d58", "e50914")) +
  coord_cartesian(clip = "off") +
  theme_void() +
  labs(title = "Reputation of FAANG companies",
       subtitle = "How did tech companies Facebook (Meta), Amazon, Apple, Netflix, and Google (Alphabet) fare from 2017 to 2022 in the annual Axios Harris Poll 100,
       whixh gauges coporate awareness and reputation among the U.S. public?",
       caption = "Data: Axios Harris Poll 100 | Viz: Evan Gower") +
  annotate(GeomTextBox, x = 2019.2, y = 79, label = "Facebook's reputation dropped in 2019 amid privacy concerns and allegations of election interference",
           hjust = 0, fill = NA, box.size = 0, size = 2.5) +
  annotate("segment", x = 2019.3, xend = 2019, y = 83.25, yend = 92, size = 0.2) +
  theme(plot.margin = margin(t = 4, r = 45, b = 4, l = 45),
        plot.tilte = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(t = 4, b = 2)),
        plot.subtitle = element_textbox_simple(size = 10, hjust = 0.5, width = 0.95, color = "grey15", margin = margin(t = 5, b = 16)),
        plot.caption = element_text(size = 6, hjust = 0),
        axis.text.x.top = element_text(color = "grey20", margin = margin(b = 4)),
        axis.line.x.top = element_line(size = 0.1, color = "grey50"))