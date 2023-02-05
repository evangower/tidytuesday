library(tidyverse)
library(lubridate)
library(ggtext)
library(showtext)

# Load data
tuesdata <- tidytuesdayR::tt_load(2022, week = 40)
product_hunt <- tuesdata$product_hunt

# Add custom font
font_add_google("Cairo", family = "cairo")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Prepare data
df <- product_hunt %>%
  filter(str_detect(category_tags, "ANALYTICS")) %>%
  mutate(r_date = ymd(release_date), release_year = year(r_date),
         grp = case_when(is.na(product_of_the_day_date) ~ "No", TRUE ~ "Yes")) %>%
  add_count(release_year) %>%
  mutate(y = glue::glue("**{release_year}**<br><span style = 'font-size:6pt'>(n={n})<span>"))

year(df$r_date) = 2020

# Create visualization
ggplot(df, aes(r_date, fct_rev(factor(y)))) +
  ggbeeswarm::geom_beeswarm(aes(size = upvotes, color = grp), method = "square", alpha = 0.6, cex = 0.9) +
  geom_richtext(data = df, slice_max(upvotes, n = 1), aes(label = glue::glue("***{name}***: **{scales::comma(upvotes)}** upvotes ")), size = 2.8, hjust = 0, nudge_y = 0.4, fill = NA, color = "#08415c") +
  scale_size_area("Upvotes:" guide = guide_legend(override.aes = list(shape = 20)), breaks = c(100, 1000, 2000, 4000, 8000)) +
  scale_color_manual("Was it product of the day?", values = c("#cc2936", "#08415c")) +
  scale_x_date(date_labels = "%b") +
  cowplot::theme_minimal_grid(10.5) +
  guides(color = guide_legend(order = 1, override.aes = list(size = 3.8, shape = 15, alpha = 0.8))) +
  labs(title = "2,248 Analytics products on Product Hunt by release date",
       caption = "Data: components.one | Viz: Evan Gower")
  theme(text = element_text(family = "cairo"),
        legend.position = "top",
        legend.box = "vertical",
        legend.justification = "left",
        legend.box.just = "left",
        legend.spacing.y = unit(-0, "cm"),
        legend.title = element_text(size = 8.7),
        legend.text = element_text(size = 8.5),
        plot.margin = margin(l = -31),
        axis.title = element_blank(),
        axis.text.y = element_markdown(hjust = 0.5, lineheight = 1),
        axis.text.x = element_text(size = 8.5),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 7.5, color = "grey30", margin = margin(t = 13)))
  
  # Save plot
  ggsave("producthunt.png", width = 7, height = 7, bg = "#fafafa")