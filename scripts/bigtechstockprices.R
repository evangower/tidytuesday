library(tidyverse)
library(showtext)
library(janitor)
library(ggtext)
library(glue)

# Load data
tuesdata <- tidytuesdayR::tt_load(2023, week = 6)
big_tech_stock_prices <- tuesdata$big_tech_stock_prices
big_tech_companies <- tuesdata$big_tech_companies

# Add custom font
font_add_google(name = "Inter", family = "inter")
showtext_auto()

# Add color palette
pal <- c(AAPL = "#a2aaad", ADBE = "#ff0000", AMZN = "#ff9900", CRM = "#009edb", CSCO = "#1798c1", GOOGL = "#34a853", IBM = "#1fc0c1",
         INTC = "#127cc1", META = "#0668e1", MSFT = "#f34f1c", NFLX = "#d81f26", NVDA = "#76b900", ORCL = "#c74634", TSLA = "#e31937")

# Clean company data
big_tech_companies$company <- gsub(',', '', big_tech_companies$company)
big_tech_companies$company <- gsub('Inc.', '', big_tech_companies$company)
big_tech_companies$company <- gsub('Corporation', '', big_tech_companies$company)

# Join datasets 
df_joined <- map_dfr(unique(big_tech_stock_prices$stock_symbol), ~{
  big_tech_stock_prices $>$
    mutate(group = stock_symbol, stock_symbol = .x)
}) %>%
  left_join(big_tech_companies, by = "stock_symbol")

# Create visualization
ggplot(df_joined) %>%
  geom_line(aes(date, close, group = group), alpha = 0.5, size = 0.1, color = "grey") +
  geom_area(aes(date, close, color = stock_symbol, fill = stock_symbol), big_tech_stock_prices, alpha = 0.2, size = 0.5) +
  geom_text(aes(lubridate::ymd("2011-01-01"), 500, label = str_wrap(company, 20), color = stock_symbol), big_tech_companies, family = "inter", size = 16, hjust = 0, vjust = 1, lineheight = 0.3, fontface = "bold") +
  facet_wrap(~stock_symbol, ncol = 3) +
  scale_color_manual(vaules = pal) +
  scale_fill_manual(vaules = pal) +
  theme_void() +
  labs( title = "Big Tech Stock Prices",
        subtitle = "Stock price movement of 14 big tech companies between 2010 and 2022",
        caption = "Data: Yahoo Finance | Viz: Evan Gower") +
  theme(text = element_text(family = "inter", size = 48, lineheight = 0.3, color = "black"),
        plot.background = element_rect(fill = "#fafafa", color = "#fafafa"),
        plot.title = element_text(size = 84, margin = margin(b = 10), face = "bold"),
        plot.subtitle = element_text(size = 50, margin = margin(b = 10)),
        plot.caption = element_markdown(margin = margin(b = 10)),
        plot.margin = margin(20, 50, 20, 50),
        axis.text = element_text(size = 24),
        strip.text = element_blank(),
        legend.position = "none")

# Save plot
ggsave("bigtechstockprices.png", width = 12, height = 12)