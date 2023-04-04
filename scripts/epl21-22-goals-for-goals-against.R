library(tidyverse)
library(showtext)
library(rlang)
library(ggforce)
library(ggrepel)

# Load data
tuesdata <- tidytuesdayR::tt_load(2023, week = 14)
soccer <- tuesdata$soccer

# Add custom font
font_add_google('Cairo', 'cairo')
showtext_auto()

# Preparing data
# Define the arguments, and set defaults to use the full time results
full_season_stats <- function(x, teamName, resultColumn = FTR, resultPrefix = FT) {
  results <- enquo(result)
  home_goals <- new_quosure(sym(paste0(quo_name(enquo(resultPrefix)), 'HG')))
  away_goals <- new_quosure(sym(paste0(quo_name(enquo(resultPrefix)), 'AG')))
  
  home_game_summary <- x %>%
    filter(HomeTeam == teamName) %>%
    summarise(MP = n(),
              W = sum(!!result == 'H'),
              D = sum(!!result == 'D'),
              L = sum(!!result == 'A'),
              GF = sum(!!home_goals),
              GA = sum(!!away_goals),
              GD = GF - GA,
              Points = W * 3 + D)
  
  away_game_summary <- x %>%
    filter(AwayTeam == teamName) %>%
    summarise(MP = n(),
              W = sum(!!result == 'A'),
              D = sum(!!result == 'D'),
              L = sum(!!result == 'H'),
              GF = sum(!!away_goals),
              GA = sum(!!home_goals),
              GD = GF - GA,
              Points = W * 3 + D)
  
  final_stats <-(home_game_summary %>% rownames_to_colum()) %>%
    bind_rows(away_game_summary %>% rownames_to_column()) %>%
    group_by(rowname) %>%
    summarise_all(sum) %>%
    select(-rowname) %>%
    mutate(Team = teamName) %>%
    select(Team, everything())
  
  return(final_stats)
}

# Applying function to all teams
league_table <- tibble()

for(team in unique(soccer$HomeTeam)) {
  league_table <- rbind(league_table, full_season_stats(soccer, team, FTR, FT))
}

# Arrange league table and apply group column
league_table <- league_table %>%
  arrange(desc(Points), desc(GD)) %>%
  rownames_to_column() %>%
  rename('Pos' = rowname) %>%
  mutate(GRP = case_when(Pos %in% 1:4 ~ 'Top 4',
                         Pos %in% 4:10 ~ 'Top Half',
                         Pos %in% 11:17 ~ 'Bottom Half',
                         Pos %in% 18:20 ~ 'Relegation'))

# Create data visualization
ggplot(league_table, aes(GA, GF)) +
  geom_point(aes(color = GRP), size = 2) +
  geom_mark_ellipse(aes(fill = GRP), alpha = 0.1, color = NA, show.legend = FALSE) +
  geom_text_repel(aes(label = Team), family = 'cairo', color = "#888888") +
  scale_color_maual(values = c("#ffc20e", "#d71920", "#0db14b", "#00aeef")) +
  scale_fill_manual(values = c("#ffc20e", "#d71920", "#0db14b", "#00aeef")) +
  labs(title = "Goals For And Against",
       subtitle = "A look at clubs goals for/against during the 2021/22 EPL season",
       caption = "Data English Premier League | Viz: Evan Gower",
       x = "Goals Against", y = "Goals For") +
  theme_minimal() +
  theme(text = element_text(family = 'cairo', color = "gray10"),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 4),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = 'top',
        axis.title = element_text(size = 8),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank())
  
# Save plot
ggsave("epl21-22-goals-for-goals-against.png", width = 7, height = 5.5)