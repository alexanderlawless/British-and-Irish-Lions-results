library(tidyverse)
library(janitor)

results <- 
  read_csv("Lions_results_csv.csv") %>% 
  clean_names() %>% 
  mutate(test_flag = case_when(str_detect(game, "test") ~ "Test",
                               TRUE ~ "Warm up"))

# Bar chart
results %>% 
  mutate(opposition_points_neg = -opposition_points) %>% 
  pivot_longer(cols = c(lions_points, opposition_points_neg),
               names_to = "team",
               values_to = "score") %>% 
  mutate(team_id = case_when(
    str_detect(team, "lions") ~ "Lions",
    tour == "New Zealand" ~ "NZ",
    tour == "South Africa" ~ "SA",
    tour == "Australia" ~ "Aus"
  )) %>% 
  mutate(team_id = factor(team_id, levels = c("Lions", "NZ", "SA", "Aus"))) %>% 
  
  ggplot(aes(x = game_id, 
             y = score, 
             fill = team_id,
             alpha = test_flag)) +
  geom_vline(xintercept = 13.5, linetype = "dashed", colour = "grey38") +
  geom_vline(xintercept = 26.5, linetype = "dashed", colour = "grey38") +
  geom_vline(xintercept = 36.5, linetype = "dashed", colour = "grey38") +
  geom_vline(xintercept = 48.5, linetype = "dashed", colour = "grey38") +
  geom_vline(xintercept = 58.5, linetype = "dashed", colour = "grey38") +
  geom_vline(xintercept = 68.5, linetype = "dashed", colour = "grey38") +
  geom_vline(xintercept = 78.5, linetype = "dashed", colour = "grey38") +
  geom_hline(yintercept = 0) +
  
  # Series opposition
  annotate("text", x = 7, y = -55,    label = "New Zealand '93", fontface = 2) +
  annotate("text", x = 20, y = -55,   label = "South Africa '97", fontface = 2) +
  annotate("text", x = 31.5, y = -55, label = "Australia '01", fontface = 2) +
  annotate("text", x = 42.5, y = -55, label = "New Zealand '05", fontface = 2) +
  annotate("text", x = 53.5, y = -55, label = "South Africa '09", fontface = 2) +
  annotate("text", x = 63.5, y = -55, label = "Australia '13", fontface = 2) +
  annotate("text", x = 73.5, y = -55, label = "New Zealand '17", fontface = 2) +
  annotate("text", x = 83.5, y = -55, label = "South Africa '21", fontface = 2) +
  
  # Series result
  annotate("text", x = 1,    y = 110, label = "Series result:", fontface = 2) +
  annotate("text", x = 7,    y = 110, label = "Lost", fontface = 2, colour = "#d7301f") +
  annotate("text", x = 20,   y = 110, label = "Won",  fontface = 2, colour = "#006d2c") +
  annotate("text", x = 31.5, y = 110, label = "Lost", fontface = 2, colour = "#d7301f") +
  annotate("text", x = 42.5, y = 110, label = "Lost", fontface = 2, colour = "#d7301f") +
  annotate("text", x = 53.5, y = 110, label = "Lost", fontface = 2, colour = "#d7301f") +
  annotate("text", x = 63.5, y = 110, label = "Won",  fontface = 2, colour = "#006d2c") +
  annotate("text", x = 73.5, y = 110, label = "Draw", fontface = 2, colour = "grey38") +
  annotate("text", x = 83.5, y = 110, label = "...",  fontface = 2, colour = "grey38") +
  
  # Arrows + labels
  annotate(geom = "curve", x = 20, xend = 26, y = 95, yend = 110, 
           curvature = -0.2, arrow = arrow(length = unit(2, "mm"))) +
  annotate("text", x = 20, y = 90, 
           label = "Lions' biggest win 
(Western Australia, 116-10)", 
           fontface = 2, colour = "grey38", size = 3) +
  
  annotate(geom = "curve", x = 42, xend = 45, y = -43, yend = -48, 
           curvature = 0.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate("text", x = 41, y = -35, 
           label = "Lions' biggest 
loss 
  (All Blacks, 48-18)", 
           fontface = 2, colour = "grey38", size = 3) +
  
  geom_col() +
  scale_y_continuous(breaks = c(-50, 0, 50, 100), labels = c("50", "0", "50", "100")) +
  scale_fill_manual(values = c(
    "#d7301f", #Lions
    "#252525", #NZ
    "#006d2c", #SA
    "#ffd92f"  #Aus
  )) +
  scale_alpha_manual(values = c(1, 0.3)) +
  xlim(0,87) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    panel.background = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 16), 
    plot.caption = element_text(hjust = 0, face= "italic"), 
    plot.subtitle = element_text(face = "italic")
    ) +
  labs(title = "British and Irish Lions warm up game and test series results, 1993-2021",
       subtitle = "Light colour: Warm up games, Dark colour: Test series",
       caption = "Twitter: @alexlawless92, Github: alexanderlawless",
       x = "",
       y = "Points")
  

