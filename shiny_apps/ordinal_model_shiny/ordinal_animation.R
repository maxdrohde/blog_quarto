library(tidyverse)
library(gganimate)

get_prob <- function(x, b, cut_high, cut_low){
  pnorm(cut_high, mean = b*x) - pnorm(cut_low, mean = b*x) 
}

generate_data <- function(b, cuts){
  grid <- seq(-4, 4, length.out=200)
  p_level_1 <- get_prob(grid, b, cuts[1], -Inf)
  p_level_2 <- get_prob(grid, b, cuts[2], cuts[1])
  p_level_3 <- get_prob(grid, b, cuts[3], cuts[2])
  p_level_4 <- get_prob(grid, b, Inf, cuts[3])
  
  data <-
  tibble(grid, p_level_1, p_level_2, p_level_3, p_level_4) %>%
    pivot_longer(p_level_1:p_level_4, names_to = "level", values_to = "probability")
  
  data$level <- recode(data$level,
                       `p_level_1` = "Y=1",
                       `p_level_2` = "Y=2",
                       `p_level_3` = "Y=3",
                       `p_level_4` = "Y=4",
                       )
  
  data$b <- b
  
  return(data)
}

slopes <- seq(-2, 2, by=0.1)
df <- map_dfr(slopes , ~generate_data(.x,  c(-0.5, 2, 3.3)))

df %>%
ggplot() +
  aes(x=grid, y=probability, color=level) +
  geom_line() +
  coord_cartesian(xlim = c(-5,5)) +
  transition_manual(frames = b) +
  cowplot::theme_cowplot(font_size=12,
                         font_family = "Source Sans Pro") +
  labs(x = "x",
       y = "Probability",
       subtitle = "Slope = {current_frame}")
