library(tidyverse)

# Set global ggplot theme
theme_set(cowplot::theme_cowplot(font_size=12,
                                 font_family = "Source Sans Pro"))

b <- 2.7
cuts <- c(-0.5, 2, 3.3)


get_prob <- function(x, b, cut_high, cut_low){
  pnorm(cut_high, mean = b*x) - pnorm(cut_low, mean = b*x) 
}

generate_plot <- function(b, cuts){
  grid <- seq(-2, 2, length.out=200)
  p_level_1 <- get_prob(grid, b, cuts[1], -Inf)
  p_level_2 <- get_prob(grid, b, cuts[2], cuts[1])
  p_level_3 <- get_prob(grid, b, cuts[3], cuts[2])
  p_level_4 <- get_prob(grid, b, Inf, cuts[3])
  
  tibble(grid, p_level_1, p_level_2, p_level_3, p_level_4) %>%
    pivot_longer(p_level_1:p_level_4, names_to = "level", values_to = "probability") %>%
    ggplot() +
    aes(x=grid, y=probability, color=level) +
    geom_line()
}

generate_plot(b, cuts)
