library(shiny)
library(bslib)
library(tidyverse)
library(glue)

### FUNCTIONS ###

get_prob <- function(x, b, cut_high, cut_low, type="logit"){
  
  if (type == "logit") {
    plogis(cut_high, location = b*x) - plogis(cut_low, location = b*x) 
  } else if (type == "probit"){
    pnorm(cut_high, mean = b*x) - pnorm(cut_low, mean = b*x) 
  }
}
  

generate_plot <- function(b, cuts, type){
  grid <- seq(-4, 4, length.out=200)
  p_level_1 <- get_prob(grid, b, cuts[1], -Inf, type)
  p_level_2 <- get_prob(grid, b, cuts[2], cuts[1], type)
  p_level_3 <- get_prob(grid, b, cuts[3], cuts[2], type)
  p_level_4 <- get_prob(grid, b, Inf, cuts[3], type)
  
  tibble(grid, p_level_1, p_level_2, p_level_3, p_level_4) %>%
    pivot_longer(p_level_1:p_level_4, names_to = "level", values_to = "probability") %>%
    mutate(level = recode(level,
                          `p_level_1` = "Y=1",
                          `p_level_2` = "Y=2",
                          `p_level_3` = "Y=3",
                          `p_level_4` = "Y=4")) %>%
    ggplot() +
    aes(x=grid, y=probability, color=level) +
    geom_line() +
    geom_vline(xintercept = cuts[1], linetype=2, alpha=0.7) +
    geom_vline(xintercept = cuts[2], linetype=2, alpha=0.7) +
    geom_vline(xintercept = cuts[3], linetype=2, alpha=0.7) +
    coord_cartesian(xlim = c(-4,4), ylim=c(0,1)) +
    cowplot::theme_cowplot(font_size=18,
                           font_family = "Source Sans Pro") +
    labs(x = "x",
         y = "Probability",
         subtitle = glue("Cumulative ordinal model with {type} link (with a single predictor)"),
         color = "Level")
}

generate_plot2 <- function(b, cuts, type){
  grid <- seq(-4, 4, length.out=200)
  p_level_1 <- get_prob(grid, b, cuts[1], -Inf, type)
  p_level_2 <- get_prob(grid, b, cuts[2], cuts[1], type)
  p_level_3 <- get_prob(grid, b, cuts[3], cuts[2], type)
  p_level_4 <- get_prob(grid, b, Inf, cuts[3], type)
  
  tibble(grid, p_level_1, p_level_2, p_level_3, p_level_4) %>%
    pivot_longer(p_level_1:p_level_4, names_to = "level", values_to = "probability") %>%
    mutate(level = recode(level,
                          `p_level_1` = "Y=1",
                          `p_level_2` = "Y=2",
                          `p_level_3` = "Y=3",
                          `p_level_4` = "Y=4")) %>%
    ggplot() +
    aes(x=grid, y=probability, fill=level) +
    geom_area(position = "fill") +
    geom_vline(xintercept = cuts[1], linetype=2, alpha=0.7) +
    geom_vline(xintercept = cuts[2], linetype=2, alpha=0.7) +
    geom_vline(xintercept = cuts[3], linetype=2, alpha=0.7) +
    coord_cartesian(xlim = c(-4,4), ylim=c(0,1)) +
    cowplot::theme_cowplot(font_size=18,
                           font_family = "Source Sans Pro") +
    labs(x = "x",
         y = "Probability",
         fill = "Level")
}

### SHINY ###

ui <-
  fluidPage(theme=bs_theme(version = 5,
                           bootswatch = "journal",
                           primary = "#5b7eb0",
                           base_font = font_google("Source Sans Pro"),
                           code_font = font_google("Source Code Pro")),
    titlePanel("Understanding Cumulative Ordinal Models"),
    sidebarLayout(
      sidebarPanel(
        p("Cumulative ordinal models have slope and cutpoint parameters, but they can be difficult to interpret."),
        p("This app allows you to adjust these parameters and see how the predicted probability of each category changes."),
        sliderInput("slope", "Slope", value=2.7, min = -4, max = 4, step = 0.1),
        sliderInput("c1", "Cutpoint 1", value=-0.5, min = -4, max = 1, step = 0.1),
        sliderInput("c2", "Cutpoint 2", value=2, min = 1, max = 2.5, step = 0.1),
        sliderInput("c3", "Cutpoint 3", value=3.3, min = 2.5, max = 4, step = 0.1),
        selectInput("type", "Link Function", choices = c("probit", "logit"))
      ),
      mainPanel(
        plotOutput("main"),
        plotOutput("area")
      )
    )
  )

server <- function(input, output, session) {
  output$main <- renderPlot({
    generate_plot(input$slope, c(input$c1, input$c2, input$c3), input$type)
  })
  
  output$area <- renderPlot({
    generate_plot2(input$slope, c(input$c1, input$c2, input$c3), input$type)
  })
  
 
}

shinyApp(ui, server)