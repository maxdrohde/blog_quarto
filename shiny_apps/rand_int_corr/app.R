library(shiny)
library(bslib)
library(tidyverse)
library(glue)
library(patchwork)

### FUNCTIONS ###

generate_plot <- function(n, sd_rand_int, sd_error){
  t <- 1:4
  
  Y <-
    map(set_names(t), ~rnorm(n = n, mean = 1, sd=sd_error)) |>
    do.call(cbind, args = _)
  
  Y <- Y + rnorm(n=n, mean = 0, sd=sd_rand_int)
  
  df_wide <-
    as_tibble(Y) |>
    rowid_to_column("id")
  
  pairs(Y, main = "Pairs plot")
}

generate_plot2 <- function(n, sd_rand_int, sd_error){
  t <- 1:5
  
  Y <-
    map(set_names(t), ~rnorm(n = n, mean = 1, sd=sd_error)) |>
    do.call(cbind, args = _)
  
  Y <- Y + rnorm(n=n, mean = 0, sd=sd_rand_int)
  
  ggcorrplot::ggcorrplot(cor(Y), lab = TRUE, colors = c("#6D9EC1", "white", "#E46726"), tl.srt = 0) +
    cowplot::theme_cowplot(font_size=12,
                           font_family = "Source Sans Pro") +
    labs(title = "Correlation matrix", x = "Time 1", y = "Time 2")
}

generate_plot3 <- function(n, sd_rand_int, sd_error){
  t <- 1:5
  
  Y <-
    map(set_names(t), ~rnorm(n = n, mean = 1, sd=sd_error)) |>
    do.call(cbind, args = _)
  
  Y <- Y + rnorm(n=n, mean = 0, sd=sd_rand_int)
  
  df_wide <-
    as_tibble(Y) |>
    rowid_to_column("id")
  
  df_long <-
    df_wide |>
    pivot_longer(cols = -id, names_to = "t", values_to = "y")
  
  df_long |>
    ggplot() +
    aes(x=t, y=y, group=id) +
    geom_line(alpha=0.5) +
    cowplot::theme_cowplot(font_size=12,
                           font_family = "Source Sans Pro") +
    labs(title = "Profile plot", x = "Time", y = "y")
}

### SHINY ###

ui <-
  fluidPage(theme=bs_theme(version = 5,
                           bootswatch = "journal",
                           primary = "#5b7eb0",
                           base_font = font_google("Source Sans Pro"),
                           code_font = font_google("Source Code Pro")),
            titlePanel("How random intercepts induce correlation structure"),
            sidebarLayout(
              sidebarPanel(
                sliderInput("n", "Number of subjects (n)", value=50, min = 5, max = 200, step = 5),
                sliderInput("sd_rand_int", "SD of random intercept (Tau)", value = 1, min = 0, max = 10, step = 0.5),
                sliderInput("sd_error", "SD of error term (Sigma)", value = 1, min = 0, max = 10, step = 0.5),
                p("This Shiny app demonstrates how random intercepts introduce a correlation structure in longitudinal data."),
                p("In this simulation, data is collected at four timepoints. Each of the n subjects has an outcome distributed Normal with mean = 1 and SD = sigma."),
                p("A correlation structure is induced by giving each subject a random intercept distributed Normal with mean = 0 and SD = tau."),
                p("Change these parameters and see how they affect the correlation structure of the data."),
              ),
              mainPanel(
                fluidRow(
                  splitLayout(cellWidths = c("50%", "50%"), plotOutput("main"), plotOutput("area"))
                ),
                plotOutput("profile")
              )
            )
  )

server <- function(input, output, session) {
  output$main <- renderPlot({
    generate_plot(input$n, input$sd_rand_int, input$sd_error)
  })
  
  output$area <- renderPlot({
    generate_plot2(input$n, input$sd_rand_int, input$sd_error)
  })
  
  output$profile <- renderPlot({
    generate_plot3(input$n, input$sd_rand_int, input$sd_error)
  })
  
}

shinyApp(ui, server)