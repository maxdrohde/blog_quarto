```{r}
#| echo: false
if (!file.exists("nelder_mead_path.mp4")) {
  
  neg_loglikelihood_function <- function(parameters){
    
    # optim() expects the parameters as a single vector, so we set the coefficients
    # as the elements of a vector called `parameters`
    b0 <- 58.075
    b1 <- parameters[1]
    b2 <- parameters[2]
    
    linear_predictor <- (b0) + (b1*df$bill_length_cm) + (b2*df$body_mass_kg)
    
    # Likelihood for each observation
    # If the observation is Adelie, then the likelihood is the probability of Adelie
    # If the observation is not Adelie (i.e., Gentoo), then the likelihood is the probability of not Adelie
    # which is 1 - P(Adelie)
    likelihood <- ifelse(df$adelie==1,
                         expit(linear_predictor),
                         1-expit(linear_predictor))
    
    # Log-likelihood for each observation
    log_likelihood <- log(likelihood)
    
    # Joint log-likelihood for all the observations. Note the sum because
    # multiplication is addition on the log-scale
    total_log_likelihood <- sum(log_likelihood)
    
    # the optim() function only minimizes, so we return the negative log-likelihood
    # and then maximize it
    return(-total_log_likelihood)
  }
  
  ## Optimizing
  set.seed(1234)
  optim_res <- optim(par=c(-5, -5),
                     fn = neg_loglikelihood_function,
                     method="Nelder-Mead")
  
  run_opt <- function(maxit){
    set.seed(1234)
    res <-
      optim(par=c(-5, -5),
            fn = neg_loglikelihood_function,
            method="Nelder-Mead",
            control=list(maxit=maxit))
    
    return(tibble(b1=res$par[1],
                  b2=res$par[2],
                  val=res$value))
  }
  
  counts <- optim_res$counts["function"]
  
  path_df <-
    map_dfr(1:counts, ~run_opt(.x)) %>%
    mutate(step = 1:counts)
  
  grid <-
    crossing(
      b1 = seq(-12, -3, length.out=1e3),
      b2 = seq(-10, -3, length.out=1e3)
    )
  
  grid$neg_loglikelihood <-
    pmap_dbl(grid,
             ~neg_loglikelihood_function(c(..1, ..2)))
  
  p1 <-
    path_df %>%
    ggplot() +
    aes(x=b1, y=b2) +
    geom_raster(data=grid, aes(fill = neg_loglikelihood)) +
    geom_contour(data = grid, aes(z=neg_loglikelihood), bins = 50, size=0.1, color="gray") +
    geom_point(color="white") +
    geom_path(color="white", alpha=0.5) +
    scale_fill_viridis_c() +
    coord_cartesian(expand = FALSE) +
    annotate(geom="point", x=-8.999, y=-4.363, color="red") +
    transition_reveal(along = step) +
    labs(subtitle = "Iteration: {frame_along}",
         fill = "Negative log-likelihood\n",
         x = "Beta 1",
         y = "Beta 2",
         title = "Visualizing the path of Nelder-Mead") +
    theme(legend.position = "none",
          legend.key.height = unit(1, "cm"))
  
  gif <- animate(p1,
                 duration=10,
                 height = 4,
                 width = 5,
                 units = "in",
                 res = 300,
                 renderer = ffmpeg_renderer())
  
  # Save to mp4
  anim_save(animation = gif, filename = "p1.mp4")
  
  p2 <-
    path_df %>%
    ggplot() +
    aes(x=step, y=val) +
    geom_point() +
    geom_path(alpha=0.5) +
    transition_reveal(along = step) +
    cowplot::theme_cowplot() +
    labs(subtitle = "Objective function",
         x="Iteration",
         y='Negative Log-Likelihood') +
    theme(legend.position = "none")
  
  gif <- animate(p2,
                 duration=10,
                 height = 4,
                 width = 3,
                 units = "in",
                 res = 300,
                 renderer = ffmpeg_renderer())
  
  # Save to mp4
  anim_save(animation = gif, filename = "p2.mp4")
  
  system('ffmpeg -i p1.mp4 -i p2.mp4 -filter_complex hstack nelder_mead_path.mp4 -vsync 2')
}
```

::: {.column-page-right}
![Animation of the path taken by the Nelder Mead algorithm](nelder_mead_path.mp4)
:::