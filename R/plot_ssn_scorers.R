plot_ssn_scorers <- function(seasons) {
  df <- get_ssn_scorers(seasons) %>%
    mutate(
      ordered = paste0(season, total_goals, player_name) %>%
        forcats::fct_inorder()
    )

  player_order <- df$player_name

  p <- ggplot(
    df,
    aes(
      x = ordered,
      y = total_goals
    )
  ) +
    geom_col(
      aes(
        fill = season
        ),
      color = "black",
      linewidth = 0.2
      ) +
    theme_classic() +
    labs(
      x = NULL,
      y = NULL
    ) +
    facet_wrap(
      ~season,
      scales = "free_x",
      ncol = 2
    ) +
    scale_fill_brewer(
      palette = "Greens"
    ) +
    scale_x_discrete(
      labels = setNames(df$player_name, df$ordered)
    ) +
    theme(
      legend.position = "none"
    )

  ggplotly(p) %>% layout(hoverlabel = list(align = "left"))
}
