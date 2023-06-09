plot_ssn_scorers <- function(seasons) {
  df <- get_ssn_scorers(seasons) %>%
    arrange(
      season,
      total_goals,
      player_name
    ) %>%
    mutate(
      ordered = paste0(season, total_goals, player_name) %>%
        forcats::fct_inorder()
    )
  p <- ggplot(
    df,
    aes(x = ordered, y = total_goals)
  ) +
  geom_col(
    aes(
      fill = total_goals
      )
    ) +
  geom_text(aes(x = ordered, y = total_goals, label = total_goals),
            color = "white",
            hjust = "left",
            nudge_y = -0.275) +
  labs(
    x = NULL,
    y = NULL
  ) +
  facet_wrap(
    ~season,
    scales = "free_y",
    ncol = 2
  ) +
  scale_fill_gradient(
    low = "green4",
    high = "darkgreen"
  ) +
  scale_x_discrete(
    labels = setNames(df$player_name, df$ordered),
    expand = expansion(mult = c(0, 0), add = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0), add = c(0, 0))
   ) +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(
      family = "Helvetica Neue"
    ),
    strip.text.x = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    strip.background = element_rect(
      fill = "white",
      color="white"
    ),
    panel.border = element_blank(),
    line = element_blank(),
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    panel.spacing = unit(1.0, "lines")
  ) +
  coord_flip()

  ggplotly(p, tooltip = "text")
}
