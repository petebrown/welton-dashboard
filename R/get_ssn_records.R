get_ssn_records <- function(seasons, input_venue = "all") {
  df <- get_results_raw() %>%
    filter(
      season %in% seasons,
      game_type == "league",
      case_when(
        input_venue == "all" ~ venue %in% c("H", "A", "N"),
        TRUE ~ venue == toupper(input_venue))
    ) %>%
    group_by(season) %>%
    summarise(
      P = n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against)
    ) %>%
    mutate(
      GD = GF - GA,
      GF_GA = paste(GF, GA, sep="-"),
      W_pc = round((W/P) * 100, 2),
      Pts = (W * 3) + D,
      PPG = round((Pts / P), 2)
    ) %>%
    select(season, P, W, D, L, GF, GA, GD, W_pc, Pts, PPG) %>%
    rename(
      Season = season
    )
  # %>%
  #   rename_with(.cols = W:ppg,
  #               .fn = ~ paste0(input_venue, .x)
  #   )
  
  return (df)
}