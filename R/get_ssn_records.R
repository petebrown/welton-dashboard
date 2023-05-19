get_ssn_records <- function(seasons) {
  df <- get_results_raw() %>%
    filter(season %in% seasons)

  home_df <- df %>%
    filter(
      game_type == "league",
      venue == "H",
    ) %>%
    group_by(season) %>%
    summarise(
      P = n(),
      HW = sum(outcome == "W"),
      HD = sum(outcome == "D"),
      HL = sum(outcome == "L"),
      HGF = sum(goals_for),
      HGA = sum(goals_against),
    ) %>%
    mutate(
      H_pts = (HW * 3) + HD,
      HGD = HGF - HGA,
      HGF_HGA = paste(HGF, HGA, sep="-"),
      HW_pc = round((HW/P) * 100, 2),
      H_ppg = round((H_pts / P), 2)
    ) %>%
    select(season, HW, HD, HL, HGF, HGA, HGF_HGA, HGD, HW_pc, H_pts, H_ppg)

  away_df <- df %>%
    filter(
      game_type == "league",
      venue == "A",
    ) %>%
    group_by(season) %>%
    summarise(
      P = n(),
      AW = sum(outcome == "W"),
      AD = sum(outcome == "D"),
      AL = sum(outcome == "L"),
      AGF = sum(goals_for),
      AGA = sum(goals_against),
    ) %>%
    mutate(
      A_pts = (AW * 3) + AD,
      AGD = AGF - AGA,
      AGF_AGA = paste(AGF, AGA, sep="-"),
      AW_pc = round((AW/P) * 100, 2),
      A_ppg = round((A_pts / P), 2)
    ) %>%
    select(season, AW, AD, AL, AGF, AGA, AGF_AGA, AGD, AW_pc, A_pts, A_ppg)

  seasons_df <- inner_join(home_df, away_df, by = c("season")) %>%
    mutate(
      P = HW + HD + HL + AW + AD + AL,
      GF = (HGF + AGF),
      GA = (HGA + AGA),
      GF_GA = paste(GF, GA, sep = "-"),
      GD = (HGF + AGF) - (HGA + AGA),
      Pts = (HW * 3) + HD + (AW *3) + AD,
      PPG = round(Pts / P, 2)
    ) %>%
    select(season, P, HW, HD, HL, HGF, HGA, HGF_HGA, HGD, HW_pc, H_pts, H_ppg, AW, AD, AL, AGF, AGA, AGF_AGA, AGD, AW_pc, A_pts, A_ppg, GF, GA, GF_GA, GD, Pts, PPG)

  return (seasons_df)
}