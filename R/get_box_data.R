get_win_pc <- function(seasons) {
  df <- get_results_raw() %>%
    filter(season %in% seasons) %>%
    summarise(
      win_pc = sum(outcome == "W") / n(),
      win_pc = round(win_pc * 100, 1)
    )

  return (df)
}

get_most_ssn_goals <- function(season) {
  df <- get_ssn_scorers(season)

  return (df)
}

get_most_ssn_goals_name <- function(seasons) {
  df <- get_most_ssn_goals(seasons) %>%
    filter(
      total_goals == max(total_goals)
    ) %>%
    arrange(
      season, player_name
    ) %>%
    mutate(
      player_and_season = case_when(
        length(seasons) > 1 ~ stringr::str_glue("{player_name} ({season})"),
        TRUE ~ player_name
      )
    )
  
  if (length(seasons) > 1) {
    top_scorers <- paste0(df$player_and_season, collapse = ", ")
  } else {
    top_scorers <- paste0(df$player_name, collapse = ", ")
  }
  
  return(top_scorers)
}

get_most_ssn_goals_number <- function(season) {
  df <- get_most_ssn_goals(season)

  return (max(df$total_goals))
}

get_top_scorer_name <- function(season) {
  df <- get_ssn_scorers(season) %>%
    group_by(player_name) %>%
    summarise(
      total_goals = sum(total_goals)
    ) %>%
    filter(total_goals == max(total_goals))

  top_scorers <- paste0(df$player_name, collapse = ", ")

  return (top_scorers)
}

get_top_scorer_goals <- function(season) {
  df <- get_ssn_scorers(season) %>%
    group_by(player_name) %>%
    summarise(
      total_goals = sum(total_goals)
    )

  return (max(df$total_goals))
}

get_winning_streak <- function(season) {
  df <- get_streaks(season)

  return(max(df$Wins))
}

get_winning_streak_ssns <- function(season) {
  df <- get_streaks(season) %>%
    filter(
      Wins == max(Wins)
    ) %>%
    arrange(Season)
  
  win_streak_ssns <- paste0(df$Season, collapse = ", ")
  
  return(win_streak_ssns)
}

get_biggest_win <- function(seasons) {
  df <- get_results_raw() %>%
    filter(season %in% seasons) %>%
    mutate(gd = goals_for - goals_against) %>%
    arrange(desc(gd), desc(goals_for), date)

  return(df)
}

get_biggest_win_score <- function(seasons) {
  df <- get_biggest_win(seasons)

  return(df$score[[1]])
}

get_biggest_win_opponent <- function(seasons) {
  df <- get_biggest_win(seasons) %>%
    mutate(
      oppo_and_ssn = stringr::str_glue("{opponent} ({venue}), {season}")
    )
  
  biggest_win_oppo <- df$oppo_and_ssn[[1]]
  
  return(biggest_win_oppo)
}

get_av_league_pts <- function(seasons) {
  df <- get_results_raw() %>%
    group_by(season) %>%
    filter(
      season %in% seasons,
      game_type == "league",
      comp_game_no == max(comp_game_no)
    ) %>%
    ungroup()
  
  av_pts <- round(mean(df$ssn_pts), 1)
  
  return(av_pts)
}

get_av_league_pts(c("2022/23", "2021/22"))