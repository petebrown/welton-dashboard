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

get_most_ssn_goals_name <- function(season) {
  df <- get_most_ssn_goals(season) %>%
    filter(
      total_goals == max(total_goals)
    ) %>%
    arrange(
      desc(player_name)
    )

  top_scorers <- paste0(df$player_name, collapse = ", ")

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

get_biggest_win <- function(seasons) {
  df <- get_results_raw() %>%
    filter(season %in% seasons) %>%
    mutate(gd = goals_for - goals_against) %>%
    arrange(desc(gd), desc(goals_for))

  return(df)
}

get_biggest_win_score <- function(seasons) {
  df <- get_biggest_win(seasons)

  return(df$score[[1]])
}

get_biggest_win_opponent <- function(seasons) {
  df <- get_biggest_win(seasons)

  return(df$opponent[[1]])
}
