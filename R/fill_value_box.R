get_box_values <- function(box_type, seasons) {
  box_type = noquote(box_type)
  seasons = noquote(seasons)
  
  if (box_type == "win_pc") {
    main_stat <- stringr::str_glue("{get_win_pc(seasons)}%")
    desc_1 <- "Games won"
    desc_2 <- "All selected seasons"
  }
  else if (box_type == "most_goals") {
    main_stat <- get_most_ssn_goals_number(seasons)
    desc_1 <- "Most goals in a season"
    desc_2 <- get_most_ssn_goals_name(seasons)
  }
  else if (box_type == "winning_streak") {
    main_stat <- get_winning_streak(seasons)
    desc_1 <- "Most consecutive wins"
    desc_2 <- get_winning_streak_ssns(seasons)
  }
  else if (box_type == "top_scorer") {
    main_stat <- get_top_scorer_goals(seasons)
    desc_1 <- "Top goalscorer"
    desc_2 <- get_top_scorer_name(seasons)
  }
  else if (box_type == "biggest_win") {
    main_stat <- get_biggest_win_score(seasons)
    desc_1 <- "Biggest win"
    desc_2 <- stringr::str_glue("v {get_biggest_win_opponent(seasons)}")
  }
  else if (box_type == "av_league_pts") {
    main_stat <- get_av_league_pts(seasons)
    desc_1 <- "Av. league points"
    desc_2 <- "All selected seasons"
  }
  else {
    main_stat <- "Unknown"
    desc_1 <- "Unknown"
    desc_2 <- "Unknown"
  }
  
  values <- list(main_stat, desc_1, desc_2)
  
  return (values)
}


fill_value_box <- function(box_type, seasons) {
  
  box_data <- get_box_values(box_type, seasons)
  main_stat <- box_data[[1]]
  desc_1 <- box_data[[2]]
  desc_2 <- box_data[[3]]
  
  stat_html <- htmltools::HTML(
    stringr::str_glue("<center>
                <b>
                  {main_stat}
                </b>
             </center>")
      )
  
  desc_html <- htmltools::HTML(
    stringr::str_glue("<center>
                <b>
                  {desc_1}
                </b><br/>
                
                  {desc_2}
              </center>")
    )
    
  html_output <- list(stat_html, desc_html)
  
  return (html_output)
}