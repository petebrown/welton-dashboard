get_box_values2 <- function(box_type, seasons) {
  if (box_type == "win_pc") {
    values <- "1"
    # values <- list(get_win_pc(seasons), "Games won", "All selected seasons")
  } else if (box_type == "most_goals") {
    print("2")
    # values <- list(get_most_ssn_goals_number(seasons), "Most goals in a season", get_most_ssn_goals_name(seasons))
  } else if (box_type == "winning_streak") {
    print("3")
    # values <- list(get_winning_streak(seasons), "Most consecutive wins", get_winning_streak_ssns(seasons))
  } else {
    print("4")
    # values <- list("nope", "nope", "nope")
  }
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

get_box_values <- function(box_type, seasons) {
  if (box_type == "win_pc") {
    values <- list(get_win_pc(seasons), "Games won", "All selected seasons")
  }
  return (values)
}

get_box_values2(box_type = "win_pc", seasons = "made up")