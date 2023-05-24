# main_stat <- get_most_ssn_goals_number(input$season)
# desc_1 <- "Most goals in a season"
# desc_2 <- get_most_ssn_goals_name(input$season)

main_stat <- get_most_ssn_goals_number(c("2022/23"))
desc_1 <- "Most goals in a season"
desc_2 <- get_most_ssn_goals_name(c("2022/23"))


fill_value_box <- function(main_stat, desc_1, desc_2) {
  
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

fill_value_box(main_stat, desc_1, desc_2)