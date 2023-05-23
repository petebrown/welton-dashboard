source("./R/a_packages.R")
source("./R/get_data.R")
source("./R/get_box_data.R")
source("./R/get_table_headers.R")
source("./R/get_ssn_records.R")
source("./R/get_streaks.R")
source("./R/plot_ssn_pts.R")
source("./R/plot_ssn_ppg.R")
source("./R/plot_ssn_scorers.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "green",
                    title = "Welton Rovers",

  dashboardHeader(title = "Welton Rovers F.C."),

  # Sidebar panel for inputs
  dashboardSidebar(
    # First drop-down where a season is selected
    selectInput(
      "season", "Select season(s):", get_season_list(),
      selected = "2022/23",
      multiple = TRUE
    )
  ),

  dashboardBody(
    tags$head(
      includeCSS(path = "www/style.css")
    ),

    h1("Quick Facts"),

    fluidRow(
      valueBoxOutput("win_pc"),

      valueBoxOutput("most_goals"),

      valueBoxOutput("winning_streak")
    ),

    fluidRow(
      valueBoxOutput("top_scorer"),

      valueBoxOutput("biggest_win"),

      valueBoxOutput("approvalBox2")
    ),

    h1("Point Accumulation"),
    plotlyOutput("pts_plot"),

    hr(),

    h1("Points-per-Game"),
    plotlyOutput("ppg_plot"),

    hr(),

    h1("League Records"),
    
    DT::dataTableOutput("ssn_records_all"),

    br(),
    h3("Home"),
    DT::dataTableOutput("ssn_records_home"),
    
    br(),
    h3("Away"),
    DT::dataTableOutput("ssn_records_away"),
    
    hr(),

    h1("Streaks"),
    DT::dataTableOutput("streaks_table"),

    hr(),

    h1("Results by Season"),
    uiOutput("ssn_tabs"),

    hr(),

    # h1("All Results"),
    # DT::dataTableOutput("results_table"),
    #
    # hr(),

    h1("Top Scorers"),
    plotlyOutput("scorers_plot")
  )
)

# Define server logic requir`ed to draw a histogram
server <- function(input, output, session) {

  output$win_pc <- renderValueBox({
    valueBox(
      paste0(get_win_pc(input$season), "%"), "Games won", icon = icon("futbol"),
      color = "green"
    )
  })

  output$most_goals <- renderValueBox({
    valueBox(
      get_most_ssn_goals_number(input$season), paste0("Most goals in a season", " (", get_most_ssn_goals_name(input$season), ")"), icon = icon("futbol"),
      color = "olive"
    )
  })

  output$winning_streak <- renderValueBox({
    valueBox(
      get_winning_streak(input$season), "Most consecutive wins", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })

  output$top_scorer <- renderValueBox({
    valueBox(
      get_top_scorer_goals(input$season), paste0("Most goals in all selected seasons\n", "(", get_top_scorer_name(input$season), ")"), icon = icon("user", lib = "glyphicon"),
      color = "olive"
    )
  })

  output$biggest_win <- renderValueBox({
    valueBox(
      get_biggest_win_score(input$season), stringr::str_glue("Biggest win (v {get_biggest_win_opponent(input$season)})"), icon = icon("futbol", lib = "glyphicon"),
      color = "green"
    )
  })

  output$pts_plot <- renderPlotly(
    plot_ssn_pts(input$season)
  )

  output$ppg_plot <- renderPlotly(
    plot_ssn_ppg(input$season)
  )
  
  output$ssn_records_all <- DT::renderDataTable(
    get_ssn_records(input$season),
    rownames = FALSE,
    # container = get_table_headers(),
    options = list(paging = FALSE,    ## paginate the output
                   pageLength = 10,  ## number of rows to output for each page
                   scrollX = TRUE,   ## enable scrolling on X axis
                   scrollY = TRUE,   ## enable scrolling on Y axis
                   autoWidth = FALSE, ## use smart column width handling
                   server = FALSE,   ## use client-side processing
                   dom = 'rtp')
  )
  
  output$ssn_records_home <- DT::renderDataTable(
    get_ssn_records(input$season, "H"),
    rownames = FALSE,
    # container = get_table_headers(),
    options = list(paging = FALSE,    ## paginate the output
                   pageLength = 10,  ## number of rows to output for each page
                   scrollX = TRUE,   ## enable scrolling on X axis
                   scrollY = TRUE,   ## enable scrolling on Y axis
                   autoWidth = FALSE, ## use smart column width handling
                   server = FALSE,   ## use client-side processing
                   dom = 'rtp')
  )
  
  output$ssn_records_away <- DT::renderDataTable(
    get_ssn_records(input$season, "A"),
    rownames = FALSE,
    # container = get_table_headers(),
    options = list(paging = FALSE,    ## paginate the output
                   pageLength = 10,  ## number of rows to output for each page
                   scrollX = TRUE,   ## enable scrolling on X axis
                   scrollY = TRUE,   ## enable scrolling on Y axis
                   autoWidth = FALSE, ## use smart column width handling
                   server = FALSE,   ## use client-side processing
                   dom = 'rtp')
  )

  output$streaks_table <- DT::renderDataTable(
    get_streaks(input$season),
    rownames = FALSE,
    options = list(
      pageLength = 5,
      scrollX = TRUE,
      dom = 'tip',
      info = FALSE,
      paging = FALSE
      )
  )

  output_ssn_results <- function(season) {
    DT::renderDataTable(filter_results(season) %>% mutate(date = format(date, format = "%d %b %Y")),
                        options = list(paging = TRUE,    ## paginate the output
                                       pageLength = 10,  ## number of rows to output for each page
                                       info = TRUE,
                                       scrollX = TRUE,   ## enable scrolling on X axis
                                       scrollY = TRUE,   ## enable scrolling on Y axis
                                       autoWidth = FALSE, ## use smart column width handling
                                       server = FALSE,   ## use client-side processing
                                       dom = 'frtip',
                                       columnDefs = list(list(targets = c(0, 2, 3, 6, 7, 8, 10, 11), className = 'dt-left'),
                                                         list(targets = c(1, 4, 5), className = 'dt-center'),
                                                         list(targets = c(9), className = 'dt-right'))
                        ),
                        extensions = 'Buttons',
                        selection = 'single', ## enable selection of a single row
                        filter = 'bottom',              ## include column filters at the bottom
                        rownames = FALSE                ## don't show row numbers/names
    )
  }

  # Dynamically render the tab panels based on user input
  output$ssn_tabs <- renderUI({
    if (!is.null(input$season)) {
      # Get selected seasons
      selected_seasons <- sort(input$season, decreasing = TRUE)

      # Create a tab panel for each selected season
      ssn_tabs <- lapply(selected_seasons, function(season) {
        tabPanel(title = season,
                 fluidRow(
                   output_ssn_results(season)
                 )
        )
      })

      # Return the tabsetPanel containing season results
      do.call(tabsetPanel, ssn_tabs)
    } else {
      p("Please select one or more seasons from the dropdown menu.")
    }
  })

  output$scorers_plot <- renderPlotly(
    plot_ssn_scorers(input$season)
  )
}

# Run the application
shinyApp(ui = ui, server = server)
