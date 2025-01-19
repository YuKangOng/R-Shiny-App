library(shiny)
library(dplyr)
library(plotly)
library(DT)
library(leaflet)
library(shinyWidgets)
library(shinythemes)

#read csv
players <- read.csv("players.csv")
teams <- read.csv("teams.csv")
players_shooting <- read.csv("players_shooting_pct.csv")


#factor seasons by levels
players$Season <- factor(players$Season, levels = c("2014-15","2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23", "2023-24"))
teams$Season <- factor(teams$Season, levels= c("2014-15","2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23", "2023-24"))

#factor categorical variables
players$All_Star <- factor(players$All_Star, labels = c("No", "Yes"))
teams$NBA_Championships <- factor(teams$NBA_Championships, labels = c("No", "Yes"))



#color code for each teams
team_colours <- c(
  "Atlanta Hawks" = "#C8102E",
  "Boston Celtics" = "#007A33",
  "Brooklyn Nets" = "#000000",
  "Charlotte Hornets" = "#1D1160",
  "Chicago Bulls" = "#CE1141",
  "Cleveland Cavaliers" = "#860038",
  "Dallas Mavericks" = "#00538C",
  "Denver Nuggets" = "#0E2240",
  "Detroit Pistons" = "#C8102E",
  "Golden State Warriors" = "#1D428A",
  "Houston Rockets" = "#CE1141",
  "Indiana Pacers" = "#002D62",
  "Los Angeles Clippers" = "#C8102E",
  "Los Angeles Lakers" = "#552583",
  "Memphis Grizzlies" = "#5D76A9",
  "Miami Heat" = "#98002E",
  "Milwaukee Bucks" = "#00471B",
  "Minnesota Timberwolves" = "#0C2340",
  "New Orleans Pelicans" = "#0C2340",
  "New York Knicks" = "#006BB6",
  "Oklahoma City Thunder" = "#007AC1",
  "Orlando Magic" = "#0077C0",
  "Philadelphia 76ers" = "#006BB6",
  "Phoenix Suns" = "#1D1160",
  "Portland Trail Blazers" = "#E03A3E",
  "Sacramento Kings" = "#5A2D81",
  "San Antonio Spurs" = "#C4CED4",
  "Toronto Raptors" = "#CE1141",
  "Utah Jazz" = "#002B5C",
  "Washington Wizards" = "#002B5C"
)



# dataset for first page, group players that played for different teams in the same season
data1 <- players %>%
  group_by(Season, Player, Age, All_Star) %>%
  summarise(
    Team = paste(Team, collapse = ", "),
    Position = ifelse(n_distinct(Position) > 1, paste(unique(Position), collapse = ", "), unique(Position)),
    Games_Started = sum(Games_Started),
    Minutes_Played = round(sum(Minutes_Played * Games) / sum(Games), 1),
    Field_Goals = round(sum(Field_Goals * Games) / sum(Games), 1),
    Field_Goal_Attempts = round(sum(Field_Goal_Attempts * Games) / sum(Games), 1),
    Field_Goal_Percentage = round(sum(Field_Goal_Percentage * Games) / sum(Games), 3),
    Three_Point_Field_Goals = round(sum(Three_Point_Field_Goals * Games) / sum(Games), 1),
    Three_Point_Field_Goal_Attempts = round(sum(Three_Point_Field_Goal_Attempts * Games) / sum(Games), 1),
    Three_Point_Field_Goal_Percentage = round(sum(Three_Point_Field_Goal_Percentage * Games) / sum(Games), 3),
    Two_Point_Field_Goals = round(sum(Two_Point_Field_Goals * Games) / sum(Games), 1),
    Two_Point_Field_Goal_Attempts = round(sum(Two_Point_Field_Goal_Attempts * Games) / sum(Games), 1),
    Two_Point_Field_Goal_Percentage = round(sum(Two_Point_Field_Goal_Percentage * Games) / sum(Games), 3),
    Effective_Field_Goal_Percentage = round(sum(Effective_Field_Goal_Percentage * Games) / sum(Games), 3),
    Free_Throws = round(sum(Free_Throws * Games) / sum(Games), 1),
    Free_Throw_Attempts = round(sum(Free_Throw_Attempts * Games) / sum(Games), 1),
    Free_Throw_Percentage = round(sum(Free_Throw_Percentage * Games) / sum(Games), 3),
    Offensive_Rebounds = round(sum(Offensive_Rebounds * Games) / sum(Games), 1),
    Defensive_Rebounds = round(sum(Defensive_Rebounds * Games) / sum(Games), 1),
    Total_Rebounds = round(sum(Total_Rebounds * Games) / sum(Games), 1),
    Assists = round(sum(Assists * Games) / sum(Games), 1),
    Steals = round(sum(Steals * Games) / sum(Games), 1),
    Blocks = round(sum(Blocks * Games) / sum(Games), 1),
    Turnovers = round(sum(Turnovers * Games) / sum(Games), 1),
    Personal_Fouls = round(sum(Personal_Fouls * Games) / sum(Games), 1),
    Points = round(sum(Points * Games) / sum(Games), 1),
    Games = sum(Games),
    .groups = "drop"
    
  )

# dataset for 6th page, group players that played for different teams in the same season
data6 <- players_shooting %>%
  group_by(Season, Player, Age) %>%
  summarise(
    Team = paste(Team, collapse = ", "),
    Position = ifelse(n_distinct(Position) > 1, paste(unique(Position), collapse = ", "), unique(Position)),
    Games_Started = sum(Games_Started),
    Minutes_Played = sum(Minutes_Played),
    Field_Goal_Percentage = round(sum(Field_Goal_Percentage * Games) / sum(Games), 3),
    FG_percentage_on_FGAs_that_are_0_to_3_feet_from_the_basket = round(sum(FG_percentage_on_FGAs_that_are_0_to_3_feet_from_the_basket * Games) / sum(Games), 3),
    FG_percentage_on_FGAs_that_are_3_to_10_feet_from_the_basket = round(sum(FG_percentage_on_FGAs_that_are_3_to_10_feet_from_the_basket * Games) / sum(Games), 3),
    FG_percentage_on_FGAs_that_are_10_to_16_feet_from_the_basket = round(sum(FG_percentage_on_FGAs_that_are_10_to_16_feet_from_the_basket * Games) / sum(Games), 3),
    FG_percentage_on_FGAs_that_are_more_than_16_feet_from_the_basket = round(sum(FG_percentage_on_FGAs_that_are_more_than_16_feet_from_the_basket * Games) / sum(Games), 3),
    FG_percentage_on_FGAs_that_are_3_Pt_FGAs = round(sum(FG_percentage_on_FGAs_that_are_3_Pt_FGAs * Games) / sum(Games), 3),
    Games = sum(Games),
    .groups = "drop"
    
  )




ui <- navbarPage("NBA Stats for Nerds",
        theme = shinytheme("cerulean"),
        tabPanel("Players",
                titlePanel("Time Series of Players Stats Per Game"),
                sidebarPanel(
                  uiOutput("playerPicker1"),
                  checkboxGroupInput(
                    inputId = "season1", 
                    label = "Select Seasons", 
                    choices = sort(unique(data1$Season)), 
                    selected = unique(data1$Season), 
                    inline = TRUE
                    ),
                  selectInput(
                    inputId = "stats1",
                    label = "Select Stats:",
                    selected = "Points",
                    choices = c(
                      "Minutes_Played",
                      "Field_Goals",
                      "Field_Goal_Attempts",
                      "Field_Goal_Percentage",
                      "Three_Point_Field_Goals",
                      "Three_Point_Field_Goal_Attempts",
                      "Three_Point_Field_Goal_Percentage",
                      "Two_Point_Field_Goals",
                      "Two_Point_Field_Goal_Attempts",
                      "Two_Point_Field_Goal_Percentage",
                      "Effective_Field_Goal_Percentage",
                      "Free_Throws",
                      "Free_Throw_Attempts",
                      "Free_Throw_Percentage",
                      "Offensive_Rebounds",
                      "Defensive_Rebounds",
                      "Total_Rebounds",
                      "Assists",
                      "Steals",
                      "Blocks",
                      "Turnovers",
                      "Personal_Fouls",
                      "Points"
                      )
                    ),
                  materialSwitch(
                    inputId = "leaders1",
                    label = "Show Season Leaders (minimum 58 games)",
                    status = "primary"
                    )
                  ),
                mainPanel(
                  plotlyOutput(outputId = "lineplot1"),
                  HTML("<div style='height:40px;'></div>"), # add some space
                  DTOutput("table1")
                )
        ),
        tabPanel("Teams",
                 titlePanel("Time Series of Teams Stats Per Game"),
                 sidebarPanel(
                   uiOutput("teamPicker"),
                   checkboxGroupInput(
                     inputId = "season2", 
                     label = "Select Seasons", 
                     choices = sort(unique(data1$Season)), 
                     selected = unique(data1$Season), 
                     inline = TRUE
                   ),
                   selectInput(
                     inputId = "stats2",
                     label = "Select Stats:",
                     selected = "Points",
                     choices = c(
                       "Minutes_Played",
                       "Field_Goals",
                       "Field_Goal_Attempts",
                       "Field_Goal_Percentage",
                       "Three_Point_Field_Goals",
                       "Three_Point_Field_Goal_Attempts",
                       "Three_Point_Field_Goal_Percentage",
                       "Two_Point_Field_Goals",
                       "Two_Point_Field_Goal_Attempts",
                       "Two_Point_Field_Goal_Percentage",
                       "Effective_Field_Goal_Percentage",
                       "Free_Throws",
                       "Free_Throw_Attempts",
                       "Free_Throw_Percentage",
                       "Offensive_Rebounds",
                       "Defensive_Rebounds",
                       "Total_Rebounds",
                       "Assists",
                       "Steals",
                       "Blocks",
                       "Turnovers",
                       "Personal_Fouls",
                       "Points"
                     )
                   ),
                   materialSwitch(
                     inputId = "leaders2",
                     label = "Show Season Leaders",
                     status = "primary"
                   )
                 ),
                 mainPanel(
                   plotlyOutput(outputId = "lineplot2"),
                   HTML("<div style='height:40px;'></div>"), # add some space
                   DTOutput("table2")
                 )
        ),
        tabPanel("Players Efficiency",
                 titlePanel("Players Efficiency"),
                 sidebarPanel(
                   checkboxGroupInput(
                     inputId = "season3", 
                     label = "Select Seasons", 
                     choices = sort(unique(players$Season)), 
                     selected = "2023-24", 
                     inline = TRUE
                   ),
                   actionButton("select_all3", "Select All"),
                   actionButton("deselect_all3", "Deselect All"),
                   pickerInput(
                     inputId = "team3",
                     label = "Select Teams:",
                     selected = unique(players$Team),
                     choices = sort(unique(players$Team)),
                     options = pickerOptions(actionsBox = TRUE, size = 10),
                     multiple = TRUE
                   ),
                   checkboxGroupInput(
                     inputId = "position3", 
                     label = "Select Positions", 
                     choices = c("PG", "SG", "SF", "PF", "C"), 
                     selected = unique(players$Position), 
                     inline = TRUE
                   ),
                   sliderInput(
                     inputId = "age3",
                     label = "Select Age",
                     value = c(min(players$Age), max(players$Age)),
                     min = min(players$Age),
                     max = max(players$Age)
                     ),
                   materialSwitch(
                     inputId = "all_star3",
                     label = "Show All Stars Only",
                     status = "primary"
                     )
                 ),
                 mainPanel(
                   plotlyOutput(outputId = "scatterplot3_1"),
                   HTML("<div style='height:40px;'></div>"), # add some space
                   plotlyOutput(outputId = "scatterplot3_2"),
                   HTML("<div style='height:40px;'></div>"), # add some space
                   plotlyOutput(outputId = "scatterplot3_3"),
                   HTML("<div style='height:40px;'></div>"), # add some space
                   plotlyOutput(outputId = "scatterplot3_4"),
                   HTML("<div style='height:40px;'></div>") # add some space
                 )
        ),
        tabPanel("Teams Efficiency",
                 titlePanel("Teams Efficiency"),
                 sidebarPanel(
                   checkboxGroupInput(
                     inputId = "season4", 
                     label = "Select Seasons", 
                     choices = sort(unique(teams$Season)), 
                     selected = "2023-24",
                     inline = TRUE
                   ),
                   actionButton("select_all4", "Select All"),
                   actionButton("deselect_all4", "Deselect All"),
                   pickerInput(
                     inputId = "team4",
                     label = "Select Teams:",
                     selected = unique(teams$Team),
                     choices = sort(unique(teams$Team)),
                     options = pickerOptions(actionsBox = TRUE, size = 10),
                     multiple = TRUE
                   ),
                   sliderInput(
                     inputId = "wins4",
                     label = "Select Number of Wins",
                     value = c(min(teams$Wins), max(teams$Wins)),
                     min = min(teams$Wins),
                     max = max(teams$Wins)
                   ),
                   materialSwitch(
                     inputId = "championship4",
                     label = "Show Teams That Have Won Championships",
                     status = "primary"
                     )
                 ),
                 mainPanel(
                   plotlyOutput(outputId = "scatterplot4_1"),
                   HTML("<div style='height:40px;'></div>"), # add some space
                   plotlyOutput(outputId = "scatterplot4_2"),
                   HTML("<div style='height:40px;'></div>"), # add some space
                   plotlyOutput(outputId = "scatterplot4_3"),
                   HTML("<div style='height:40px;'></div>"), # add some space
                   plotlyOutput(outputId = "scatterplot4_4"),
                   HTML("<div style='height:40px;'></div>") # add some space
                 )
              
        ),
        tabPanel("Arenas",
                 titlePanel("Show Teams Location"),
                 sidebarPanel(
                   checkboxGroupInput(
                     inputId = "season5", 
                     label = "Select Seasons", 
                     choices = sort(unique(teams$Season)), 
                     selected = "2023-24", 
                     inline = TRUE
                   ),
                   pickerInput(
                     inputId = "team5",
                     label = "Select Teams:",
                     selected = unique(teams$Team),
                     choices = sort(unique(teams$Team)),
                     options = pickerOptions(actionsBox = TRUE, size = 10),
                     multiple = TRUE
                   )
                 ),
                 mainPanel(
                   leafletOutput("map", height="400px"),
                   HTML("<div style='height:40px;'></div>"), # add some space
                   DTOutput("table5")
                 )
        ),
        tabPanel("Shooting Percentage",
                 titlePanel("Shooting Percentage of Players from various spots on the court"),
                 sidebarPanel(
                   awesomeRadio(
                     inputId = "season6", 
                     label = "Select Seasons", 
                     choices = sort(unique(players_shooting$Season)), 
                     selected = "2023-24",
                     inline = TRUE
                   ),
                   uiOutput("playerPicker6")
                 ),
                 mainPanel(
                   plotlyOutput(outputId = "shootinggraph6"),
                   HTML("<div style='height:40px;'></div>"), # add some space
                   DTOutput("table6")
                )
        ),
        tabPanel("All-Star Predictor",
                 titlePanel("Predict Whether a Player will make the All-Star Team"),
                 sidebarPanel(
                   numericInput( 
                     inputId = "threes", 
                     label = "Enter Three Points Made Per Game",
                     value = 0,
                     min = 0
                     ),
                   numericInput( 
                     inputId = "twos", 
                     label = "Enter Two Points Made Per Game",
                     value = 0,
                     min = 0
                   ),
                   numericInput( 
                     inputId = "fts", 
                     label = "Enter Free Throws Made Per Game",
                     value = 0,
                     min = 0
                   ),
                   numericInput( 
                     inputId = "rbds", 
                     label = "Enter Defensive Rebounds Per Game",
                     value = 0,
                     min = 0
                   ),
                   numericInput( 
                     inputId = "asts", 
                     label = "Enter Assists Per Game",
                     value = 0,
                     min = 0
                   ),
                   numericInput( 
                     inputId = "stls", 
                     label = "Enter Steals Per Game",
                     value = 0,
                     min = 0
                   ),
                   numericInput( 
                     inputId = "blks", 
                     label = "Enter Blocks Per Game",
                     value = 0,
                     min = 0
                   ),
                   numericInput( 
                     inputId = "tos", 
                     label = "Enter Turnovers Per Game",
                     value = 0,
                     min = 0
                   )
                 ),
                 mainPanel(
                   div(style = "text-align: center;", 
                       h3("Prediction Result"),
                       h4(textOutput("predict1")),
                       uiOutput("predict2")
                   ),
                )
        )
)



server <- function(input, output, session) {
  output$playerPicker1 <- renderUI({
    if (!input$leaders1) {
      pickerInput(
        inputId = "name1",
        label = "Select Players:",
        selected = "Nikola Jokić",
        choices = sort(unique(data1$Player)),
        multiple = TRUE
      )
    } else {
      # If input$leaders1 is TRUE, return NULL to remove the picker
      NULL
    }
  })
  
  # Reactive filtering of the data
  filtered_data1 <- reactive({
    if (input$leaders1) {
      req(input$season1)
      data1 %>%
        filter(Season %in% input$season1) %>%
        group_by(Season) %>%
        filter(Games >= 58) %>%
        filter(!!sym(input$stats1) == max(!!sym(input$stats1), na.rm = TRUE)) %>%
        ungroup() %>%
        summarise(Season, Team, Player, Age, Position, Games, Games_Started, !!sym(input$stats1))
    } else {
        req(input$name1) # Ensure input is available
        req(input$season1)
        data1 %>%
          filter(Player %in% input$name1) %>%
          filter(Season %in% input$season1) %>%
          summarise(Season, Team, Player, Age, Position, Games, Games_Started, !!sym(input$stats1))
    } 
  })
  
  # Render the lineplot based on filtered data
  output$lineplot1 <- renderPlotly({
    dataset1 <- filtered_data1() # Get the filtered dataset
    selected_stat1 <- input$stats1
    LinePlot1 <-
      if (input$leaders1) {
        ggplot(data=dataset1, aes(x=Season, y=!!sym(selected_stat1), group=1,
                                  text = paste(
                                    "Player:", Player, "<br>",
                                    "Team:", Team, "<br>",
                                    "Position:", Position, "<br>",
                                    "Age:", Age, "<br>",
                                    "Games Played:", Games))) + 
          geom_line() +
          geom_point() +
          labs(x = "Season", y = selected_stat1, title = paste("Players", selected_stat1, "Per Game by Season"))
      } else {
        ggplot(data=dataset1, aes(x=Season, y=!!sym(selected_stat1), color=Player, group=1,
                                 text = paste(
                                   "Team:", Team, "<br>",
                                   "Position:", Position, "<br>",
                                   "Age:", Age, "<br>",
                                   "Games Played:", Games))) + 
        geom_line() +
        geom_point() +
        labs(x = "Season", y = selected_stat1, title = paste("Players", selected_stat1, "Per Game by Season")) +
        theme_minimal() +
        theme(legend.position = "right")
      }
    
    ggplotly(LinePlot1)
  })
  
  output$table1 <- renderDT({
    datatable(filtered_data1(),
              options = list(pageLength = 10),
              filter = "top")
  })
  
  
  output$teamPicker <- renderUI({
    if (!input$leaders2) {
      pickerInput(
        inputId = "team2",
        label = "Select Teams:",
        selected = "Boston Celtics",
        choices = sort(unique(teams$Team)),
        options = pickerOptions(actionsBox = TRUE, size = 10),
        multiple = TRUE
      )
    } else {
        # If input$leaders2 is TRUE, return NULL to remove the picker
        NULL
    }
  })
  
  # Reactive filtering of the data
  filtered_data2 <- reactive({
    if (input$leaders2) {
      req(input$season2)
      teams %>%
        filter(Season %in% input$season2) %>%
        group_by(Season) %>%
        filter(!!sym(input$stats2) == max(!!sym(input$stats2), na.rm = TRUE)) %>%
        ungroup() %>%
        summarise(Season, Team, Wins, Losses, !!sym(input$stats2))
    } else {
      req(input$team2) # Ensure input is available
      req(input$season2)
      teams %>%
        filter(Team %in% input$team2) %>%
        filter(Season %in% input$season2) %>%
        summarise(Season, Team, Wins, Losses, !!sym(input$stats2))
    }
  })
  
  output$lineplot2 <- renderPlotly({
    dataset2 <- filtered_data2() # Get the filtered dataset
    selected_stat2 <- input$stats2
    LinePlot2 <-
      if (input$leaders2) {
        ggplot(data=dataset2, aes(x=Season, y=!!sym(selected_stat2), group=1,
                                  text = paste(
                                    "Team:", Team, "<br>",
                                    "Record:", paste(Wins,"-",Losses)))) + 
          geom_line() +
          geom_point() +
          scale_color_manual(values = team_colours) +
          labs(x = "Season", y = selected_stat2, title = paste("Teams", selected_stat2, "Per Game by Season"))
      } else {
        ggplot(data=dataset2, aes(x=Season, y=!!sym(selected_stat2), color=Team, group=1,
                                 text = paste(
                                   "Record:", paste(Wins,"-",Losses)))) + 
        geom_line() +
        geom_point() +
        scale_color_manual(values = team_colours) +
        labs(x = "Season", y = selected_stat2, title = paste("Teams", selected_stat2, "Per Game by Season")) +
        theme_minimal() +
        theme(legend.position = "right")
      }
    
    ggplotly(LinePlot2)
  })
  
  output$table2 <- renderDT({
    datatable(filtered_data2(),
              options = list(pageLength = 10),
              filter = "top")
  })
  
  observeEvent(input$select_all3, {
    updateCheckboxGroupInput(
      session, 
      "season3", 
      selected = sort(unique(players$Season))
    )
  })
  
  observeEvent(input$deselect_all3, {
    updateCheckboxGroupInput(session, "season3", selected = character(0))
  })
  
  filtered_data3 <- reactive({
    req(input$position3)
    req(input$team3)# Ensure input is available
    if (input$all_star3) {
      players %>%
        filter(Season %in% input$season3) %>%
        filter(Team %in% input$team3) %>%
        filter(Position %in% input$position3) %>%
        filter(Age >= input$age3[1] & Age <= input$age3[2]) %>%
        filter(All_Star == "Yes")
    } else {
      players %>%
        filter(Season %in% input$season3) %>%
        filter(Team %in% input$team3) %>%
        filter(Position %in% input$position3) %>%
        filter(Age >= input$age3[1] & Age <= input$age3[2])
    }
  })
  
  output$scatterplot3_1 <- renderPlotly({
    dataset3_1 <- filtered_data3() # Get the filtered dataset
    ScatterPlot3_1 <-
      ggplot(data=dataset3_1, aes(x=Field_Goal_Attempts, y=Field_Goals, color=Team, group=1,
                                  text = paste(
                                    "Season:", Season, "<br>",
                                    "Player:", Player, "<br>",
                                    "Games Played:", Games, "<br>",
                                    "FG%:", Field_Goal_Percentage))) + 
      geom_point() + 
      expand_limits(x = 0, y = 0) +
      geom_abline(slope=0.5, intercept=0) +
      scale_color_manual(values = team_colours) +
      labs(x = "Field Goal Attempts", y = "Field Goal Made", title = "Field Goal Attempts vs Field Goal Made") +
      theme_minimal() +
      theme(legend.position = "right")
    
    ggplotly(ScatterPlot3_1)
  })
  
  output$scatterplot3_2 <- renderPlotly({
    dataset3_2 <- filtered_data3() # Get the filtered dataset
    ScatterPlot3_2 <-
      ggplot(data=dataset3_2, aes(x=Three_Point_Field_Goal_Attempts, y=Three_Point_Field_Goals, color=Team, group=1,
                                  text = paste(
                                    "Season:", Season, "<br>",
                                    "Player:", Player, "<br>",
                                    "Games Played:", Games, "<br>",
                                    "3P%:", Three_Point_Field_Goal_Percentage))) + 
      geom_point() +
      expand_limits(x = 0, y = 0) +
      geom_abline(slope=0.4, intercept=0) +
      scale_color_manual(values = team_colours) +
      labs(x = "Three Point Attempts", y = "Three Point Made", title = "Three Point Attempts vs Three Point Made") +
      theme_minimal() +
      theme(legend.position = "right")
    
    ggplotly(ScatterPlot3_2)
  })
  
  output$scatterplot3_3 <- renderPlotly({
    dataset3_3 <- filtered_data3() # Get the filtered dataset
    ScatterPlot3_3 <-
      ggplot(data=dataset3_3, aes(x=Free_Throw_Attempts, y=Free_Throws, color=Team, group=1,
                                  text = paste(
                                    "Season:", Season, "<br>",
                                    "Player:", Player, "<br>",
                                    "Games Played:", Games, "<br>",
                                    "FT%:", Free_Throw_Percentage))) + 
      geom_point() +
      expand_limits(x = 0, y = 0) +
      geom_abline(slope=0.9, intercept=0) +
      scale_color_manual(values = team_colours) +
      labs(x = "Free Throw Attempts", y = "Free Throw Made", title = "Free Throw Attempts vs Free Throw Made") +
      theme_minimal() +
      theme(legend.position = "right")
    
    ggplotly(ScatterPlot3_3)
  })
  
  output$scatterplot3_4 <- renderPlotly({
    dataset3_4 <- filtered_data3() # Get the filtered dataset
    ScatterPlot3_4 <-
      ggplot(data=dataset3_4, aes(x=Assists, y=Turnovers, color=Team, group=1,
                                  text = paste(
                                    "Season:", Season, "<br>",
                                    "Player:", Player, "<br>",
                                    "Games Played:", Games, "<br>",
                                    "Asts/TO Ratio:", round(Assists/Turnovers, 1)))) + 
      geom_point() +
      expand_limits(x = 0, y = 0) +
      geom_abline(slope=1/3, intercept=0) +
      scale_color_manual(values = team_colours) +
      labs(x = "Assists", y = "Turnovers", title = "Assists vs Turnovers") +
      theme_minimal() +
      theme(legend.position = "right")
    
    ggplotly(ScatterPlot3_4)
  })
  
  observeEvent(input$select_all4, {
    updateCheckboxGroupInput(
      session, 
      "season4", 
      selected = sort(unique(teams$Season))
    )
  })
  
  observeEvent(input$deselect_all4, {
    updateCheckboxGroupInput(session, "season4", selected = character(0))
  })
  
  filtered_data4 <- reactive({
    req(input$team4) # Ensure input is available
    if (input$championship4) {
    teams %>%
      filter(Season %in% input$season4) %>%
      filter(Team %in% input$team4) %>%
      filter(Wins >= input$wins4[1] & Wins <= input$wins4[2]) %>%
      filter(NBA_Championships == "Yes")
    } else {
      teams %>%
        filter(Season %in% input$season4) %>%
        filter(Team %in% input$team4) %>%
        filter(Wins >= input$wins4[1] & Wins <= input$wins4[2])
    }
  })
  
  output$scatterplot4_1 <- renderPlotly({
    dataset4_1 <- filtered_data4() # Get the filtered dataset
    ScatterPlot4_1 <-
      ggplot(data=dataset4_1, aes(x=Field_Goal_Attempts, y=Field_Goals, color=Team, group=1,
                                  text = paste(
                                    "Season:", Season, "<br>",
                                    "FG%:", Field_Goal_Percentage, "<br>",
                                    "Record:", paste(Wins,"-",Losses)))) + 
      geom_point() +
      #expand_limits(x = 0, y = 0) +
      geom_abline(slope=0.5, intercept=0) +
      scale_color_manual(values = team_colours) +
      labs(x = "Field Goal Attempts", y = "Field Goal Made", title = "Field Goal Attempts vs Field Goal Made") +
      theme_minimal() +
      theme(legend.position = "right")
    
    ggplotly(ScatterPlot4_1)
  })
  
  output$scatterplot4_2 <- renderPlotly({
    dataset4_2 <- filtered_data4() # Get the filtered dataset
    ScatterPlot4_2 <-
      ggplot(data=dataset4_2, aes(x=Three_Point_Field_Goal_Attempts, y=Three_Point_Field_Goals, color=Team, group=1,
                                  text = paste(
                                    "Season:", Season, "<br>",
                                    "3P%:", Three_Point_Field_Goal_Percentage, "<br>",
                                    "Record:", paste(Wins,"-",Losses)))) + 
      geom_point() +
      #expand_limits(x = 0, y = 0) +
      geom_abline(slope=0.4, intercept=0) +
      scale_color_manual(values = team_colours) +
      labs(x = "Three Point Attempts", y = "Three Point Made", title = "Three Point Attempts vs Three Point Made") +
      theme_minimal() +
      theme(legend.position = "right")
    
    ggplotly(ScatterPlot4_2)
  })
  
  output$scatterplot4_3 <- renderPlotly({
    dataset4_3 <- filtered_data4() # Get the filtered dataset
    ScatterPlot4_3 <-
      ggplot(data=dataset4_3, aes(x=Free_Throw_Attempts, y=Free_Throws, color=Team, group=1,
                                  text = paste(
                                    "Season:", Season, "<br>",
                                    "FT%:", Free_Throw_Percentage, "<br>",
                                    "Record:", paste(Wins,"-",Losses)))) + 
      geom_point() +
      #expand_limits(x = 0, y = 0) +
      geom_abline(slope=0.9, intercept=0) +
      scale_color_manual(values = team_colours) +
      labs(x = "Free Throw Attempts", y = "Free Throw Made", title = "Free Throw Attempts vs Free Throw Made") +
      theme_minimal() +
      theme(legend.position = "right")
    
    ggplotly(ScatterPlot4_3)
  })
  
  output$scatterplot4_4 <- renderPlotly({
    dataset4_4 <- filtered_data4() # Get the filtered dataset
    ScatterPlot4_4 <-
      ggplot(data=dataset4_4, aes(x=Assists, y=Turnovers, color=Team, group=1,
                                  text = paste(
                                    "Season:", Season, "<br>",
                                    "Asts/TO Ratio:", round(Assists/Turnovers, 1), "<br>",
                                    "Record:", paste(Wins,"-",Losses)))) + 
      geom_point() +
      #expand_limits(x = 0, y = 0) +
      geom_abline(slope=1/3, intercept=0) +
      scale_color_manual(values = team_colours) +
      labs(x = "Assists", y = "Turnovers", title = "Assists vs Turnovers") +
      theme_minimal() +
      theme(legend.position = "right")
    
    ggplotly(ScatterPlot4_4)
  })
  
  filtered_data5 <- reactive({
    req(input$team5) # Ensure input is available
    teams %>%
      filter(Season %in% input$season5) %>%
      filter(Team %in% input$team5)
  })
  
  data_of_click <- reactiveValues(clickedMarker = NULL, clickedTeam = NULL, clickedSeason = NULL)
  
  # Create a function to get image link based on team name
  get_team_image <- function(team_name) {
    # This is a placeholder. Replace with actual image links for each team
    image_links <- list(
      "Atlanta Hawks" = "https://upload.wikimedia.org/wikipedia/en/thumb/2/24/Atlanta_Hawks_logo.svg/400px-Atlanta_Hawks_logo.svg.png",
      "Boston Celtics" = "https://upload.wikimedia.org/wikipedia/en/thumb/8/8f/Boston_Celtics.svg/380px-Boston_Celtics.svg.png",
      "Brooklyn Nets" = "https://upload.wikimedia.org/wikipedia/en/thumb/4/40/Brooklyn_Nets_primary_icon_logo_2024.svg/340px-Brooklyn_Nets_primary_icon_logo_2024.svg.png",
      "Charlotte Hornets" = "https://upload.wikimedia.org/wikipedia/en/thumb/c/c4/Charlotte_Hornets_%282014%29.svg/440px-Charlotte_Hornets_%282014%29.svg.png",
      "Chicago Bulls" = "https://upload.wikimedia.org/wikipedia/en/thumb/6/67/Chicago_Bulls_logo.svg/400px-Chicago_Bulls_logo.svg.png",
      "Cleveland Cavaliers" = "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4b/Cleveland_Cavaliers_logo.svg/340px-Cleveland_Cavaliers_logo.svg.png",
      "Dallas Mavericks" = "https://upload.wikimedia.org/wikipedia/en/thumb/9/97/Dallas_Mavericks_logo.svg/420px-Dallas_Mavericks_logo.svg.png",
      "Denver Nuggets" = "https://upload.wikimedia.org/wikipedia/en/thumb/7/76/Denver_Nuggets.svg/400px-Denver_Nuggets.svg.png",
      "Detroit Pistons" = "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c9/Logo_of_the_Detroit_Pistons.svg/400px-Logo_of_the_Detroit_Pistons.svg.png",
      "Golden State Warriors" = "https://upload.wikimedia.org/wikipedia/en/thumb/0/01/Golden_State_Warriors_logo.svg/400px-Golden_State_Warriors_logo.svg.png",
      "Houston Rockets" = "https://upload.wikimedia.org/wikipedia/en/thumb/2/28/Houston_Rockets.svg/340px-Houston_Rockets.svg.png",
      "Indiana Pacers" = "https://upload.wikimedia.org/wikipedia/en/thumb/1/1b/Indiana_Pacers.svg/400px-Indiana_Pacers.svg.png",
      "Los Angeles Clippers" = "https://upload.wikimedia.org/wikipedia/en/thumb/e/ed/Los_Angeles_Clippers_%282024%29.svg/440px-Los_Angeles_Clippers_%282024%29.svg.png",
      "Los Angeles Lakers" = "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3c/Los_Angeles_Lakers_logo.svg/440px-Los_Angeles_Lakers_logo.svg.png",
      "Memphis Grizzlies" = "https://upload.wikimedia.org/wikipedia/en/thumb/f/f1/Memphis_Grizzlies.svg/380px-Memphis_Grizzlies.svg.png",
      "Miami Heat" = "https://upload.wikimedia.org/wikipedia/en/thumb/f/fb/Miami_Heat_logo.svg/400px-Miami_Heat_logo.svg.png",
      "Milwaukee Bucks" = "https://upload.wikimedia.org/wikipedia/en/thumb/4/4a/Milwaukee_Bucks_logo.svg/360px-Milwaukee_Bucks_logo.svg.png",
      "Minnesota Timberwolves" = "https://upload.wikimedia.org/wikipedia/en/thumb/c/c2/Minnesota_Timberwolves_logo.svg/400px-Minnesota_Timberwolves_logo.svg.png",
      "New Orleans Pelicans" = "https://upload.wikimedia.org/wikipedia/en/thumb/0/0d/New_Orleans_Pelicans_logo.svg/500px-New_Orleans_Pelicans_logo.svg.png",
      "New York Knicks" = "https://upload.wikimedia.org/wikipedia/en/thumb/2/25/New_York_Knicks_logo.svg/480px-New_York_Knicks_logo.svg.png",
      "Oklahoma City Thunder" = "https://upload.wikimedia.org/wikipedia/en/thumb/5/5d/Oklahoma_City_Thunder.svg/400px-Oklahoma_City_Thunder.svg.png",
      "Orlando Magic" = "https://upload.wikimedia.org/wikipedia/en/thumb/1/10/Orlando_Magic_logo.svg/440px-Orlando_Magic_logo.svg.png",
      "Philadelphia 76ers" = "https://upload.wikimedia.org/wikipedia/en/thumb/0/0e/Philadelphia_76ers_logo.svg/400px-Philadelphia_76ers_logo.svg.png",
      "Phoenix Suns" = "https://upload.wikimedia.org/wikipedia/en/thumb/d/dc/Phoenix_Suns_logo.svg/380px-Phoenix_Suns_logo.svg.png",
      "Portland Trail Blazers" = "https://upload.wikimedia.org/wikipedia/en/thumb/2/21/Portland_Trail_Blazers_logo.svg/440px-Portland_Trail_Blazers_logo.svg.png",
      "Sacramento Kings" = "https://upload.wikimedia.org/wikipedia/en/thumb/c/c7/SacramentoKings.svg/360px-SacramentoKings.svg.png",
      "San Antonio Spurs" = "https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/San_Antonio_Spurs.svg/480px-San_Antonio_Spurs.svg.png",
      "Toronto Raptors" = "https://upload.wikimedia.org/wikipedia/en/thumb/3/36/Toronto_Raptors_logo.svg/400px-Toronto_Raptors_logo.svg.png",
      "Utah Jazz" = "https://upload.wikimedia.org/wikipedia/en/thumb/5/52/Utah_Jazz_logo_2022.svg/460px-Utah_Jazz_logo_2022.svg.png",
      "Washington Wizards" = "https://upload.wikimedia.org/wikipedia/en/thumb/0/02/Washington_Wizards_logo.svg/400px-Washington_Wizards_logo.svg.png"
    )
    
    # Return the image link for the given team, or a default image if not found
    return(image_links[[team_name]])
  }
  
  output$map <- renderLeaflet({
    dataset5 <- filtered_data5()
    leaflet(data = dataset5) %>% 
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        clusterOptions = markerClusterOptions(),
        popup = paste("Season:", dataset5$Season, "<br>",
                      "Team:", dataset5$Team, "<br>",
                      "<img src='", sapply(dataset5$Team, get_team_image),
                      "' alt='logo not found' width='90px' height='90px'>")
      ) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 8
      )
  })
  
  # Store the click event
  observeEvent(input$map_marker_click, {
    data_of_click$clickedMarker <- input$map_marker_click
    clicked_data <- filtered_data5() %>%
      filter(Latitude == input$map_marker_click$lat,
             Longitude == input$map_marker_click$lng)
      data_of_click$clickedTeam <- clicked_data$Team # Pick the first row to avoid issues
      data_of_click$clickedSeason <- clicked_data$Season

  })
  
  output$table5 <- renderDT({
    req(data_of_click$clickedTeam, data_of_click$clickedSeason)
    
    players_filtered <- players %>%
      filter(Team == data_of_click$clickedTeam,
             Season == data_of_click$clickedSeason)
    
    # Check if filtering produces correct results
    validate(
      need(nrow(players_filtered) > 0, "No players found for this selection.")
    )
    
    datatable(
      players_filtered,
      options = list(pageLength = 10, scrollX = TRUE),
      filter = "top"
    )
  })
  

  
  output$playerPicker6 <- renderUI({
    pickerInput(
      inputId = "name6",
      label = "Select Players:",
      selected = "Nikola Jokić",
      choices = sort(unique(players_shooting[players_shooting$Season == input$season6, "Player"])),
      options = list(`live-search` = TRUE)
    )
  })
  
  filtered_data6 <- reactive({
    req(input$name6) # Ensure input is available
    req(input$season6)
    data6 %>%
      filter(Player == input$name6) %>%
      filter(Season == input$season6)
  })
  
  output$shootinggraph6 <- renderPlotly({
    dataset6 <- filtered_data6()
    
  #plot basketball court manually
  plot_court <- function(court_theme = court_themes$dark, use_short_three = FALSE, shooting_data) {
    if (use_short_three) {
      three_point_radius <- 22
      three_point_side_height <- 0
    }
    
    court_points <- tibble(
      x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
      y = c(height, 0, 0, height, height),
      desc = "perimeter"
    )
    
    court_points <- bind_rows(court_points , tibble(
      x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
      y = c(0, key_height, key_height, 0),
      desc = "outer_key"
    ))
    
    court_points <- bind_rows(court_points , tibble(
      x = c(-backboard_width / 2, backboard_width / 2),
      y = c(backboard_offset, backboard_offset),
      desc = "backboard"
    ))
    
    court_points <- bind_rows(court_points , tibble(
      x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
    ))
    
    foul_circle <- ellipse_points(center = c(0, key_height), h_radius = inner_key_width / 2, v_radius = inner_key_width / 2)
    
    foul_circle_top <- filter(foul_circle, y > key_height) %>%
      mutate(desc = "foul_circle_top")
    
    foul_circle_bottom <- filter(foul_circle, y < key_height) %>%
      mutate(
        angle = atan((y - key_height) / x) * 180 / pi,
        angle_group = floor((angle - 5.625) / 11.25),
        desc = paste0("foul_circle_bottom_", angle_group)
      ) %>%
      filter(angle_group %% 2 == 0) %>%
      select(x, y, desc)
    
    hoop <- ellipse_points(center = c(0, hoop_center_y), h_radius = hoop_radius, v_radius = hoop_radius) %>%
      mutate(desc = "hoop")
    
    restricted <- ellipse_points(center = c(0, hoop_center_y), h_radius = 4, v_radius = 4) %>%
      filter(y >= hoop_center_y) %>%
      mutate(desc = "restricted")
    
    three_point_circle <- ellipse_points(center = c(0, hoop_center_y), h_radius = three_point_radius, v_radius = three_point_radius) %>%
      filter(y >= three_point_side_height, y >= hoop_center_y)
    
    three_point_line <- tibble(
      x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
      y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
      desc = "three_point_line"
    )
    
    court_points <- bind_rows(
      court_points,
      foul_circle_top,
      foul_circle_bottom,
      hoop,
      restricted,
      three_point_line
    )
    
    # Extract shooting percentages from the single row
    percentages <- c(
      shooting_data$FG_percentage_on_FGAs_that_are_0_to_3_feet_from_the_basket,
      shooting_data$FG_percentage_on_FGAs_that_are_3_to_10_feet_from_the_basket,
      shooting_data$FG_percentage_on_FGAs_that_are_10_to_16_feet_from_the_basket,
      shooting_data$FG_percentage_on_FGAs_that_are_more_than_16_feet_from_the_basket,
      shooting_data$FG_percentage_on_FGAs_that_are_3_Pt_FGAs
    )
    
    # Replace NAs with 0 and handle 0s correctly for color scaling
    percentages[is.na(percentages)] <- 0
    
    # Create a color scale. Modified to include a "no data" color
    color_scale <- colorRampPalette(c("#004d00", "#66ff66"))(101)

    
    zone_radii <- c(3, 10, 16, three_point_radius)
    zone_descriptions <- c("0-3 feet", "3-10 feet", "10-16 feet", "16+ feet", "3pt+")
    
    get_color <- function(percentage) {
      if (is.na(percentage) || percentage == 0) {
        return("#004d00")  # Return gray color for NA or 0 percentage
      } else {
        color_index <- round(percentage * 100) + 1
        color_index <- max(1, min(color_index, 101))
        return(color_scale[color_index])
      }
    }
    
    zones <- tibble()
    for (i in 1:length(zone_radii)) {
      zone_color <- get_color(percentages[i])
      
      # Half-circle part
      half_circle <- ellipse_points(center = c(0, hoop_center_y), h_radius = zone_radii[i], v_radius = zone_radii[i]) %>%
        filter(y >= hoop_center_y, y <= height) %>%
        mutate(desc = zone_descriptions[i], fill = zone_color, percentage = percentages[i])
      
      # Rectangular part
      rectangle <- tibble(
        x = c(-zone_radii[i], zone_radii[i], zone_radii[i], -zone_radii[i]),
        y = c(0, 0, hoop_center_y, hoop_center_y),
        desc = zone_descriptions[i],
        fill = zone_color,
        percentage = percentages[i]
      )
      
      zones <- bind_rows(zones, half_circle, rectangle)
    }
    
    # Create a background rectangle for the entire court
    court_background <- tibble(
      x = c(-width/2, width/2, width/2, -width/2),
      y = c(0, 0, height, height),
      desc = "court_background",
      fill = get_color(percentages[5]),  # Use the color for 3pt+ shots
      text = paste("3pt+:", sprintf("%.1f%%", percentages[5] * 100))
    )
    
    # Create a data frame for the legend
    legend_data <- tibble(
      percentage = seq(0, 1, length.out = 101),
      color = color_scale
    )
    
    # Create labels for the shooting percentages
    labels <- tibble(
      x = c(0, 0, 0, 0, 0),
      y = c(hoop_center_y + 1.5, hoop_center_y + 6.5, hoop_center_y + 13, hoop_center_y + 19, hoop_center_y + 25),
      label = sprintf("%.1f%%", percentages * 100)
    )
    
    court_plot <-
      ggplot() +
      geom_polygon(
        data = court_background,
        aes(x = x, y = y, fill = fill, text = text),
        alpha = 1
      ) +
      geom_polygon(
        data = zones,
        aes(x = x, y = y, group = desc, fill = fill, 
            text = paste(desc, ":", sprintf("%.1f%%", percentage * 100))),
        alpha = 0.7
      ) +
      geom_path(
        data = court_points,
        aes(x = x, y = y, group = desc),
        #color = court_theme$lines
        color = "black"
      ) +
      geom_text(
        data = labels,
        aes(x = x, y = y, label = label),
        color = court_theme$text,
        size = 4,
        fontface = "bold"
      ) +
      # Add color bar legend
      geom_tile(
        data = legend_data,
        aes(x = 30, y = percentage * 35, fill = color, height = 35/101),
        width = 2
      ) +
      geom_text(
        aes(x = 33, y = c(0, 17.5, 35), label = c("0%", "50%", "100%")),
        color = court_theme$text,
        size = 3,
        hjust = 0
      ) +
      coord_fixed(ylim = c(0, 35), xlim = c(-25, 37)) +
      scale_fill_identity() +
      theme_minimal(base_size = 22) +
      theme(
        text = element_text(color = court_theme$text),
        plot.background = element_rect(fill = court_theme$court, color = court_theme$court),
        panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none"
      ) +
      guides(fill = "none")
    
    ggplotly(court_plot, tooltip = c("text")) %>%
      style(hoverinfo = "text", traces = seq_len(nrow(zones) + 2))
  }
  
  
  # Usage:
  plot_court(court_theme = court_themes$light, shooting_data = dataset6)
    
    
  })
  
  output$table6 <- renderDT({
    datatable(
      filtered_data6() %>% 
        summarise(Team = Team,
                  Age = Age,
                  Position = Position,
                  Games = Games,
                  `FG%` = Field_Goal_Percentage,
                  `0-3 ft` = FG_percentage_on_FGAs_that_are_0_to_3_feet_from_the_basket,
                  `3-10 ft` = FG_percentage_on_FGAs_that_are_3_to_10_feet_from_the_basket,
                  `10-16 ft` = FG_percentage_on_FGAs_that_are_10_to_16_feet_from_the_basket,
                  `16+ ft` = FG_percentage_on_FGAs_that_are_more_than_16_feet_from_the_basket,
                  `3pt%` = FG_percentage_on_FGAs_that_are_3_Pt_FGAs
        )
      )
  })
  
  filtered_data7 <- reactive({
    req(input$threes) # Ensure input is available
    req(input$twos)
    req(input$fts)
    req(input$rbds)
    req(input$asts)
    req(input$stls)
    req(input$blks)
    req(input$tos)
    data.frame(
      Three_Point_Field_Goals = input$threes,
      Two_Point_Field_Goals = input$twos,
      Free_Throws = input$fts,
      Defensive_Rebounds = input$rbds,
      Assists = input$asts,
      Steals = input$stls,
      Blocks = input$blks,
      Turnovers = input$tos
    )
  })
  
  output$predict1 <- renderText({
    dataset7 <- filtered_data7()
    model <- glm(All_Star ~ Three_Point_Field_Goals + Two_Point_Field_Goals + Free_Throws + 
                   Defensive_Rebounds + Assists + Steals + Blocks + Turnovers,
                   family=binomial(link='logit'),data=data1)
    ifelse(predict(model,newdata=dataset7,type='response') > 0.5, "All Star", "Not an All Star")
  })
  
  output$predict2 <- renderUI({
    dataset7 <- filtered_data7()
    model <- glm(All_Star ~ Three_Point_Field_Goals + Two_Point_Field_Goals + Free_Throws + 
                   Defensive_Rebounds + Assists + Steals + Blocks + Turnovers,
                 family=binomial(link='logit'), data=data1)
    
    prediction_prob <- predict(model, newdata=dataset7, type='response')
    
    if (prediction_prob > 0.5) {
      tags$img(src = "https://www.freeawardcertificates.com/wp-content/uploads/2015/05/allstar.jpg", alt = "All-Star", height = "200px")
    } else {
      tags$img(src = "https://media.licdn.com/dms/image/v2/C5612AQGoj0G_LLwXhg/article-cover_image-shrink_600_2000/article-cover_image-shrink_600_2000/0/1520204644586?e=2147483647&v=beta&t=eVORIAzCrxLCMplkRPzK5nam7k17nusmQxik53ALDys", alt = "Not All-Star", height = "200px")
    }
  })
  
}

shinyApp(ui = ui, server = server)