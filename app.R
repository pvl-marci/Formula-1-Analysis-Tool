# Laden der Bibliotheken
library(shiny)
library(tidyverse)


# Dataframes
races_data <- read.csv(file = 'datasets/races.csv', sep = ";")
results_data <- read.csv(file = 'datasets/results.csv', sep = ";")
constructors_data <-
  read.csv(file = 'datasets/constructors.csv', sep = ";")
drivers_data <- read.csv(file = 'datasets/drivers.csv', sep = ";")
data_one = merge(races_data, results_data, by = c("raceId"))
data_two = merge(data_one, drivers_data, by = c("driverId"))
data = merge(data_two, constructors_data, by = c("constructorId"))


# User Interface der Shiny App
ui <- fluidPage(
  titlePanel("Formel 1: Auswertung der Endplatzierungen"),
  
  
  # User Inputs
  sidebarLayout(
    sidebarPanel(
      h3("Anleitung"),
      p(
        "Wähle 2 Teams und die gewünschten Start- und Endpositionen aus, die du vergleichen möchtest. Du kannst auch 2 gleiche Teams in unterschieldichen Zeiträumen vergleichen"
      ),
      selectInput("team1", "Team 1:", unique(data$constructor_name)),
      selectInput("team2", "Team 2:", unique(data$constructor_name), selected = ""),
      sliderInput(
        "start_pos",
        "Startposition:",
        min = 1,
        max = 24,
        value = c(1, 5)
      ),
      sliderInput(
        "end_pos",
        "Position im Ziel:",
        min = 1,
        max = 24,
        value = c(1, 5)
      ),
      dateRangeInput(
        "years1",
        "Zeitraum Team 1:",
        language = "de",
        separator = " bis ",
        start = "1950-01-01",
        end = "2022-12-31",
        min = "1950-01-01",
        max = "2022-12-31",
        format = "yyyy-mm-dd"
      ),
      dateRangeInput(
        "years2",
        "Zeitraum Team 2:",
        language = "de",
        separator = " bis ",
        start = "1950-01-01",
        end = "2022-12-31",
        min = "1950-01-01",
        max = "2022-12-31",
        format = "yyyy-mm-dd"
      ),
      sliderInput(
        "confidence",
        "Konfidenzniveau:",
        0.95,
        min = 0.95,
        max = 0.99,
        step = 0.01
      ),
      actionButton("ttest_button", "t-Test durchführen"),
      verbatimTextOutput("ttest_result"),
      HTML("<br>"),
      p("Quelle:"),
      tags$a(href = "https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020",
             "kaggle.com")
    ),
    
    
    # Output
    mainPanel(fluidRow(
      h2(textOutput("anleitung")),
      
      
      column(
        4,
        
        h3(textOutput("team1_name")),
        h4(textOutput("team1_start_pos_text")),
        h4(textOutput("team1_anzahl_position")),
        h4(textOutput("team1_team_aktiv")),
      ),
      column(
        4,
        h3(textOutput("team2_name")),
        h4(textOutput("team2_start_pos_text")),
        h4(textOutput("team2_anzahl_position")),
        h4(textOutput("team2_team_aktiv")),
      ),
      
      column(12,
             tabsetPanel(
               tabPanel(
                 "Konfidenzintervalle",
                 
                 column(
                   12,
                   
                   plotOutput("team1_end_pos_plot"),
                   textOutput("position_sd_team1"),
                   h4(textOutput("team1_end_pos_text")),
                   HTML("<br>"),
                   HTML("<br>"),
                   plotOutput("team2_end_pos_plot"),
                   textOutput("position_sd_team2"),
                   h4(textOutput("team2_end_pos_text")),
                   
                 ),
               ),
               
               tabPanel(
                 "Positionsverteilungen",
                 column(4,
                        tableOutput("position_table1")),
                 column(4,
                        tableOutput("position_table2"))
               )
             ))
      
      
    ))
  )
)



# Serverfunktion
server <- function(input, output) {
  # Änderung des Konfidenzniveaus
  conf_level <- reactive({
    qnorm(1 - (1 - input$confidence) / 2)
  })
  
  # Filter für Alter der Teams
  team1_filter_data <- reactive({
    data %>% filter(constructor_name == input$team1)
  })
  
  
  # Filter Team 1
  
  #Filter für Teamalter
  table_daten_team1 <- reactive({
    data %>% filter(
      constructor_name == input$team1,
      grid >= input$start_pos[1],
      grid <= input$start_pos[2],
      year >= as.numeric(format(as.Date(input$years1[1]), format = "%Y")),
      year <= as.numeric(format(as.Date(input$years1[2]), format = "%Y")),
      
    )
  })
  
  # Kompletter Filter
  daten_team1 <- reactive({
    data %>% filter(
      constructor_name == input$team1,
      grid >= input$start_pos[1],
      grid <= input$start_pos[2],
      position >= input$end_pos[1],
      position <= input$end_pos[2],
      year >= as.numeric(format(as.Date(input$years1[1]), format = "%Y")),
      year <= as.numeric(format(as.Date(input$years1[2]), format = "%Y")),
      
    )
  })
  
  # Outputgenerierung Team 1
  output$team1_team_aktiv <- renderText({
    minjahr1 <- min(team1_filter_data()$year)
    maxjahr1 <- max(team1_filter_data()$year)
    paste("Team Aktiv von ", minjahr1, "-", maxjahr1)
  })
  
  output$position_table1 <- renderTable({
    setNames(as.data.frame(table(table_daten_team1()$position)), c("Position", "Anzahl"))
  })
  
  output$position_sd_team1 <- renderText({
    sd_positionen_team1 <- round(sd(daten_team1()$position), 3)
    paste("Standardabweichung beträgt:  ", sd_positionen_team1)
  })
  
  output$team1_name <- renderText({
    paste("Team: ", input$team1)
  })
  
  output$team1_start_pos_text <- renderText({
    minstart <- min(input$start_pos)
    maxstart <- max(input$start_pos)
    paste("Startpositionen:", minstart, "-", maxstart)
  })
  
  
  output$team1_anzahl_position <- renderText({
    paste("Erfasste Positionen: ", nrow(daten_team1()))
  })
  
  
  output$team1_end_pos_text <- renderText({
    konfidenzniveau <- input$confidence * 100
    mean_pos <- mean(daten_team1()$position)
    sd_pos <- sd(daten_team1()$position)
    n <- nrow(daten_team1())
    conf_int_low <- mean_pos - conf_level() * sd_pos / sqrt(n)
    conf_int_high <- mean_pos + conf_level() * sd_pos / sqrt(n)
    paste(
      "Die durchschnittliche Zielposition ist: ",
      round(mean_pos, 2),
      "Bei einem Konfidenzniveau von ",
      konfidenzniveau,
      "% liegt das Konfidenzintervall zwischen [",
      round(conf_int_low, 2),
      ",",
      round(conf_int_high, 2),
      "]"
    )
  })
  
  #Plot Team 1
  output$team1_end_pos_plot <- renderPlot({
    ggplot(daten_team1(), aes(x = position)) +
      geom_histogram(
        binwidth = 1,
        fill = "cornflowerblue",
        color = "white"
      ) +
      geom_smooth(
        aes(y = ..count..),
        stat = "bin",
        binwidth = 1,
        color = "violet",
        size = 1.5,
        method = 'loess'
      ) +
      geom_vline(
        xintercept = mean(daten_team1()$position),
        color = "red",
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = mean(daten_team1()$position) - conf_level() * sd(daten_team1()$position) /
          sqrt(nrow(daten_team1())),
        color = "black",
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = mean(daten_team1()$position) + conf_level() * sd(daten_team1()$position) /
          sqrt(nrow(daten_team1())),
        color = "black",
        linetype =
          "dashed"
      ) +
      ggtitle(paste("Verteilung der Positionen im Ziel für ", input$team1)) +
      xlab("Position") +
      ylab("Anzahl")
  })
  
  
  
  
  # Filter Team 2
  
  # Filter für Teamalter
  table_daten_team2 <- reactive({
    data %>% filter(
      constructor_name == input$team2,
      grid >= input$start_pos[1],
      grid <= input$start_pos[2],
      year >= as.numeric(format(as.Date(input$years2[1]), format = "%Y")),
      year <= as.numeric(format(as.Date(input$years2[2]), format = "%Y")),
      
    )
  })
  
  # Kompletter Filter
  daten_team2 <- reactive({
    data %>% filter(
      constructor_name == input$team2,
      grid >= input$start_pos[1],
      grid <= input$start_pos[2],
      position >= input$end_pos[1],
      position <= input$end_pos[2],
      year >= as.numeric(format(as.Date(input$years2[1]), format = "%Y")),
      year <= as.numeric(format(as.Date(input$years2[2]), format = "%Y"))
    )
  })
  
  team2_filter_data <- reactive({
    data %>% filter(constructor_name == input$team2)
  })
  
  output$position_table2 <- renderTable({
    setNames(as.data.frame(table(table_daten_team2()$position)), c("Position", "Anzahl"))
  })
  
  output$team2_team_aktiv <- renderText({
    minjahr2 <- min(team2_filter_data()$year)
    maxjahr2 <- max(team2_filter_data()$year)
    paste("Team Aktiv von ", minjahr2, "-", maxjahr2)
  })
  
  output$position_sd_team2 <- renderText({
    sd_positionen_team2 <- round(sd(daten_team2()$position), 3)
    paste("Standardabweichung beträgt:  ", sd_positionen_team2)
  })
  
  output$team2_anzahl_position <- renderText({
    paste("Erfasste Positionen: ", nrow(daten_team2()))
  })
  
  
  
  # Outputgenerierung Team 2
  output$team2_name <- renderText({
    paste("Team: ", input$team2)
  })
  
  output$team2_start_pos_text <- renderText({
    minstart <- min(input$start_pos)
    maxstart <- max(input$start_pos)
    paste("Startpositionen:", minstart, "-", maxstart)
  })
  
  
  output$team2_end_pos_text <- renderText({
    konfidenzniveau <- input$confidence * 100
    mean_pos <- mean(daten_team2()$position)
    sd_pos <- sd(daten_team2()$position)
    n <- nrow(daten_team1())
    conf_int_low <- mean_pos - conf_level() * sd_pos / sqrt(n)
    conf_int_high <- mean_pos + conf_level() * sd_pos / sqrt(n)
    paste(
      "Die durchschnittliche Zielposition ist: ",
      round(mean_pos, 2),
      "Bei einem Konfidenzniveaul von ",
      konfidenzniveau,
      "% liegt das Konfidenzintervall zwischen [",
      round(conf_int_low, 2),
      ",",
      round(conf_int_high, 2),
      "]"
    )
  })
  
  
  # Plot Team 2
  output$team2_end_pos_plot <- renderPlot({
    ggplot(daten_team2(), aes(x = position)) +
      geom_histogram(
        binwidth = 1,
        fill = "cornflowerblue",
        color = "white"
      ) +
      geom_smooth(
        aes(y = ..count..),
        stat = "bin",
        binwidth = 1,
        color = "violet",
        size = 1.5,
        method = 'loess'
      ) +
      geom_vline(
        xintercept = mean(daten_team2()$position),
        color = "red",
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = mean(daten_team2()$position) - conf_level() * sd(daten_team2()$position) /
          sqrt(nrow(daten_team2())),
        color = "black",
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = mean(daten_team2()$position) + conf_level() * sd(daten_team2()$position) /
          sqrt(nrow(daten_team2())),
        color = "black",
        linetype =
          "dashed"
      ) +
      ggtitle(paste("Verteilung der Positionen im Ziel für ", input$team2)) +
      xlab("Position") +
      ylab("Anzahl")
  })
  
  
  observeEvent(input$ttest_button, {
    ttest_result <- t.test(daten_team1()$position,
                           daten_team2()$position,
                           alternative = "two.sided")
    output$ttest_result <- renderText({
      paste("t-value:",
            ttest_result$statistic,
            "p-value:",
            ttest_result$p.value)
    })
  })
  
  
  
  
  
  
  
} #Ende Server Funktion







shinyApp(ui, server)
