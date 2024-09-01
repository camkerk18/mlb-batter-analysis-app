library(shiny)
library(tidyverse)
library(Lahman)
library(dplyr)
library(ggplot2)

batting = as_tibble(Batting)
people = as_tibble(People)

### DATA WRANGLING

bat_stats = batting %>%
  mutate_all(~ifelse(is.na(.), 0, .)) %>%
  mutate(AVG = (H/AB)) %>%
  mutate(OBP = ((H + BB + HBP)/(AB + BB + HBP + SF))) %>%
  mutate(SLG = ((H + (2*X2B) + (3*X3B) + (4*HR))/AB)) %>%
  mutate(AVG = round(AVG, 3)) %>%
  mutate(OBP = round(OBP, 3)) %>%
  mutate(SLG = round(SLG, 3)) %>%
  mutate(OPS = (OBP + SLG)) %>%
  mutate_all(~ifelse(is.na(.), 0, .))

names = people %>%
  select(playerID, nameFirst, nameLast) %>%
  mutate(name = paste(nameFirst, nameLast, sep = " ")) %>%
  select(playerID, name)

batting_merged = left_join(bat_stats, names, by = "playerID")

batting_merged = batting_merged %>%
  rename(year = yearID) %>%
  filter(year >= 2005) %>%
  filter(AB >= 20)

batting_table = batting_merged %>%
  select(name, year, G, AB, R, H, X2B, X3B, HR, RBI, SB, CS, BB, SO, IBB, 
         HBP, SH, SF, GIDP, AVG, OBP, SLG, OPS) %>%
  rename(Name = name) %>%
  rename(Year = year)

# APP
ui = fluidPage(
  
  titlePanel("Batter Statistics Post-Steroid Era (2005-2022)"),
  
  tabsetPanel(
    tabPanel("Graph",
      sidebarLayout(
      sidebarPanel(
      selectInput(inputId = "batter", label = "Batter:", 
                  choices = sort(unique(batting_merged$name)), 
                  selected = "Robinson Cano"),
      selectInput(inputId = "start", 
                  label = "Start Year:",
                  choices = sort(unique(batting_merged$year)),
                  selected = 2005),
      selectInput(inputId = "end", 
                  label = "End Year:", 
                  choices = sort(unique(batting_merged$year)),
                  selected = 2022),
      selectInput(inputId = "stat", 
                  label = "Statistic:",
                  choices = c("Games" = "G", "At Bats" = "AB", "Runs" = "R", 
                              "Hits" = "H", "Doubles" = "X2B", 
                              "Triples" = "X3B", "Home Runs" = "HR", 
                              "Runs Batted In" = "RBI", 
                              "Stolen Bases" = "SB", 
                              "Caught Stealing" = "CS", "Walks" = "BB", 
                              "Strike Outs" = "SO", 
                              "Intentional Walks" = "IBB", 
                              "Hit By Pitch" = "HBP", 
                              "Sacrifice Bunts" = "SH", 
                              "Sacrifice Flies" = "SF", 
                              "Double Plays Committed" = "GIDP", 
                              "Batting Average" = "AVG", 
                              "On-Base Percentage" = "OBP", 
                              "Slugging Percentage" = "SLG", 
                              "On-Base Plus Slugging" = "OPS"),
                  selected = "H"),
      sliderInput(inputId = "min_ab", label = "Minimum At Bats", 
                  min = 20, max = max(batting_merged$AB), 
                  value = 20, step = 1),
      submitButton(text = "Apply Changes", icon = NULL, width = NULL)
      
    ),
    
    mainPanel(
      plotOutput("bat_stat_plot"),
      dataTableOutput("table")
    )
  )),
  tabPanel("Description",
           fluidPage(
             h2("Batter Statistics Description"),
             p("Name: Cameron Kerkemeyer"),
             p("Email: ck45@illinois.edu"),
             p("The game of baseball has many different statistics that are used to analyze player performance. This application takes the core batting statistics taken over an MLB player's career to analyze a player's performance throughout their career. Through graphical representation, trends can be recognized to see player improvement, regression, and sometimes injury. And a table is used to show each player's career batting performance across all statistics broken down by year."),
             p("The data used in the application is from Sean Lahman's Baseball Database which includes various baseball statistics of the MLB and various other leagues from 1871 to 2022. The data obtained and used was the yearly batting statistics of each player from 1871 to 2022. The data has been filtered in the application to the MLB statistics for Post-Steroid Era (2005 to 2022)."),
             p(""),
             p("References:"),
             p("Lahman, S. (2023) Lahman's Baseball Database, 1871-2022, Main page, https://www.seanlahman.com/baseball-archive/statistics/")
           ))
  ))

server = function(input, output) {
  batters = reactive({
    batting_merged %>%
      filter(year >= input$start) %>%
      filter(year <= input$end) %>%
      filter(name == input$batter) %>%
      filter(AB >= input$min_ab) %>%
      select(name, year, G, AB, R, H, X2B, X3B, HR, RBI, SB, CS, BB, SO, IBB, 
             HBP, SH, SF, GIDP, AVG, OBP, SLG, OPS) %>%
      rename(Name = name) %>%
      rename(Year = year)
  })
  
  observeEvent(eventExpr = input$batter,
               handlerExpr = {
                 batting_merged_2 = batting_merged %>%
                   filter(name == input$batter) %>%
                   filter(year >= 2005) %>%
                   filter(AB >= 20)
                 updateSelectInput(inputId = "start",
                                   choices = sort(unique(batting_merged_2$year)),
                                   selected = min(batting_merged_2$year))
               })
  observeEvent(eventExpr = input$batter,
               handlerExpr = {
                 batting_merged_3 = batting_merged %>%
                   filter(name == input$batter) %>%
                   filter(year >= 2005) %>%
                   filter(AB >= 20)
                 updateSelectInput(inputId = "end",
                                   choices = sort(unique(batting_merged_3$year)),
                                   selected = max(batting_merged_3$year))
               })
  
  batters_2 = reactive({
    batters %>%
      filter(year >= input$start)
  })
  
  output$bat_stat_plot = renderPlot({
    p = ggplot(data = batters(), aes(x = Year, y = !!as.symbol(input$stat))) +
      geom_point() +
      geom_line() +
      theme_bw()
    if (nrow(batters()) > 1) {
      p <- p + geom_line()
    }
    p
  })
  output$table <- renderDataTable({
    batters()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
