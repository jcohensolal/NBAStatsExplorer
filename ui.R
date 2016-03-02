if (!require(shiny)) {install.packages("shiny")}
if (!require(ggvis)) {install.packages("ggvis")}
library(shiny)
library(ggvis)

shinyUI(fluidPage(
    titlePanel("NBA Player Stats Explorer"),
    fluidRow(column(4,
                    wellPanel(
                        h4("AXES", align = "center"),
                        selectInput("xvar", "X-axis variable", axis_vars, selected = "PTS"),
                        selectInput("yvar", "Y-axis variable", axis_vars, selected = "eFGP")), 
                    wellPanel(
                        h4("FILTERS", align = "center"),
                        sliderInput("GP", "Games played in 2015/2016 season",
                                    1, 82, value = c(15, 82)),
                        sliderInput("MPG", "Minutes per game (average)",
                                    1, 48, value = c(15, 48)),
                        sliderInput("Age", "Age",
                                    18, 45, value = c(18, 45)),
                        selectInput("Team", "Team",
                                    c("All", "ATL", "BOS", "BRK", "CHI", "CHO", "CLE", 
                                      "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", 
                                      "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", 
                                      "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", 
                                      "TOR", "TOT", "UTA", "WAS"))),
                       # textInput("Name", "Player name contains (e.g., Williams)"))),
                    tags$small("All stats provided by "), 
                    br("http://www.basketball-reference.com"), 
                    tags$small("Free agency data provided by "), 
                    br("http://basketball.realgm.com/")),
             column(8,
                    tabsetPanel(
                        # show a plot  
                        tabPanel("Explorer", ggvisOutput("plot1"),
                                 tags$small(paste0("First loading might take a few seconds, please 
                                        change a variable or a filter to show the plot")), 
                            wellPanel(strong("Number of players selected :"), 
                                      textOutput("n_players"), 
                                      strong("Calculated means for current sample :"),
                                      htmlOutput("means"))), 
                        # show support document 
                        tabPanel("User Guide", includeHTML("NBAStatsExplorer.html")))))))
