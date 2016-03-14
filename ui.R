if (!require(shiny)) {install.packages("shiny")}
if (!require(ggvis)) {install.packages("ggvis")}
library(shiny)
library(ggvis)

shinyUI(fluidPage(
    titlePanel("NBA Player Stats Explorer"),
    fluidRow(column(4,
                    wellPanel(
                        h4("AXES", align = "center"),
                        selectInput("xvar", "X-axis variable", axis_vars, selected = "OBPM"),
                        selectInput("yvar", "Y-axis variable", axis_vars, selected = "DBPM")), 
                    wellPanel(
                        h4("FILTERS", align = "center"),
                        sliderInput("GP", "Games played in 2015/2016 season",
                                    1, 82, value = c(15, 82)),
                        sliderInput("MPG", "Minutes per game (average)",
                                    1, 48, value = c(15, 48)),
                        sliderInput("Age", "Age",
                                    18, 45, value = c(18, 45)),
                        checkboxGroupInput("position", "Position", 
                                           choices = list("PG" = 'PG', "SG" = 'SG', 
                                                          "SF" = 'SF', "PF" = 'PF', 
                                                          "C" = 'C'),
                                           selected = list("PG" = 'PG', "SG" = 'SG', 
                                                           "SF" = 'SF', "PF" = 'PF', 
                                                           "C" = 'C')),
                        selectInput("Team", "Team",
                                    c("All", "ATL", "BOS", "BRK", "CHI", "CHO", "CLE", 
                                      "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", 
                                      "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", 
                                      "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", 
                                      "TOR", "TOT", "UTA", "WAS")), 
                        checkboxInput('fa', '2016 Free Agents'))),
             column(8,
                    tabsetPanel(
                        # show a plot  
                        tabPanel("Explorer", ggvisOutput("plot1"),
                                 tags$small(paste0("First loading might take 10-15 seconds, 
                                            please be patient. You can then change a variable or 
                                            a filter to show the plot")), 
                            wellPanel(strong("Number of players selected :"), 
                                      textOutput("n_players"), 
                                      strong("Means for current sample :"),
                                      htmlOutput("means")),
                            fluidRow(
                                column(4, tags$small("All stats from "), 
                                       br(a("Basketball Reference", 
                                            href="http://www.basketball-reference.com/leagues/NBA_2016_advanced.html", 
                                            target="_blank"))),
                                column(4, tags$small("Free agency data from "), 
                                       br(a("Real GM", 
                                            href="http://basketball.realgm.com/nba/future_free_agents/2017/All/Per_Game/0/NBA/player", 
                                            target="_blank"))),
                                column(4, tags$small("Positions from "), 
                                       br(a("Yahoo Sports", 
                                            href="http://sports.yahoo.com/nba/stats/byposition", 
                                            target="_blank"))))), 
                        # show support document 
                        tabPanel("User Guide", includeHTML("NBAStatsExplorer.html")))))))
