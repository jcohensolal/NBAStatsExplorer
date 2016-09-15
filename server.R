if (!require(shiny)) {install.packages("shiny")}
if (!require(dplyr)) {install.packages("dplyr")}
if (!require(ggvis)) {install.packages("ggvis")}
library(shiny)
library(dplyr)
library(ggvis)

# Retrieve data
fourteenData <- read.csv(file = "data/2014nbastats.csv")
fifteenData <- read.csv(file = "data/2015nbastats.csv")
sixteenData <- read.csv(file = "data/2016nbastats.csv")

# Function for generating mouse-over text
playerMouseOver <- function(x) 
{
    if(is.null(x)) return(NULL)
    paste0("<b>", x[, 4], "</b>", "</br>",
           colnames(x)[1], " : ", x[, 1], "</br>", 
           colnames(x)[2], " : ", x[, 2], 
           sep = "")
}

shinyServer(
    function(input, output) 
    {
        # Reactive function to filter the players using the user's inputs
        players <- reactive(
        {
            minGp <- input$GP[1]
            maxGp <- input$GP[2]
            minMpg <- input$MPG[1]
            maxMpg <- input$MPG[2]
            minAge <- input$Age[1]
            maxAge <- input$Age[2]
            
            # Apply filters
            currentData <- getSeason()
            currentData <- currentData[currentData$G > (minGp - 1), ]
            currentData <- currentData[currentData$G < (maxGp + 1), ]
            currentData <- currentData[currentData$MP > (minMpg - 0.1), ]
            currentData <- currentData[currentData$MP < (maxMpg + 0.1), ]
            currentData <- currentData[currentData$Age > (minAge - 1), ]
            currentData <- currentData[currentData$Age < (maxAge + 1), ]

            # Optional : filter by team
            if (input$Team != "All") 
            {
                currentData <- currentData[currentData$Tm %in% input$Team, ]
            }

            # Optional : filter by FA
            if (input$fa) 
            {
                currentData <- currentData[currentData$FA == 1, ]
            }
            
            # Optional : filter by Position
            pos <- input$position
            if (!is.null(pos))
            {
                currentData <- currentData[as.logical(rowSums(currentData[pos] == 1)), ]
            }

            # Add column which says whether the player will be a free agent in 
            # July of 2016
            currentData$isFA <- character(nrow(currentData))
            currentData$isFA[currentData$FA == 0] <- "No"
            currentData$isFA[currentData$FA == 1] <- "Yes"

            currentData
        })
        
        # Reactive function to retrieve X input
        getXName <- eventReactive(input$xvar, 
        {
            return(names(axis_vars)[axis_vars == input$xvar])
        })
        
        # Reactive function to retrieve Y input
        getYName <- eventReactive(input$yvar, 
        {
          return(names(axis_vars)[axis_vars == input$yvar])
        })
        
        # Reactive function to retrieve season input
        getSeason <- eventReactive(input$season, 
        {
            if(input$season == "2016")    {currentData <- sixteenData}
            if(input$season == "2015")    {currentData <- fifteenData}
            if(input$season == "2014")    {currentData <- fourteenData}
            return(currentData)
        })
        
        # Reactive function to calculate sample mean on X axis
        meanX <- reactive(
        {
            xvar <- as.symbol(input$xvar)
            currentData <- players()
            currentDataX <- currentData[, which(colnames(currentData) == xvar)]
            meanPointX <- round(mean(currentDataX), 3)
            meanPointX
        })
        
        # Reactive function to calculate sample mean on Y axis
        meanY <- reactive(
        {
            yvar <- as.symbol(input$yvar)
            currentData <- players()
            currentDataY <- currentData[, which(colnames(currentData) == yvar)]
            meanPointY <- round(mean(currentDataY), 3)
            meanPointY
        })
        
        # Reactive function to plot using Ggvis
        visu <- reactive(
        {
            # Get axes inputs
            xvar_name <- getXName()
            yvar_name <- getYName()
            xvar <- as.symbol(input$xvar)
            yvar <- as.symbol(input$yvar)
            xprop <- prop("x", xvar)
            yprop <- prop("y", yvar)
            
            # Compute means for selected variables
            currentData <- players()
            currentDataX <- currentData[, which(colnames(currentData) == xvar)]
            currentDataY <- currentData[, which(colnames(currentData) == yvar)]
            meanPointX <- mean(currentDataX)
            meanPointY <- mean(currentDataY)
            
            # Plot players
            currentData %>%
                ggvis(x = xprop, y = yprop) %>%
                layer_points(size := 50, size.hover := 200,
                             fillOpacity := 0.2, fillOpacity.hover := 0.5,
                             stroke = ~isFA, key := ~ Player) %>%
                add_tooltip(playerMouseOver, "hover") %>%
                add_axis("x", title = xvar_name) %>%
                add_axis("y", title = yvar_name) %>%
                add_legend("stroke", title = "Future Free Agent", values = c("Yes", "No")) %>%
                scale_nominal("stroke", domain = c("Yes", "No"),
                              range = c("dodgerblue", "#aaa")) %>%
                set_options(width = 650, height = 560)
        })
        
        visu %>% bind_shiny("plot1")

        output$n_players <- renderText({ nrow(players()) })
        output$means <- renderUI(
        { 
            str1 <- getXName() 
            str2 <- meanX() 
            str3 <- getYName() 
            str4 <- meanY()
            HTML(paste(str1, " : ", str2, "<br/>", str3, " : ", str4))
        })
    })
