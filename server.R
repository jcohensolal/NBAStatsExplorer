if (!require(shiny)) {install.packages("shiny")}
if (!require(dplyr)) {install.packages("dplyr")}
if (!require(ggvis)) {install.packages("ggvis")}
library(shiny)
library(dplyr)
library(ggvis)

# Retrieve data
nbaData <- read.csv(file = "data/nbastats_20160224.csv")

shinyServer(
    function(input, output) 
    {
        # Function for generating mouse-over text
        playerMouseOver <- function(x) 
        {
            if (is.null(x))     {return(NULL)}
            if (is.null(x$Rk))  {return(NULL)}
            
            # Pick out the player with this Rk
            player <- nbaData[nbaData$Rk == x$Rk, ]
            paste0("<b>", player$Player, "</b><br>",
                   paste("Team :", player$Tm, "<br>"),
                   paste("Games played :", player$G))
        }

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
            filteredData <- nbaData[nbaData$G > (minGp - 1), ]
            filteredData <- filteredData[filteredData$G < (maxGp + 1), ]
            filteredData <- filteredData[filteredData$MP > (minMpg - 1), ]
            filteredData <- filteredData[filteredData$MP < (maxMpg + 1), ]
            filteredData <- filteredData[filteredData$Age > (minAge - 1), ]
            filteredData <- filteredData[filteredData$Age < (maxAge + 1), ]
            
            # Optional : filter by team
            if (input$Team != "All") 
            {
                filteredData <- filteredData[filteredData$Tm %in% input$Team, ]
            }
            
            # Optional : filter by name
            #if (!is.null(input$Name) && input$Name != "") 
            #{
            #    print(input$Name)
            #    filteredData <- filter(filteredData, grepl(input$Name, Player))
            #}
            
            # Add column which says whether the player will be a free agent in 
            # July of 2016
            filteredData$isFA <- character(nrow(filteredData))
            filteredData$isFA[filteredData$FA == 0] <- "No"
            filteredData$isFA[filteredData$FA == 1] <- "Yes"

            filteredData
        })
        
        # Reactive function to retrieve X input
        getXName <- reactive(
        {
            xvar_name <- names(axis_vars)[axis_vars == input$xvar]
        })
        
        # Reactive function to retrieve Y input
        getYName <- reactive(
        {
            yvar_name <- names(axis_vars)[axis_vars == input$yvar]
        })
        
        # Reactive function to calculate sample mean on X axis
        meanX <- reactive(
        {
            xvar <- as.symbol(input$xvar)
            filteredData <- players()
            filteredDataX <- filteredData[, which(colnames(filteredData) == xvar)]
            meanPointX <- round(mean(filteredDataX), 3)
            meanPointX
        })
        
        # Reactive function to calculate sample mean on Y axis
        meanY <- reactive(
        {
            yvar <- as.symbol(input$yvar)
            filteredData <- players()
            filteredDataY <- filteredData[, which(colnames(filteredData) == yvar)]
            meanPointY <- round(mean(filteredDataY), 3)
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
            filteredData <- players()
            filteredDataX <- filteredData[, which(colnames(filteredData) == xvar)]
            filteredDataY <- filteredData[, which(colnames(filteredData) == yvar)]
            meanPointX <- mean(filteredDataX)
            meanPointY <- mean(filteredDataY)
            
            # Plot players
            players %>%
                ggvis(x = xprop, y = yprop) %>%
                layer_points(size := 50, size.hover := 200,
                             fillOpacity := 0.2, fillOpacity.hover := 0.5,
                             stroke = ~isFA, key := ~ Rk) %>%
                add_tooltip(playerMouseOver, "hover") %>%
                add_axis("x", title = xvar_name) %>%
                add_axis("y", title = yvar_name) %>%
                add_legend("stroke", title = "2016 Free Agent", values = c("Yes", "No")) %>%
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
