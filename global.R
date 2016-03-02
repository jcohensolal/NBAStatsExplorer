if (!require(httr)) {install.packages("httr")}
if (!require(XML)) {install.packages("XML")}
library(httr)
library(XML)

# Variables that can be put on the X and Y axes
axis_vars <- c(
    "Points per game" = "PTS",
    "Rebounds per game" = "TRB",
    "Offensive rebounds per game" = "ORB",
    "Defensive rebounds per game" = "DRB",
    "Assists per game" = "AST",
    "Steals per game" = "STL",
    "Blocks per game" = "BLK",
    "Effective field goal pct" = "eFGP",
    "Field goals made per game" = "FG",
    "Field goals attempts per game" = "FGA",
    "Field goal pct" = "FGP",
    "3 points shots made per game" = "TH",
    "3 points shots attempts per game" = "THA",
    "3 points shot pct" = "THP",
    "2 points shots made per game" = "TW",
    "2 points shots attempts per game" = "TWA",
    "2 points shot pct" = "TWP",
    "Free throws made per game" = "FT",
    "Free throws attempts per game" = "FTA",
    "Free throw pct" = "FTP",
    "Turnovers per game" = "TOV",
    "Player efficiency rating" = "PER",
    "True shooting pct" = "TSP",
    "3 points attempt rate" = "TPAr",
    "Free throw rate" = "FTr",
    "Offensive rebound pct" = "ORBP",
    "Defensive rebound pct" = "DRBP",
    "Total rebound pct" = "TRBP",
    "Assist pct" = "ASTP",
    "Steal pct" = "STLP",
    "Block pct" = "BLKP",
    "Turnover pct" = "TOVP",
    "Usage pct" = "USGP",
    "Offensive win shares" = "OWS",
    "Defensive win shares" = "DWS",
    "Win shares" = "WS",
    "Win shares per 48 mins" = "WSPFE",
    "Offensive box plus/minus" = "OBPM",
    "Defensive box plus/minus" = "DBPM",
    "Box plus/minus" = "BPM",
    "Value over replacement player" = "VORP")

# Retrieving NBA data from various sources and consolidate all info into one 
# single CSV file
createCSV <- function()
{
    # Get Free Agency data from HTML source code
    htmlDoc <- GET("http://basketball.realgm.com/nba/future_free_agents/2017/All/Per_Game/0/NBA/player")
    stop_for_status(htmlDoc)
    docContent <- content(htmlDoc, as = "text")
    parsedDoc = htmlParse(docContent)
    faTable <- xpathSApply(parsedDoc, "/descendant::tbody", xmlValue)
    faVector <- unlist(strsplit(faTable, "\n"))
    
    # Retrieve basic stats from HTML source code
    htmlDoc <- GET("http://www.basketball-reference.com/leagues/NBA_2016_per_game.html")
    stop_for_status(htmlDoc)
    docContent <- content(htmlDoc, as = "text")
    parsedDoc = htmlParse(docContent)
    playerTable <- xpathSApply(parsedDoc, "/descendant::tbody", xmlValue)
    playerVector <- unlist(strsplit(playerTable, "\n   "))
    
    # Retrieve advanced stats from HTML source code
    htmlDoc <- GET("http://www.basketball-reference.com/leagues/NBA_2016_advanced.html")
    stop_for_status(htmlDoc)
    docContent <- content(htmlDoc, as = "text")
    parsedDoc = htmlParse(docContent)
    advancedTable <- xpathSApply(parsedDoc, "/descendant::tbody", xmlValue)
    advancedVector <- unlist(strsplit(advancedTable, "\n   "))
    
    # Create file with a header
    file = "data/nbastats.csv"
    players <- data.frame(Rk = integer(), Player = factor(), Pos = factor(), 
                          Age = integer(), Tm = factor(), G = integer(), 
                          GS = integer(), MP = numeric(), FG = numeric(), 
                          FGA = numeric(), FGP = numeric(), TH = numeric(), 
                          THA = numeric(), THP = numeric(), TW = numeric(), 
                          TWA = numeric(), TWP = numeric(), eFGP = numeric(), 
                          FT = numeric(), FTA = numeric(), FTP = numeric(), 
                          ORB = numeric(), DRB = numeric(), TRB = numeric(), 
                          AST = numeric(), STL = numeric(), BLK = numeric(), 
                          TOV = numeric(), PF = numeric(), PTS = numeric(), 
                          FA = integer(), PER = numeric(), TSP = numeric(), 
                          TPAr = numeric(), FTr = numeric(), ORBP = numeric(), 
                          DRBP = numeric(), TRBP = numeric(), ASTP = numeric(), 
                          STLP = numeric(), BLKP = numeric(), TOVP = numeric(), 
                          USGP = numeric(), OWS = numeric(), DWS = numeric(), 
                          WS = numeric(), WSPFE = numeric(), OBPM = numeric(), 
                          DBPM = numeric(), BPM = numeric(), VORP = numeric(), 
                          stringsAsFactors=FALSE)
    write.table(x = players, 
                file = file, 
                row.names = FALSE, 
                sep = ",")
    
    # Populate CSV file
    i <- 1
    j <- 1
    previousPlayer <- ""
    while(TRUE)
    {
        curRk <- playerVector[i]
        curRk <- unlist(strsplit(curRk, "\n"))
        curRk <- curRk[length(curRk)]
        curPlayer <- playerVector[i + 1]
        
        # Hack for players traded during the season (only keep total stats)
        if (curPlayer == previousPlayer)    
        {
            i <- i +29
            j <- j +28
            next
        }
        previousPlayer <- curPlayer
        
        curPos <- playerVector[i + 2]
        curAge <- playerVector[i + 3]
        curTm <- playerVector[i + 4]
        curG <- playerVector[i + 5]
        curGS <- playerVector[i + 6]
        curMP <- playerVector[i + 7]
        curFG <- playerVector[i + 8]
        curFGA <- playerVector[i + 9]
        curFGP <- playerVector[i + 10]
        if (curFGP == "")   {curFGP <- ".000"}
        cur3P <- playerVector[i + 11]
        cur3PA <- playerVector[i + 12]
        cur3PP <- playerVector[i + 13]
        if (cur3PP == "")   {cur3PP <- ".000"}
        cur2P <- playerVector[i + 14]
        cur2PA <- playerVector[i + 15]
        cur2PP <- playerVector[i + 16]
        if (cur2PP == "")   {cur2PP <- ".000"}
        curEFG <- playerVector[i + 17]
        if (curEFG == "")   {curEFG <- ".000"}
        curFT <- playerVector[i + 18]
        curFTA <- playerVector[i + 19]
        curFTP <- playerVector[i + 20]
        if (curFTP == "")   {curFTP <- ".000"}
        curORB <- playerVector[i + 21]
        curDRB <- playerVector[i + 22]
        curTRB <- playerVector[i + 23]
        curAST <- playerVector[i + 24]
        curSTL <- playerVector[i + 25]
        curBLK <- playerVector[i + 26]
        curTOV <- playerVector[i + 27]
        curPF <- playerVector[i + 28]
        curPTS <- playerVector[i + 29]
        curPTS <- unlist(strsplit(curPTS, "\n"))
        curPTS <- curPTS[1]
        
        # Check for FA status
        if (curPlayer %in% faVector)    {curFA <- "1"}
        else                            {curFA <- "0"}
        
        curPER <- advancedVector[j + 7]
        curTSP <- advancedVector[j + 8]
        if (curTSP == "")   {curTSP <- ".000"}
        curTPAr <- advancedVector[j + 9]
        if (curTPAr == "")   {curTPAr <- ".000"}
        curFTr <- advancedVector[j + 10]
        if (curFTr == "")   {curFTr <- ".000"}
        curORBP <- advancedVector[j + 11]
        curDRBP <- advancedVector[j + 12]
        curTRBP <- advancedVector[j + 13]
        curASTP <- advancedVector[j + 14]
        curSTLP <- advancedVector[j + 15]
        curBLKP <- advancedVector[j + 16]
        curTOVP <- advancedVector[j + 17]
        if (curTOVP == "")   {curFTr <- "0.0"}
        curUSGP <- advancedVector[j + 18]
        curOWS <- advancedVector[j + 20]
        curDWS <- advancedVector[j + 21]
        curWS <- advancedVector[j + 22]
        curWSPFE <- advancedVector[j + 23]
        curOBPM <- advancedVector[j + 25]
        curDBPM <- advancedVector[j + 26]
        curBPM <- advancedVector[j + 27]
        curVORP <- advancedVector[j + 28]
        curVORP <- unlist(strsplit(curVORP, "\n"))
        curVORP <- curVORP[1]
        
        curMatrix<- matrix(
            c(curRk, curPlayer, curPos, curAge, curTm, curG, curGS, curMP, curFG, 
              curFGA, curFGP, cur3P, cur3PA, cur3PP, cur2P, cur2PA, cur2PP, curEFG, 
              curFT, curFTA, curFTP, curORB, curDRB, curTRB, curAST, curSTL, curBLK, 
              curTOV, curPF, curPTS, curFA, curPER, curTSP, curTPAr, curFTr, curORBP, 
              curDRBP, curTRBP, curASTP, curSTLP, curBLKP, curTOVP, curUSGP, curOWS, 
              curDWS, curWS, curWSPFE, curOBPM, curDBPM, curBPM, curVORP),
            nrow = 1, 
            ncol = 51)
        write.table(curMatrix, 
                    file, 
                    append = TRUE, 
                    col.names = FALSE, 
                    row.names = FALSE, 
                    sep = ",")
        
        i <- i +29
        j <- j +28
        if (!is.na(playerVector[i+1]))   {next}
        break
    }
}
