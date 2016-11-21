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
    "Fouls per game" = "FOU",
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
    "Player efficiency rating" = "PER",
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
createCSV <- function(year)
{
    # Retrieve basic stats from HTML source code
    message("Stats parsing")
    htmlDoc <- GET(paste("http://www.basketball-reference.com/leagues/NBA_", year, "_per_game.html", sep = ""))
    stop_for_status(htmlDoc)
    docContent <- content(htmlDoc, as = "text")
    basicDoc = htmlParse(docContent)
    
    # Retrieve advanced stats from HTML source code
    message("Advanced stats parsing")
    htmlDoc <- GET(paste("http://www.basketball-reference.com/leagues/NBA_", year, "_advanced.html", sep = ""))
    stop_for_status(htmlDoc)
    docContent <- content(htmlDoc, as = "text")
    advancedDoc = htmlParse(docContent)

    # Get Free Agency data from HTML source code
    message("FA data parsing")
    htmlDoc <- GET(paste("http://basketball.realgm.com/nba/future_free_agents/", year + 1, "/All/Per_Game/0/NBA/player", sep = ""))
    stop_for_status(htmlDoc)
    docContent <- content(htmlDoc, as = "text")
    parsedDoc = htmlParse(docContent)
    faTable <- xpathSApply(parsedDoc, "/descendant::tbody", xmlValue)
    faVector <- unlist(strsplit(faTable, "\n"))
    
    # Get Position data from HTML source code
    message("Positions parsing")
    htmlDoc <- GET(paste("http://sports.yahoo.com/nba/stats/byposition?pos=PG&conference=NBA&year=season_", year - 1, "&qualified=0", sep = ""))
    stop_for_status(htmlDoc)
    docContent <- content(htmlDoc, as = "text")
    parsedDoc = htmlParse(docContent)
    posTable <- xpathSApply(parsedDoc, "//div[@id='yog-bd']/div/div[2]/div/div/table[4]/tr/td/a", xmlValue)
    pgVector <- unlist(strsplit(as.character(posTable), "\n"))
    htmlDoc <- GET(paste("http://sports.yahoo.com/nba/stats/byposition?pos=SG&conference=NBA&year=season_", year - 1, "&qualified=0", sep = ""))
    stop_for_status(htmlDoc)
    docContent <- content(htmlDoc, as = "text")
    parsedDoc = htmlParse(docContent)
    posTable <- xpathSApply(parsedDoc, "//div[@id='yog-bd']/div/div[2]/div/div/table[4]/tr/td/a", xmlValue)
    sgVector <- unlist(strsplit(as.character(posTable), "\n"))
    htmlDoc <- GET(paste("http://sports.yahoo.com/nba/stats/byposition?pos=SF&conference=NBA&year=season_", year - 1, "&qualified=0", sep = ""))
    stop_for_status(htmlDoc)
    docContent <- content(htmlDoc, as = "text")
    parsedDoc = htmlParse(docContent)
    posTable <- xpathSApply(parsedDoc, "//div[@id='yog-bd']/div/div[2]/div/div/table[4]/tr/td/a", xmlValue)
    sfVector <- unlist(strsplit(as.character(posTable), "\n"))
    htmlDoc <- GET(paste("http://sports.yahoo.com/nba/stats/byposition?pos=PF&conference=NBA&year=season_", year - 1, "&qualified=0", sep = ""))
    stop_for_status(htmlDoc)
    docContent <- content(htmlDoc, as = "text")
    parsedDoc = htmlParse(docContent)
    posTable <- xpathSApply(parsedDoc, "//div[@id='yog-bd']/div/div[2]/div/div/table[4]/tr/td/a", xmlValue)
    pfVector <- unlist(strsplit(as.character(posTable), "\n"))
    htmlDoc <- GET(paste("http://sports.yahoo.com/nba/stats/byposition?pos=C&conference=NBA&year=season_", year - 1, "&qualified=0", sep = ""))
    stop_for_status(htmlDoc)
    docContent <- content(htmlDoc, as = "text")
    parsedDoc = htmlParse(docContent)
    posTable <- xpathSApply(parsedDoc, "//div[@id='yog-bd']/div/div[2]/div/div/table[4]/tr/td/a", xmlValue)
    cVector <- unlist(strsplit(as.character(posTable), "\n"))
    htmlDoc <- GET(paste("http://sports.yahoo.com/nba/stats/byposition?pos=G&conference=NBA&year=season_", year - 1, "&qualified=0", sep = ""))
    stop_for_status(htmlDoc)
    docContent <- content(htmlDoc, as = "text")
    parsedDoc = htmlParse(docContent)
    posTable <- xpathSApply(parsedDoc, "//div[@id='yog-bd']/div/div[2]/div/div/table[4]/tr/td/a", xmlValue)
    gVector <- unlist(strsplit(as.character(posTable), "\n"))
    htmlDoc <- GET(paste("http://sports.yahoo.com/nba/stats/byposition?pos=GF&conference=NBA&year=season_", year - 1, "&qualified=0", sep = ""))
    stop_for_status(htmlDoc)
    docContent <- content(htmlDoc, as = "text")
    parsedDoc = htmlParse(docContent)
    posTable <- xpathSApply(parsedDoc, "//div[@id='yog-bd']/div/div[2]/div/div/table[4]/tr/td/a", xmlValue)
    gfVector <- unlist(strsplit(as.character(posTable), "\n"))
    htmlDoc <- GET(paste("http://sports.yahoo.com/nba/stats/byposition?pos=F&conference=NBA&year=season_", year - 1, "&qualified=0", sep = ""))
    stop_for_status(htmlDoc)
    docContent <- content(htmlDoc, as = "text")
    parsedDoc = htmlParse(docContent)
    posTable <- xpathSApply(parsedDoc, "//div[@id='yog-bd']/div/div[2]/div/div/table[4]/tr/td/a", xmlValue)
    fVector <- unlist(strsplit(as.character(posTable), "\n"))
    htmlDoc <- GET(paste("http://sports.yahoo.com/nba/stats/byposition?pos=FC&conference=NBA&year=season_", year - 1, "&qualified=0", sep = ""))
    stop_for_status(htmlDoc)
    docContent <- content(htmlDoc, as = "text")
    parsedDoc = htmlParse(docContent)
    posTable <- xpathSApply(parsedDoc, "//div[@id='yog-bd']/div/div[2]/div/div/table[4]/tr/td/a", xmlValue)
    fcVector <- unlist(strsplit(as.character(posTable), "\n"))

    message("Parsing over")
    
    # Create file with a header
    file = paste("data/", year , "nbastats.csv", sep = "")
    players <- data.frame(Rk = integer(), Player = factor(), Pos = factor(), 
                          Age = integer(), Tm = factor(), G = integer(), 
                          GS = integer(), MP = numeric(), FG = numeric(), 
                          FGA = numeric(), FGP = numeric(), TH = numeric(), 
                          THA = numeric(), THP = numeric(), TW = numeric(), 
                          TWA = numeric(), TWP = numeric(), eFGP = numeric(), 
                          FT = numeric(), FTA = numeric(), FTP = numeric(), 
                          ORB = numeric(), DRB = numeric(), TRB = numeric(), 
                          AST = numeric(), STL = numeric(), BLK = numeric(), 
                          TOV = numeric(), FOU = numeric(), PTS = numeric(), 
                          FA = integer(), PER = numeric(), TSP = numeric(), 
                          TPAr = numeric(), FTr = numeric(), ORBP = numeric(), 
                          DRBP = numeric(), TRBP = numeric(), ASTP = numeric(), 
                          STLP = numeric(), BLKP = numeric(), TOVP = numeric(), 
                          USGP = numeric(), OWS = numeric(), DWS = numeric(), 
                          WS = numeric(), WSPFE = numeric(), OBPM = numeric(), 
                          DBPM = numeric(), BPM = numeric(), VORP = numeric(), 
                          PG = integer(), SG = integer(), SF = integer(), 
                          PF = integer(), C = integer(), 
                          stringsAsFactors=FALSE)
    write.table(x = players, 
                file = file, 
                row.names = FALSE, 
                sep = ",")
    
    # Populate CSV file
    message("Populate CSV file")
    playerNb <- as.integer(xpathSApply(basicDoc, "//tbody/tr[last()]/th", xmlValue))
    message(paste("playerNb :", playerNb))
    i <- 1
    previousPlayer <- ""
    while(i <= 1000)
    {
        curRk <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/th", sep = ""), xmlValue)
        curPlayer <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[1]", sep = ""), xmlValue)
        
        # Hack for lines in BBref code where they reprint stat names
        if (length(curPlayer) == 0)
        {
            i <- i + 1
            next()
        }
        else
        {
            # Hack for players traded during the season (only keep total stats)
            if (curPlayer == previousPlayer)
            {
                i <- i + 1
                next()
            }
        }
        previousPlayer <- curPlayer

        curPos <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[2]", sep = ""), xmlValue)
        curAge <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[3]", sep = ""), xmlValue)
        curTm <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[4]", sep = ""), xmlValue)
        curG <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[5]", sep = ""), xmlValue)
        curGS <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[6]", sep = ""), xmlValue)
        curMP <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[7]", sep = ""), xmlValue)
        curFG <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[8]", sep = ""), xmlValue)
        curFGA <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[9]", sep = ""), xmlValue)
        curFGP <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[10]", sep = ""), xmlValue)
        cur3P <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[11]", sep = ""), xmlValue)
        cur3PA <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[12]", sep = ""), xmlValue)
        cur3PP <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[13]", sep = ""), xmlValue)
        cur2P <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[14]", sep = ""), xmlValue)
        cur2PA <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[15]", sep = ""), xmlValue)
        cur2PP <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[16]", sep = ""), xmlValue)
        curEFG <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[17]", sep = ""), xmlValue)
        curFT <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[18]", sep = ""), xmlValue)
        curFTA <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[19]", sep = ""), xmlValue)
        curFTP <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[20]", sep = ""), xmlValue)
        curORB <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[21]", sep = ""), xmlValue)
        curDRB <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[22]", sep = ""), xmlValue)
        curTRB <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[23]", sep = ""), xmlValue)
        curAST <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[24]", sep = ""), xmlValue)
        curSTL <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[25]", sep = ""), xmlValue)
        curBLK <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[26]", sep = ""), xmlValue)
        curTOV <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[27]", sep = ""), xmlValue)
        curFOU <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[28]", sep = ""), xmlValue)
        curPTS <- xpathSApply(basicDoc, paste("//tbody/tr[", i, "]/td[29]", sep = ""), xmlValue)
        
        if (curFGP == "")   {curFGP <- ".000"}
        if (cur3PP == "")   {cur3PP <- ".000"}
        if (cur2PP == "")   {cur2PP <- ".000"}
        if (curEFG == "")   {curEFG <- ".000"}
        if (curFTP == "")   {curFTP <- ".000"}

        # Check for FA status
        if (curPlayer %in% faVector)    {curFA <- "1"}
        else                            {curFA <- "0"}

        curPER <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[7]", sep = ""), xmlValue)
        curTSP <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[8]", sep = ""), xmlValue)
        curTPAr <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[9]", sep = ""), xmlValue)
        curFTr <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[10]", sep = ""), xmlValue)
        curORBP <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[11]", sep = ""), xmlValue)
        curDRBP <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[12]", sep = ""), xmlValue)
        curTRBP <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[13]", sep = ""), xmlValue)
        curASTP <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[14]", sep = ""), xmlValue)
        curSTLP <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[15]", sep = ""), xmlValue)
        curBLKP <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[16]", sep = ""), xmlValue)
        curTOVP <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[17]", sep = ""), xmlValue)
        curUSGP <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[18]", sep = ""), xmlValue)
        curOWS <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[20]", sep = ""), xmlValue)
        curDWS <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[21]", sep = ""), xmlValue)
        curWS <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[22]", sep = ""), xmlValue)
        curWSPFE <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[23]", sep = ""), xmlValue)
        curOBPM <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[25]", sep = ""), xmlValue)
        curDBPM <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[26]", sep = ""), xmlValue)
        curBPM <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[27]", sep = ""), xmlValue)
        curVORP <- xpathSApply(advancedDoc, paste("//tbody/tr[", i, "]/td[28]", sep = ""), xmlValue)

        if (curTSP == "")   {curTSP <- ".000"}
        if (curTPAr == "")  {curTPAr <- ".000"}
        if (curFTr == "")   {curFTr <- ".000"}
        if (curTOVP == "")  {curTOVP <- ".000"}

        # Check for position
        curPG <- "0"
        curSG <- "0"
        curSF <- "0"
        curPF <- "0"
        curC <- "0"
        if (curPlayer %in% pgVector)    {curPG <- "1"}
        if (curPlayer %in% sgVector)    {curSG <- "1"}
        if (curPlayer %in% sfVector)    {curSF <- "1"}
        if (curPlayer %in% pfVector)    {curPF <- "1"}
        if (curPlayer %in% cVector)     {curC <- "1"}
        if (curPlayer %in% gVector)
        {
            curPG <- "1"
            curSG <- "1"
        }
        if (curPlayer %in% gfVector)
        {
            curSG <- "1"
            curSF <- "1"
        }
        if (curPlayer %in% fVector)
        {
            curSF <- "1"
            curPF <- "1"
        }
        if (curPlayer %in% fcVector)
        {
            curPF <- "1"
            curC  <- "1"
        }

        curMatrix<- matrix(
            c(curRk, curPlayer, curPos, curAge, curTm, curG, curGS, curMP, curFG,
              curFGA, curFGP, cur3P, cur3PA, cur3PP, cur2P, cur2PA, cur2PP, curEFG,
              curFT, curFTA, curFTP, curORB, curDRB, curTRB, curAST, curSTL, curBLK,
              curTOV, curFOU, curPTS, curFA, curPER, curTSP, curTPAr, curFTr, curORBP,
              curDRBP, curTRBP, curASTP, curSTLP, curBLKP, curTOVP, curUSGP, curOWS,
              curDWS, curWS, curWSPFE, curOBPM, curDBPM, curBPM, curVORP, curPG, curSG,
              curSF, curPF, curC),
            nrow = 1,
            ncol = 56)
        write.table(curMatrix,
                    file,
                    append = TRUE,
                    col.names = FALSE,
                    row.names = FALSE,
                    sep = ",")
        
        i <- i + 1
    }
}
