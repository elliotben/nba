library(RCurl)
library(XML)
library(scrapeR)
library(stringr)
library(plyr)
library(sqldf)
library(jsonlite)
library(RMongo)

#--------------------------------------------------------------------------------
#LOAD DESCRIPTIONS

initializeTeams <- function() {
  url <- "http://www.espn.com/nba/players"
  webpage <- getURL(url)
  tc <- textConnection(webpage)
  webpage <- readLines(tc) 
  close(tc)
  pageTree <<- htmlTreeParse(webpage, useInternalNodes = TRUE)
}

loadTeams <- function() {
  teams <<- c()
  initializeTeams()
  for (i in 1:6) {
    for (n in 1:5) {
      xpath <-
        paste(
          "//*[@id=\"my-players-table\"]/div[", as.character(i), 
          "]/div/div[2]/ul/li[", as.character(n),"]/div/a", sep = "")
      vec <- unlist(xpathApply(pageTree, xpath, xmlValue))
      teams <<- c(teams, vec)
    }
  }
}

loadLinks <- function () {
  initializeTeams()
  links <- c()
  for (i in 1:60) {
    if (i %% 2 == 1)
      link <- unlist(xpathSApply(pageTree, "//a/@href"))[[i]]
    links <- unique(c(links, link))
  }
  links <<- gsub("team/", "team/roster/", links)
}

initializeDescriptions <- function(teamLink) {
  url <- teamLink
  webpage <- getURL(url)
  tc <- textConnection(webpage)
  webpage <- readLines(tc) 
  close(tc)
  pageTree <<- htmlTreeParse(webpage, useInternalNodes = TRUE)
}

loadDescriptions <- function() {
  
  dfDescriptions <<- data.frame(matrix(vector(), 0, 9,
                                       dimnames=list(c(), c("team", "firstName", "lastName", "jerseyNumber", "position", "age", "height", "weight", "college"))),
                                stringsAsFactors=F)
  
  p <- 1
  #there are 30 teams in the nba
  while (p <= 30) {
    loadLinks()
    loadTeams()
    initializeDescriptions(links[p])
    #loop through all the players of a particular team
    for (i in 1:17) {
      xpath <- paste("//*[@id=\"my-players-table\"]/div[2]/div/table[1]/tr[", as.character(i+2), "]/td", sep = "")
      vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
      myRow <- i + (p-1)*17
      if (!is.null(vec)) {
        namePos <- unlist(strsplit(vec[2], split = ", "))
        dfDescriptions[myRow,1] <<- teams[p]
        dfDescriptions[myRow,2] <<- unlist(strsplit(namePos[1], split = " "))[1]
        dfDescriptions[myRow,3] <<- unlist(strsplit(namePos[1], split = " "))[2]
        dfDescriptions[myRow,4] <<- as.numeric(vec[1])
        dfDescriptions[myRow,5] <<- vec[3]
        dfDescriptions[myRow,6] <<- as.numeric(vec[4])
        dfDescriptions[myRow,7] <<- gsub("-", ".", vec[5])
        dfDescriptions[myRow,8] <<- as.numeric(vec[6])
        dfDescriptions[myRow,9] <<- vec[7]
      }
    }
    p <- p+1
  }
  dfDescriptions[(which(dfDescriptions$position == "F")),5] <<- "PF"
  dfDescriptions <<- na.omit(dfDescriptions)
  rownames(dfDescriptions) <<- 1:nrow(dfDescriptions)
  
}


#----------------------------------------------------------------------------------
#LOAD SCORING

initializeScoring <- function(pageNumber, year) {
  url <- paste("http://www.espn.com/nba/statistics/player/_/stat/scoring-per-game/sort/avgPoints/year/", year, "/seasontype/2/qualified/false/count/", pageNumber, sep="")
  webpage <- getURL(url)
  tc <- textConnection(webpage)
  webpage <- readLines(tc) 
  close(tc)
  pageTree <<- htmlTreeParse(webpage, useInternalNodes = TRUE)
}

loadScoring <- function(begYear, endYear) {
  
  dfScoring <<- data.frame(matrix(vector(), 0, 16,
                                 dimnames=list(c(), c("firstName", "lastName", "team", "season", "gamesPlayed", "minutesPerGame", "pointsPerGame", 
                                                      "FGM", "FGA", "FGPercentage", "3PM", "3PA", "3PPercentage", "FTM", "FA", "FTPercentage"))),
                          stringsAsFactors=F)
  
  p <- 1
  myRow <- 0
  
  for (y in begYear:endYear) {
    while (p <= 441) {
      initializeScoring(p, y)
      for (i in 1:43) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div/div[2]/table/tr[", (i+1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        myRow <- myRow + 1
        if (!is.null(vec)) {
          namePos <- unlist(strsplit(vec[2], split = ", "))
          fg <- unlist(strsplit(vec[7], split = "-"))
          threes <- unlist(strsplit(vec[9], split = "-"))
          ft <- unlist(strsplit(vec[11], split = "-"))
          dfScoring[myRow,1] <<- unlist(strsplit(namePos[1], split = " "))[1]
          dfScoring[myRow,2] <<- unlist(strsplit(namePos[1], split = " "))[2]
          dfScoring[myRow,3] <<- vec[3]
          dfScoring[myRow,4] <<- y
          dfScoring[myRow,5] <<- as.numeric(vec[4])
          dfScoring[myRow,6] <<- as.numeric(vec[5])
          dfScoring[myRow,7] <<- as.numeric(vec[6])
          dfScoring[myRow,8] <<- as.numeric(fg[1])
          dfScoring[myRow,9] <<- as.numeric(fg[2])
          dfScoring[myRow,10] <<- as.numeric(vec[8])
          dfScoring[myRow,11] <<- as.numeric(threes[1])
          dfScoring[myRow,12] <<- as.numeric(threes[2])
          dfScoring[myRow,13] <<- as.numeric(vec[10])
          dfScoring[myRow,14] <<- as.numeric(ft[1])
          dfScoring[myRow,15] <<- as.numeric(ft[2])
          dfScoring[myRow,16] <<- as.numeric(vec[12])
        }
      }
      p <- p+40
    }
    p <- 1
  }
  dfScoring <<- na.omit(dfScoring)
  rownames(dfScoring) <<- 1:nrow(dfScoring)
}

#------------------------------------------------------------------------------
#LOAD REBOUNDS

initializeRebounds <- function(pageNumber, year) {
  url <- paste("http://www.espn.com/nba/statistics/player/_/stat/rebounds/sort/avgRebounds/year/", year, "/seasontype/2/qualified/false/count/", pageNumber, sep="")
  webpage <- getURL(url)
  tc <- textConnection(webpage)
  webpage <- readLines(tc) 
  close(tc)
  pageTree <<- htmlTreeParse(webpage, useInternalNodes = TRUE)
}

loadRebounds <- function(begYear, endYear) {
  
  dfRebounds <<- data.frame(matrix(vector(), 0, 11,
                                  dimnames=list(c(), c("firstName", "lastName", "team", "season", "offREB", "ORPerGame", "defREB", "DRPerGame", "REB", "REBPerGame", 
                                                       "REBPer48"))),
                           stringsAsFactors=F)
  
  p <- 1
  myRow <- 0 
  
  for(y in begYear:endYear) {
    while (p <= 441) {
      initializeRebounds(p, y)
      for (i in 1:43) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div/div[2]/table/tr[", (i+1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        myRow <- myRow + 1
        if (!is.null(vec)) {
          namePos <- unlist(strsplit(vec[2], split = ", "))
          dfRebounds[myRow,1] <<- unlist(strsplit(namePos[1], split = " "))[1]
          dfRebounds[myRow,2] <<- unlist(strsplit(namePos[1], split = " "))[2]
          dfRebounds[myRow,3] <<- vec[3]
          dfRebounds[myRow,4] <<- y
          dfRebounds[myRow,5] <<- as.numeric(vec[6])
          dfRebounds[myRow,6] <<- as.numeric(vec[7])
          dfRebounds[myRow,7] <<- as.numeric(vec[8])
          dfRebounds[myRow,8] <<- as.numeric(vec[9])
          dfRebounds[myRow,9] <<- as.numeric(vec[10])
          dfRebounds[myRow,10] <<- as.numeric(vec[11])
          dfRebounds[myRow,11] <<- as.numeric(vec[12])
        }
      }
      p <- p+40
    }
    p <- 1
  }
  dfRebounds <<- na.omit(dfRebounds)
  rownames(dfRebounds) <<- 1:nrow(dfRebounds)
  
}

#------------------------------------------------------------------------------
#LOAD ASSISTS

initializeAssists <- function(pageNumber, year) {
  url <- paste("http://www.espn.com/nba/statistics/player/_/stat/assists/sort/avgAssists/year/", year, "/seasontype/2/qualified/false/count/", pageNumber, sep="")
  webpage <- getURL(url)
  tc <- textConnection(webpage)
  webpage <- readLines(tc) 
  close(tc)
  pageTree <<- htmlTreeParse(webpage, useInternalNodes = TRUE)
}

loadAssists <- function(begYear, endYear) {
  
  dfAssists <<- data.frame(matrix(vector(), 0, 10,
                                 dimnames=list(c(), c("firstName", "lastName", "team", "season", "AST", "ASTPerGame", "Turnovers", "TOPerGame", "ASTPer48", "AST/Turnovers"))),
                          stringsAsFactors=F)
  
  p <- 1
  myRow <- 0
  
  for (y in begYear:endYear) {
    while (p <= 441) {
      initializeAssists(p, y)
      for (i in 1:43) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div/div[2]/table/tr[", as.character(i+1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        myRow <- myRow + 1
        if (!is.null(vec)) {
          namePos <- unlist(strsplit(vec[2], split = ", "))
          dfAssists[myRow,1] <<- unlist(strsplit(namePos[1], split = " "))[1]
          dfAssists[myRow,2] <<- unlist(strsplit(namePos[1], split = " "))[2]
          dfAssists[myRow,3] <<- vec[3]
          dfAssists[myRow,4] <<- y
          dfAssists[myRow,5] <<- as.numeric(vec[6])
          dfAssists[myRow,6] <<- as.numeric(vec[7])
          dfAssists[myRow,7] <<- as.numeric(vec[8])
          dfAssists[myRow,8] <<- as.numeric(vec[9])
          dfAssists[myRow,9] <<- as.numeric(vec[10])
          dfAssists[myRow,10] <<- as.numeric(vec[11])
        }
      }
      p <- p+40
    }
    p <- 1
  }
  dfAssists <<- na.omit(dfAssists)
  rownames(dfAssists) <<- 1:nrow(dfAssists)
}

#------------------------------------------------------------------------------
#LOAD STEALS

initializeSteals <- function(pageNumber, year) {
  url <- paste("http://www.espn.com/nba/statistics/player/_/stat/steals/sort/avgSteals/year/", year, "/seasontype/2/qualified/false/count/", pageNumber, sep="")
  webpage <- getURL(url)
  tc <- textConnection(webpage)
  webpage <- readLines(tc) 
  close(tc)
  pageTree <<- htmlTreeParse(webpage, useInternalNodes = TRUE)
}

loadSteals <- function(begYear, endYear) {
  
  dfSteals <<- data.frame(matrix(vector(), 0, 8,
                                dimnames=list(c(), c("firstName", "lastName", "team", "season", "STL", "STLPerGame", "STLPer48", "STL/Turnovers"))),
                         stringsAsFactors=F)
  
  p <- 1
  myRow <- 0
  
  for (y in begYear:endYear) {
    while (p <= 441) {
      initializeSteals(p, y)
      for (i in 1:43) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div/div[2]/table/tr[", (i+1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        myRow <- myRow + 1
        if (!is.null(vec)) {
          namePos <- unlist(strsplit(vec[2], split = ", "))
          dfSteals[myRow,1] <<- unlist(strsplit(namePos[1], split = " "))[1]
          dfSteals[myRow,2] <<- unlist(strsplit(namePos[1], split = " "))[2]
          dfSteals[myRow,3] <<- vec[3]
          dfSteals[myRow,4] <<- y
          dfSteals[myRow,5] <<- as.numeric(vec[6])
          dfSteals[myRow,6] <<- as.numeric(vec[7])
          dfSteals[myRow,7] <<- as.numeric(vec[8])
          dfSteals[myRow,8] <<- as.numeric(vec[12])
        }
      }
      p <- p+40
    }
    p <- 1
  }
  dfSteals <<- na.omit(dfSteals)
  rownames(dfSteals) <<- 1:nrow(dfSteals)
}

#------------------------------------------------------------------------------
#LOAD BLOCKS

initializeBlocks <- function(pageNumber, year) {
  url <- paste("http://www.espn.com/nba/statistics/player/_/stat/blocks/sort/avgBlocks/year/", year, "/seasontype/2/qualified/false/count/", pageNumber, sep="")
  webpage <- getURL(url)
  tc <- textConnection(webpage)
  webpage <- readLines(tc) 
  close(tc)
  pageTree <<- htmlTreeParse(webpage, useInternalNodes = TRUE)
}

loadBlocks <- function(begYear, endYear) {
  
  dfBlocks <<- data.frame(matrix(vector(), 0, 8,
                                dimnames=list(c(), c("firstName", "lastName", "team", "season", "BLK", "BLKPerGame", "BLKPer48", "BLK/PF"))),
                         stringsAsFactors=F)
  
  p <- 1
  myRow <- 0
  
  for (y in begYear:endYear) {
    while (p <= 441) {
      initializeBlocks(p, y)
      for (i in 1:43) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div/div[2]/table/tr[", as.character(i+1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        myRow <- myRow + 1
        if (!is.null(vec)) {
          namePos <- unlist(strsplit(vec[2], split = ", "))
          dfBlocks[myRow,1] <<- unlist(strsplit(namePos[1], split = " "))[1]
          dfBlocks[myRow,2] <<- unlist(strsplit(namePos[1], split = " "))[2]
          dfBlocks[myRow,3] <<- vec[3]
          dfBlocks[myRow,4] <<- y
          dfBlocks[myRow,5] <<- as.numeric(vec[6])
          dfBlocks[myRow,6] <<- as.numeric(vec[8])
          dfBlocks[myRow,7] <<- as.numeric(vec[9])
          dfBlocks[myRow,8] <<- as.numeric(vec[10])
        }
      }
      p <- p+40
    }
    p <- 1
  }
  dfBlocks <<- na.omit(dfBlocks)
  rownames(dfBlocks) <<- 1:nrow(dfBlocks)
}

#------------------------------------------------------------------------------
#LOAD FOULS

initializeFouls <- function(pageNumber, year) {
  url <- paste("http://www.espn.com/nba/statistics/player/_/stat/fouls/sort/avgFouls/year/", year, "/seasontype/2/qualified/false/count/", pageNumber, sep="")
  webpage <- getURL(url)
  tc <- textConnection(webpage)
  webpage <- readLines(tc) 
  close(tc)
  pageTree <<- htmlTreeParse(webpage, useInternalNodes = TRUE)
}

loadFouls <- function(begYear, endYear) {
  
  dfFouls <<- data.frame(matrix(vector(), 0, 10,
                               dimnames=list(c(), c("firstName", "lastName", "team", "season", "PF", "PFPerGame", "PFPer48", "flag", "tech", "eject"))),
                        stringsAsFactors=F)
  
  p <- 1
  myRow <- 0
  
  for (y in begYear:endYear) {
    while (p <= 441) {
      initializeFouls(p, y)
      for (i in 1:43) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div/div[2]/table/tr[", as.character(i+1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        myRow <- myRow + 1
        if (!is.null(vec)) {
          namePos <- unlist(strsplit(vec[2], split = ", "))
          dfFouls[myRow,1] <<- unlist(strsplit(namePos[1], split = " "))[1]
          dfFouls[myRow,2] <<- unlist(strsplit(namePos[1], split = " "))[2]
          dfFouls[myRow,3] <<- vec[3]
          dfFouls[myRow,4] <<- y
          dfFouls[myRow,5] <<- as.numeric(vec[6])
          dfFouls[myRow,6] <<- as.numeric(vec[7])
          dfFouls[myRow,7] <<- as.numeric(vec[8])
          dfFouls[myRow,8] <<- as.numeric(vec[9])
          dfFouls[myRow,9] <<- as.numeric(vec[10])
          dfFouls[myRow,10] <<- as.numeric(vec[11])
        }
      }
      p <- p+40
    }
    p <- 1
  }
  dfFouls <<- na.omit(dfFouls)
  rownames(dfFouls) <<- 1:nrow(dfFouls)
}

#--------------------------------------------------------------------------------
#LOAD DD

initializeDD <- function(pageNumber, year) {
  url <- paste("http://www.espn.com/nba/statistics/player/_/stat/double-doubles/sort/doubleDouble/year/", year, "/seasontype/2/qualified/false/count/", pageNumber, sep="")
  webpage <- getURL(url)
  tc <- textConnection(webpage)
  webpage <- readLines(tc) 
  close(tc)
  pageTree <<- htmlTreeParse(webpage, useInternalNodes = TRUE)
}

loadDD <- function(begYear, endYear) {
  
  dfDD <<- data.frame(matrix(vector(), 0, 6,
                            dimnames=list(c(), c("firstName", "lastName", "team", "season","doubleDoubles", "tripleDoubles"))),
                     stringsAsFactors=F)
  
  p <- 1
  myRow <- 0
  
  for (y in begYear:endYear) {
    while (p <= 441) {
      initializeDD(p,y)
      for (i in 1:43) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div/div[2]/table/tr[", (i+1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        myRow <- myRow + 1
        if (!is.null(vec)) {
          namePos <- unlist(strsplit(vec[2], split = ", "))
          dfDD[myRow,1] <<- unlist(strsplit(namePos[1], split = " "))[1]
          dfDD[myRow,2] <<- unlist(strsplit(namePos[1], split = " "))[2]
          dfDD[myRow,3] <<- vec[3]
          dfDD[myRow,4] <<- y
          dfDD[myRow,5] <<- as.numeric(vec[11])
          dfDD[myRow,6] <<- as.numeric(vec[12])
        }
      }
      p <- p+40
    }
    p <- 1
  }
  dfDD <<- na.omit(dfDD)
  rownames(dfDD) <<- 1:nrow(dfDD)
  dfNames <<- dfDD[,c(1:4)]
}

#-------------------------------------------------------------------------
#LOAD CONTRACTS

initializeContracts1 <- function(year) {
  url <- paste("https://www.eskimo.com/~pbender/misc/salaries", substr(year,3,4),".txt", sep = "")
  webpage <- getURL(url)
  tc <- textConnection(webpage)
  webpage <<- readLines(tc) 
  close(tc)
}

loadContracts1 <- function(begYear, endYear) {
  
  dfContracts1 <<- data.frame(matrix(vector(), 0, 4,
                                     dimnames = list(
                                       c(), c("firstName", "lastName", "season", "salary"))),
                              stringsAsFactors = F)
  
  myRow <- 0
  
  for (y in begYear:endYear) {
    initializeContracts1(y)
    
    myStr <<- c("Atlanta", "Hawks", "Total")
    j <- 1
    while (!all(str_detect(webpage[j], myStr))) {
      j <- j + 1
    }
    webpage <<- webpage[-1:-(j-1)]
    
    rows <- NROW(webpage)
    
    if(y == 2000) {
      for (i in 1:rows) {
        x <- webpage[i]
        x <- gsub("[.]", "", x)
        x <- gsub("\\s+", " ", str_trim(x))
        x <- unlist(strsplit(x, " "))
        myRow <- myRow + 1
        if (is.null(x) | identical(x, character(0)) | x[2] == "76ers" |
            is.na(x[2]) | x[1] == "Patricia" | is.na(startsWith(x[3], "$"))) {
          
        } else if (is.character(x[1]) & !is.na(as.numeric(substr(x[2], 1, 1)))) {
          dfContracts1[myRow,1] <<- x[1]
          dfContracts1[myRow,3] <<- y
          salary <- gsub(",", "", x[2])
          dfContracts1[myRow,4] <<- as.numeric(salary)
        } else if (is.character(x[1]) & is.character(x[2]) & !is.na(as.numeric(substr(x[3], 1, 1)))) {
          dfContracts1[myRow,1] <<- x[1]
          dfContracts1[myRow,2] <<- x[2]
          dfContracts1[myRow,3] <<- y
          salary <- gsub(",", "", x[3])
          dfContracts1[myRow,4] <<- as.numeric(salary)
        }
      }
    } else {
      for (i in 1:rows) {
        x <- webpage[i]
        x <- gsub("[.]", "", x)
        x <- gsub("\\s+", " ", str_trim(x))
        x <- unlist(strsplit(x, " "))
        myRow <- myRow + 1
        if (is.null(x) | identical(x, character(0)) |
            is.na(x[2]) | x[1] == "Patricia" | is.na(startsWith(x[3], "$"))) {
          
        } else if (is.character(x[1]) & startsWith(x[2], "$")) {
          dfContracts1[myRow,1] <<- x[1]
          dfContracts1[myRow,3] <<- y
          salary <- gsub(",", "", x[2])
          salary <- gsub("\\$", "", salary)
          dfContracts1[myRow,4] <<- as.numeric(salary)
        } else if (is.character(x[1]) & is.character(x[2]) & startsWith(x[3], "$")) {
          dfContracts1[myRow,1] <<- x[1]
          dfContracts1[myRow,2] <<- x[2]
          dfContracts1[myRow,3] <<- y
          salary <- gsub(",", "", x[3])
          salary <- gsub("\\$", "", salary)
          dfContracts1[myRow,4] <<- as.numeric(salary)
        }
      }
    }
    
  }
  dfContracts1 <<- dfContracts1[rowSums(is.na(dfContracts1))!=4, ]
  rownames(dfContracts1) <<- 1:nrow(dfContracts1)
  
  dfContracts1[(which(dfContracts1$firstName == "Nene")), 2] <<- "Hilario"
}

initializeContracts2 <- function() {
  url <- "http://www.basketball-reference.com/contracts/players.html"
  webpage <- getURL(url)
  tc <- textConnection(webpage)
  webpage <- readLines(tc) 
  close(tc)
  pageTree <<- htmlTreeParse(webpage, useInternalNodes = TRUE)
}



loadContracts2 <- function() {
  dfContracts2 <- data.frame(matrix(vector(), 0, 9,
                                    dimnames=list(c(), c("firstName", "lastName", "team","2016-17", "2017-18", "2018-19", "2019-20", 
                                                         "2020-21", "guaranteed"))),
                             stringsAsFactors=F)
  
  initializeContracts2()
  for (i in 1:646) {
    xpath <- paste("//*[@id=\"player-contracts\"]/tbody/tr[", as.character(i), "]/td")
    vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
    if (!is.null(vec)) {
      name <- unlist(strsplit(vec[1], split = " "))
      dfContracts2[i,1] <- name[1]
      dfContracts2[i,2] <- name[2]
      dfContracts2[i,3] <- vec[2]
      dfContracts2[i,4] <- as.numeric(gsub(",", "", gsub("\\$", "", vec[3])))
      dfContracts2[i,5] <- as.numeric(gsub(",", "", gsub("\\$", "", vec[4])))
      dfContracts2[i,6] <- as.numeric(gsub(",", "", gsub("\\$", "", vec[5])))
      dfContracts2[i,7] <- as.numeric(gsub(",", "", gsub("\\$", "", vec[6])))
      dfContracts2[i,8] <- as.numeric(gsub(",", "", gsub("\\$", "", vec[7])))
      dfContracts2[i,9] <- as.numeric(gsub(",", "", gsub("\\$", "", vec[10])))
    }
  }
  dfContracts2 <- dfContracts2[complete.cases(dfContracts2[1]),]
  dfContracts2[(which(is.na(dfContracts2$guaranteed))),9] <- 0
  
  dfContracts3 <<- data.frame(matrix(vector(), 0, 4,
                                     dimnames = list(
                                       c(), c("firstName", "lastName", "season", "salary"))),
                              stringsAsFactors = F)
  rows <- nrow(dfContracts2)
  myRow <- 1
  for (i in 1:rows) {
    for (x in 1:5) {
      dfContracts3[myRow,1] <<- dfContracts2[i,1]
      dfContracts3[myRow,2] <<- dfContracts2[i,2]
      dfContracts3[myRow,3] <<- 2016+x
      dfContracts3[myRow,4] <<- dfContracts2[i,3+x]
      myRow <- myRow + 1
    }
  }
}


loadContracts <- function() {
  loadContracts1(2000,2016)
  loadContracts2()
  dfContracts <<- data.frame(matrix(vector(), 0, 4,
                                    dimnames = list(
                                      c(), c("firstName", "lastName", "season", "salary"))),
                             stringsAsFactors = F)
  dfContracts <<- rbind(dfContracts1,dfContracts3)
  dfContracts$season <<- as.numeric(dfContracts$season)
  dfContracts$salary <<- as.numeric(dfContracts$salary)
  rm(dfContracts1,pos = ".GlobalEnv")
  rm(dfContracts3,pos = ".GlobalEnv")
  dfContracts <- aggregate(salary~firstName + lastName + season,data=dfContracts,FUN=sum)
  
  addCap()
  addNext()
}

addCap <- function() {
  dfContracts["Salary cap"] <<- NA
  for (i in 1:nrow(dfContracts)) {
    if (dfContracts[i,3] == 2000) {
      dfContracts[i,5] <<- 34000000
    }
    if (dfContracts[i,3] == 2001) {
      dfContracts[i,5] <<- 35500000
    }
    if (dfContracts[i,3] == 2002) {
      dfContracts[i,5] <<- 42500000
    }
    if (dfContracts[i,3] == 2003) {
      dfContracts[i,5] <<- 40271000
    }
    if (dfContracts[i,3] == 2004) {
      dfContracts[i,5] <<- 43840000
    }
    if (dfContracts[i,3] == 2005) {
      dfContracts[i,5] <<- 43870000
    }
    if (dfContracts[i,3] == 2006) {
      dfContracts[i,5] <<- 49500000
    }
    if (dfContracts[i,3] == 2007) {
      dfContracts[i,5] <<- 53135000
    }
    if (dfContracts[i,3] == 2008) {
      dfContracts[i,5] <<- 55630000
    }
    if (dfContracts[i,3] == 2009) {
      dfContracts[i,5] <<- 58680000
    }
    if (dfContracts[i,3] == 2010) {
      dfContracts[i,5] <<- 57700000
    }
    if (dfContracts[i,3] == 2011) {
      dfContracts[i,5] <<- 58044000
    }
    if (dfContracts[i,3] == 2012) {
      dfContracts[i,5] <<- 58044000
    }
    if (dfContracts[i,3] == 2013) {
      dfContracts[i,5] <<- 58044000
    }
    if (dfContracts[i,3] == 2014) {
      dfContracts[i,5] <<- 58679000
    }
    if (dfContracts[i,3] == 2015) {
      dfContracts[i,5] <<- 63065000
    }
    if (dfContracts[i,3] == 2016) {
      dfContracts[i,5] <<- 70000000
    }
    if (dfContracts[i,3] == 2017) {
      dfContracts[i,5] <<- 94143000
    }
    if (dfContracts[i,3] == 2018) {
      dfContracts[i,5] <<- 102000000
    }
    if (dfContracts[i,3] == 2019) {
      dfContracts[i,5] <<- 103000000
    }
    if (dfContracts[i,3] == 2020) {
      dfContracts[i,5] <<- 108150000
    }
    if (dfContracts[i,3] == 2021) {
      dfContracts[i,5] <<- 113558000
    }
  }
  dfContracts["capPercent"] <<- dfContracts$salary / dfContracts$`Salary cap`
}

addNext <- function() {
  dfContracts["averageNext4Years"] <<- NA
  uniqueNames <- unique(dfContracts[c("firstName", "lastName")])
  for (i in 1:nrow(uniqueNames)) {
    first <- uniqueNames[i,1]
    last <- uniqueNames[i,2]
    player <- dfContracts[(which(dfContracts$firstName == first & 
                                    dfContracts$lastName == last)),]
    for (s in 1:nrow(player)) {
      next1 <- player[(which(player$season == player[s,3]+1)),4]
      next2 <- player[(which(player$season == player[s,3]+2)),4]
      next3 <- player[(which(player$season == player[s,3]+3)),4]
      next4 <- player[(which(player$season == player[s,3]+4)),4]
      x <- na.omit(c(next1, next2, next3, next4))
      avg <- mean(x[length(x) > 0])
      player[s, 7] <- avg
    }
    dfContracts[(which(dfContracts$firstName == first & 
                         dfContracts$lastName == last)),] <<- player
  }
}

#-------------------------------------------------------------------------
#LOAD AWARDS 
initializeAwards <- function(year) {
  url <- paste("http://www.espn.com/nba/history/awards/_/year/", as.character(year), sep="")
  webpage <- getURL(url)
  tc <- textConnection(webpage)
  webpage <- readLines(tc) 
  close(tc)
  pageTree <<- htmlTreeParse(webpage, useInternalNodes = TRUE)
}

loadAwards <- function(begYear, endYear) {
  # loadDD(begYear,endYear)
  if(!exists("dfNames")) {
    loadDD(2000,2017)
  }
  
  dfAwards <<- data.frame(matrix(0, 8223, 18,
                                 dimnames=list(c(), c("firstName", "lastName", "team", "season", "MVP", "defensivePlayerOTY", "rookieOTY", "sixthManOTY", 
                                                      "MIP", "finalsMVP", "allNBA1stTeam", "allNBA2ndTeam", "allNBA3rdTeam", 
                                                      "allRookie1stTeam", "allRookie2ndTeam", "allDefensive1stTeam", "allDefensive2ndTeam", 
                                                      "allStarMVP"))),
                          stringsAsFactors=F)
  
  dfAwards[1:4] <<- dfNames
  
  for (y in begYear:endYear) {
    initializeAwards(y)
    
    if (y == 2000) {
      #Count the numbers of MVP, defensivePlayerOTY, rookieOTY
      for (i in 5:7) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-2), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),i] <<- 1
        }
      }
      
      #Count the numbers of sixthManOTY and MIP
      for (i in 8:9) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),i] <<- 1
        }
      }
      
      #Count number of finalsMVP
      xpath <- "//*[@id=\"my-players-table\"]/div[1]/div/table/tr[10]/td"
      vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
      if (!is.null(vec)) {
        name <- unlist(strsplit(vec[2], split = ", "))[1]
        fName <- unlist(strsplit(name, split = " "))[1]
        lName <- unlist(strsplit(name, split = " "))[2]
        team <- unlist(strsplit(vec[2], split = ", "))[2]
        season <- y
        dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                        & dfAwards$season == season)),][10] <<- 1
      }
      
      #Count number of allNBA1stTeam
      for (i in 12:16) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][11] <<- 1
        }
      }
      
      #Count number of allNBA2ndTeam
      for (i in 17:21) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][12] <<- 1
        }
      }
      
      #Count number of allNBA3rdTeam
      for (i in 22:26) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][13] <<- 1
        }
      }
      
      #Count number of allRookie1stTeam
      for (i in 27:31) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][14] <<- 1
        }
      }
      
      #Count number of allRookie2ndTeam
      for (i in 32:36) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][15] <<- 1
        }
      }
      
      #Count number of allDefensive1stTeam
      for (i in 37:41) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][16] <<- 1
        }
      }
      
      #Count number of allDefensive2ndTeam
      for (i in 42:46) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][17] <<- 1
        }
      }
      
      #Count number of allStarMVP
      xpath <- "//*[@id=\"my-players-table\"]/div[1]/div/table/tr[46]/td"
      vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
      if (!is.null(vec)) {
        name <- unlist(strsplit(vec[2], split = ", "))[1]
        fName <- unlist(strsplit(name, split = " "))[1]
        lName <- unlist(strsplit(name, split = " "))[2]
        team <- unlist(strsplit(vec[2], split = ", "))[2]
        season <- y
        dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                        & dfAwards$season == season)),][18] <<- 1
      }
      
    } else if (y == 2003) {
      
      #Count the numbers of MVP, defensivePlayerOTY, rookieOTY, sixthManOTY and MIP
      for (i in 5:9) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-2), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),i] <<- 1
        }
      }
      
      #Count number of finalsMVP
      xpath <- "//*[@id=\"my-players-table\"]/div[1]/div/table/tr[10]/td"
      vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
      if (!is.null(vec)) {
        name <- unlist(strsplit(vec[2], split = ", "))[1]
        fName <- unlist(strsplit(name, split = " "))[1]
        lName <- unlist(strsplit(name, split = " "))[2]
        team <- unlist(strsplit(vec[2], split = ", "))[2]
        season <- y
        dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                        & dfAwards$season == season)),][10] <<- 1
      }
      
      #Count number of allNBA1stTeam
      for (i in 12:16) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][11] <<- 1
        }
      }
      
      #Count number of allNBA2ndTeam
      for (i in 17:21) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][12] <<- 1
        }
      }
      
      #Count number of allNBA3rdTeam
      for (i in 22:26) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][13] <<- 1
        }
      }
      
      #Count number of allRookie1stTeam
      for (i in 27:31) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][14] <<- 1
        }
      }
      
      #Count number of allRookie2ndTeam
      for (i in 32:36) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][15] <<- 1
        }
      }
      
      #Count number of allDefensive1stTeam
      for (i in 37:41) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][16] <<- 1
        }
      }
      
      #Count number of allDefensive2ndTeam
      for (i in 42:46) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-1), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][17] <<- 1
        }
      }
      
      #Count number of allStarMVP
      xpath <- "//*[@id=\"my-players-table\"]/div[1]/div/table/tr[46]/td"
      vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
      if (!is.null(vec)) {
        name <- unlist(strsplit(vec[2], split = ", "))[1]
        fName <- unlist(strsplit(name, split = " "))[1]
        lName <- unlist(strsplit(name, split = " "))[2]
        team <- unlist(strsplit(vec[2], split = ", "))[2]
        season <- y
        dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                        & dfAwards$season == season)),][18] <<- 1
      }
      
    } else {
      #Count the numbers of MVP, defensivePlayerOTY, rookieOTY, sixthManOTY and MIP
      for (i in 5:9) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-2), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),i] <<- 1
        }
      }
      
      #Count number of finalsMVP
      xpath <- "//*[@id=\"my-players-table\"]/div[1]/div/table/tr[9]/td"
      vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
      if (!is.null(vec)) {
        name <- unlist(strsplit(vec[2], split = ", "))[1]
        fName <- unlist(strsplit(name, split = " "))[1]
        lName <- unlist(strsplit(name, split = " "))[2]
        team <- unlist(strsplit(vec[2], split = ", "))[2]
        season <- y
        dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                        & dfAwards$season == season)),][10] <<- 1
      }
      
      #Count number of allNBA1stTeam
      for (i in 12:16) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-2), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][11] <<- 1
        }
      }
      
      #Count number of allNBA2ndTeam
      for (i in 17:21) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-2), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][12] <<- 1
        }
      }
      
      #Count number of allNBA3rdTeam
      for (i in 22:26) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-2), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][13] <<- 1
        }
      }
      
      #Count number of allRookie1stTeam
      for (i in 27:31) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i-2), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][14] <<- 1
        }
      }
      
      #Count number of allRookie2ndTeam
      for (i in 32:36) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][15] <<- 1
        }
      }
      
      #Count number of allDefensive1stTeam
      for (i in 37:41) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][16] <<- 1
        }
      }
      
      #Count number of allDefensive2ndTeam
      for (i in 42:46) {
        xpath <- paste("//*[@id=\"my-players-table\"]/div[1]/div/table/tr[", as.character(i), "]/td", sep = "")
        vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
        if (!is.null(vec)) {
          name <- unlist(strsplit(vec[2], split = ", "))[1]
          fName <- unlist(strsplit(name, split = " "))[1]
          lName <- unlist(strsplit(name, split = " "))[2]
          team <- unlist(strsplit(vec[2], split = ", "))[2]
          season <- y
          dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                          & dfAwards$season == season)),][17] <<- 1
        }
      }
      
      #Count number of allStarMVP
      xpath <- "//*[@id=\"my-players-table\"]/div[1]/div/table/tr[47]/td"
      vec <- unlist(xpathApply(pageTree,xpath,xmlValue))
      if (!is.null(vec)) {
        name <- unlist(strsplit(vec[2], split = ", "))[1]
        fName <- unlist(strsplit(name, split = " "))[1]
        lName <- unlist(strsplit(name, split = " "))[2]
        team <- unlist(strsplit(vec[2], split = ", "))[2]
        season <- y
        dfAwards[(which(dfAwards$firstName == fName & dfAwards$lastName == lName
                        & dfAwards$season == season)),][18] <<- 1
      }
    }
    
  }
}

#-----------------------------------------------------------
loadData <- function() {
  if (!exists("dfAssists")) {
    loadAssists(2000,2017)
  }
  if (!exists("dfBlocks")) {
    loadBlocks(2000,2017)
  }
  if (!exists("dfContracts")) {
    loadContracts()
  }
  if (!exists("dfDD")) {
    loadDD(2000,2017)
  }
  if (!exists("dfDescriptions")) {
   loadDescriptions()
  }
  if (!exists ("dfFouls")) {
    loadFouls(2000,2017)
  }
  if (!exists("dfRebounds")) {
    loadRebounds(2000,2017)
  }
  if (!exists("dfScoring")) {
    loadScoring(2000,2017)
  }
  if (!exists("dfSteals")) {
    loadSteals(2000,2017)
  }
  if (!exists ("dfAwards")) {
    loadAwards(2000,2016)
  }
}


#------------------------------------------------------------

getNBAData <- function() {
  join1 <- sqldf(
    "SELECT *
    FROM dfScoring as df1 LEFT JOIN dfAssists as df2
    ON df1.firstName = df2.firstName
    AND df1.lastName = df2.lastName
    AND df1.season = df2.season
    AND df1.team = df2.team"
  )
  join1 <- join1[, -c(17:20)]

  join2 <- sqldf(
    "SELECT *
    FROM join1 as df1 LEFT JOIN dfRebounds as df2
    ON df1.firstName = df2.firstName
    AND df1.lastName = df2.lastName
    AND df1.season = df2.season
    AND df1.team = df2.team"
  )
  join2 <- join2[, -c(23:26)]
  
  join3 <- sqldf(
    "SELECT *
    FROM join2 as df1 LEFT JOIN dfSteals as df2
    ON df1.firstName = df2.firstName
    AND df1.lastName = df2.lastName
    AND df1.season = df2.season
    AND df1.team = df2.team"
  )
  join3 <- join3[, -c(30:33)]
  
  join4 <- sqldf(
    "SELECT *
    FROM join3 as df1 LEFT JOIN dfBlocks as df2
    ON df1.firstName = df2.firstName
    AND df1.lastName = df2.lastName
    AND df1.season = df2.season
    AND df1.team = df2.team"
  )
  join4 <- join4[, -c(34:37)]
  
  join5 <- sqldf(
    "SELECT *
    FROM join4 as df1 LEFT JOIN dfFouls as df2
    ON df1.firstName = df2.firstName
    AND df1.lastName = df2.lastName
    AND df1.season = df2.season
    AND df1.team = df2.team"
  )
  join5 <- join5[, -c(38:41)]
  
  join6 <- sqldf(
    "SELECT *
    FROM join5 as df1 LEFT JOIN dfDD as df2
    ON df1.firstName = df2.firstName
    AND df1.lastName = df2.lastName
    AND df1.season = df2.season
    AND df1.team = df2.team"
  )
  join6 <- join6[, -c(44:47)]
  
  join7 <- sqldf(
    "SELECT *
    FROM join6 as df1 LEFT JOIN dfAwards as df2
    ON df1.firstName = df2.firstName
    AND df1.lastName = df2.lastName
    AND df1.season = df2.season
    AND df1.team = df2.team"
  )
  join7 <- join7[, -c(46:49)]
  
  join8 <- sqldf(
    "SELECT *
    FROM join7 as df1 LEFT JOIN dfContracts as df2
    ON df1.firstName = df2.firstName
    AND df1.lastName = df2.lastName
    AND df1.season = df2.season"
  )
  join8 <- join8[, -c(60:62)]
  
  join9 <- sqldf(
    "SELECT *
    FROM join8 as df1 LEFT JOIN dfDescriptions as df2
    ON df1.firstName = df2.firstName
    AND df1.lastName = df2.lastName"
  )
  join9 <- join9[, -c(64:66)]
  
  join9[(which(join9$firstName == 'Gary' & join9$lastName == 'Payton' & join9$season < 2008)),c(64:69)] <- NA
  join9[(which(join9$firstName == 'Tim' & join9$lastName == 'Hardaway' & join9$season < 2004)),c(64:69)] <- NA
  
  nbaData <<- join9[c(1:2, 64:69, 3:63)]
  x <- c(
    "dfAssists",
    "dfAwards",
    "dfBlocks",
    "dfContracts",
    "dfDD",
    "dfDescriptions",
    "dfFouls",
    "dfRebounds",
    "dfScoring",
    "dfSteals"
  )
  rm(list = x, pos = ".GlobalEnv")
}

# setwd("/Users/elliotbensabat/Desktop/Northeastern/SPRING 3rd YEAR/Data collec & analysis/project nba")
# write.csv(nbaData, file = "data.csv")
