### Project: NAO vs cyclones
# Contains function to generate NAO index data in the same format as the cyclone data
# Uses as basis daily NAO index from 1950 to 2018.

# Monthly NAO Index from 1950 to 2018.
# NAO_index <- read.csv("../Data/nao_1950_2018_monthly.csv", sep = ";"
#                      , header = FALSE, col.names = c("Year", c(month.abb[1:12])) )

createNAO <- function(){
  NAO_index <- read.csv("../Data/daily_nao_1950current.csv", sep = ";"
                        , header = FALSE, col.names = c("Year", "Month", "Day", "Index"),
                        stringsAsFactors = FALSE, 
                        colClasses = c("character", "character", "character", "numeric"))

  # Filter out the ones I need ->  1979 - 2009
  NAO_index <- subset(NAO_index, subset(Year >= 1979, Year <= 2009))
  # Fit date format of NAO_index
  for (i in 1:length(NAO_index$Day)){
    day <- NAO_index$Day[i]
    if (day < 10){
      NAO_index$Day[i] <- gsub(" ", "", paste0("0",day))
    }
    month <- NAO_index$Month[i]
    if (month < 10){
      NAO_index$Month[i] <- gsub(" ", "", paste0("0",month))
    }
  }

  # Merge to single date -> same format as TrackTime
  NAO_index$Date <- gsub(" ", "", paste0(NAO_index$Year, NAO_index$Month, NAO_index$Day))
  NAO_index$Date <- as.numeric(NAO_index$Date)

  which(is.na(NAO_index$Date == TRUE))
  # Two rows do not have data.
  na.omit(NAO_index)

  return(NAO_index)
}

# Definition for neutral, positive or negative index, using paper by Pinto 2008:
# strong negative: < -1.5
# negative: -1.5 to -0.5 
# neutral: 0.5 to -0.5
# positive: 0.5 to 1.5
# strong positive:  > 1.5


