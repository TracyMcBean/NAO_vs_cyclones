### Project: NAO vs cyclones
#
# Contains: sortTracks, getMinPressure, getTracklength, getMaxLaplacian
#
# In sortTracks:
# Find tracks starting day and ending day. 
# Take this as period for mean index during these days.

#'Sort given tracks depending on their NAO_index.
sortTracks <- function(tracks, nao_index, verbose=FALSE){
  # Initialize list for dataframes of tracks for each NAO category
  # Also initialize counters for each
  NAO_negneg <- list(); nn <- 1
  NAO_neg    <- list(); n <- 1
  NAO_neu    <- list(); m <- 1
  NAO_pos    <- list(); p <- 1
  NAO_pospos <- list(); pp <- 1
  
  # Create a dataframe to save starting and ending days.
  TrackTime <- as.data.frame(array(NA, c(length(tracks),4)))
  colnames(TrackTime) <- c("TrackID", "startDay", "endDay", "avg_NAO")
  
  # Get the ending and starting day
  for (i in 1:length(tracks)) {
    TrackTime$TrackID[i] <- names(tracks)[i]
    single_track <- as.data.frame(tracks[i][1])
    TrackTime$startDay[i] <- single_track[1,2]
    TrackTime$endDay[i] <- single_track[length(single_track[,2]),2]
  }

  # Take average of NAO index for the period of each cyclone
  for (i in 1:length(tracks)){
    nao_sub <- subset(NAO_index, (Date >= TrackTime$startDay[i]) & (Date <= TrackTime$endDay[i]),
                      select = Index)
    TrackTime$avg_NAO[i] <- mean(nao_sub[,1])
    track_id <- TrackTime$TrackID[i]
    
    # Save into fitting list
    if (TrackTime$avg_NAO[i] < -1.5){
      # strong negative
      NAO_negneg[[nn]] <- get(track_id, tracks)
      names(NAO_negneg)[nn] <- track_id
      nn <- nn + 1
      
    } else if ((TrackTime$avg_NAO[i] > -1.5) & (TrackTime$avg_NAO[i] <= -0.5)) {
      # negative
      NAO_neg[[n]]     <- get(track_id, tracks)
      names(NAO_neg)[n] <- track_id
      n <- n + 1
      
    } else if ((TrackTime$avg_NAO[i] > -0.5) & (TrackTime$avg_NAO[i] <= 0.5)) {
      # neutral
      NAO_neu[[m]] <- get(track_id, tracks)
      names(NAO_neu)[m] <- track_id
      m <- m + 1
      
    } else if ((TrackTime$avg_NAO[i] > 0.5) & (TrackTime$avg_NAO[i] <= 1.5)) { 
      # positive
      NAO_pos[[p]]     <- get(track_id, tracks)
      names(NAO_pos)[p] <- track_id
      p <- p + 1
      
    } else if (TrackTime$avg_NAO[i] > 1.5) {
      # strong positive
      NAO_pospos[[pp]] <- get(track_id, tracks)
      names(NAO_pospos)[pp] <- track_id
      pp <- pp + 1
    }
  }
  
  return_list <- list(NAO_negneg, NAO_neg, NAO_neu, NAO_pos, NAO_pospos)
  names(return_list) <- c("negneg", "neg", "neutral", "pos", "pospos")
  # return all lists of data frames
  return(return_list)
}


#'Get tracklength in days for the cyclones contained in tracks
getTracklength <- function(tracks){
  
  tracklength <- array(0, length(tracks))
  if (length(tracks)>0){
    # Go threw all tracks, calculating the period in h
    for (i in 1:length(tracks)){
      single_track <- as.data.frame(tracks[i][1])
      # times 6 to get hours (6 hour steps)
      tracklength[i] <- length(single_track[,1]) * 6
    }
  } else {
    tracklength[1] <- 0
  }
  
  return(tracklength)
}

#' Calculate minimum pressure for given tracks
getMinPressure <- function(tracks){
  
  minp <- array(0, length(tracks))
  
  if (length(tracks)>0){
    # Go threw all tracks and get min pressure
    for (i in 1:length(tracks)){
      single_track <- as.data.frame(tracks[i][1])
      # times 6 to get hours (6 hour steps)
      minp[i] <- min(single_track[,10])
    }
    
  } else {
    minp[1] <-  0
  }
  
  return(minp)
}

#' Get maximum laplacian
getMaxLaplacian <- function(tracks){
  
  maxlc <- array(0, length(tracks))
  
  if (length(tracks)>0){
    # Go threw all tracks and get min pressure
    for (i in 1:length(tracks)){
      single_track <- as.data.frame(tracks[i][1])
      # times 6 to get hours (6 hour steps)
      maxlc[i] <- max(single_track[,11])
    }
    
  } else {
    maxlc[1] <-  0
  }
  
  return(maxlc)
}
