### Project: NAO index vs Cyclone tracks
# Compute tracks of winter (Oct-Mar) cyclones over the atlantic based on reanalysis of ncar cfs.
# Viszualize and analyze differences of tracks depending on NAO index.
#
# Count all cyclones that
#a) pass (track density td)
#b) exist (system density sd)
#d) are generated (genesis gen)
# Within the radius maxdist around a gridpoint and plot

#tidy up
rm(list=ls())


library("reshape2", character.only = TRUE)        # reshaping data
library("ggplot2", character.only = TRUE)         # for plots
library("RColorBrewer", character.only=TRUE)
library("mapdata", character.only = TRUE)

source("./readTracks.R")
source('./tdStatistic.R')
source("./createNAO.R")
source("./sortTracks.R")
source("./dataGgplot2.R")
source("./plotTrackDensity.R")

####-------------------- Begin Body -----------------------####

# Time span
startyear <- 1979
endyear <- 2009
nyears<-endyear-startyear+1 
period <- endyear-startyear+1

# Region
lonmin <- -80
lonmax <- 40
latmin <- 30
latmax <- 80
# Resolution
res <- 2.5
maxdist<-500
lonvec <-seq(lonmin-res,lonmax+res,res) 
latvec <-seq(latmin-res,latmax+res,res)

#inpath="../Data/test/eraint/TRACKS_MANIP/r1i1p1/"
inpath="../Data/cfsr_1979_2009/"
alltracks<-list()
td<-array(0,dim=c(length(lonvec),length(latvec),nyears))
# for nao categories
td_strongneg <-array(0,dim=c(length(lonvec),length(latvec),nyears))
td_strongpos <-array(0,dim=c(length(lonvec),length(latvec),nyears))
td_neutral   <-array(0,dim=c(length(lonvec),length(latvec),nyears))
td_negative  <-array(0,dim=c(length(lonvec),length(latvec),nyears))
td_positive  <-array(0,dim=c(length(lonvec),length(latvec),nyears))

# Create NAO data
NAO_index <- createNAO()

#Read track files
for (year in startyear:(endyear-1)){
  year2<-year+1 # change if data covers 1 year
  infile<-paste0(inpath,"trkmn.cfsr.",year,"10-",year2,"03.c_r1i1p1.t_r1i1p1.m_r1i1p1")
  tracklist <- read.tracks(filename = infile, track.type ="cyclone", verbose=TRUE)
  trackpath <- tracklist$tracks
  
  #remove all tracks outside area of interest
  in.or.out <- lapply(trackpath, function(track){
    tracklen <- length(track$x)
    outside<-TRUE
    track$x[track$x>180] <- track$x[track$x>180] - 360
    for (i in 1 : tracklen){
      if (track$x[i] < (lonmax+3*res) && track$x[i] > lonmin-3*res &&
          track$y[i] < latmax+3*res && track$y[i]>latmin-3*res) {
          outside<-FALSE}
    }  
    return(outside)
  })
  
  tracks <- trackpath[in.or.out == FALSE]
  #Track density
  yearindex<-year-startyear+1
  # td[,,yearindex]<-td[,,yearindex]+prop_grid(tracks,year,year2,lonvec,latvec,res,maxdist,'td')[,,]
  # If covering 2 years
  td[,,yearindex:(yearindex+1)]<-td[,,yearindex:(yearindex+1)]+prop_grid(tracks,year,year2,lonvec,latvec,res,maxdist,'td')
  
  # Create NAO index dependent lists
  NAO_strongneg <- list(); sn <- 1
  NAO_negative  <- list(); n <- 1
  NAO_neutral   <- list(); m <- 1
  NAO_positive  <- list(); p <- 1
  NAO_strongpos <- list(); sp <- 1
  
  # Calculate NAO phases for tracks
  sorted_tracks_lists <- sortTracks(tracks, NAO_index)
  # Seperate them
  for ( i in 1:5){
    length(sorted_tracks_lists[[5]])
    NAO_list <- sorted_tracks_lists[[i]]
    if (length(NAO_list) >= 1){
      
      for ( k in 1:length(NAO_list)){
        if (i == 1){
          NAO_strongneg[sn] <- NAO_list[k]
          names(NAO_strongneg)[sn] <- names(NAO_list[k])
          sn <- sn+1
        } else if (i == 2){
          NAO_negative[n] <- NAO_list[k]
          names(NAO_negative)[n] <- names(NAO_list[k])
          n <- n+1
        } else if (i == 3){
          NAO_neutral[m] <- NAO_list[k]
          names(NAO_neutral)[m] <- names(NAO_list[k])
          m <- m+1
        } else if (i == 4){
          NAO_positive[p] <- NAO_list[k]
          names(NAO_positive)[p] <- names(NAO_list[k])
          p <- p+1
        } else if (i == 5){
          NAO_strongpos[sp] <- NAO_list[k]
          names(NAO_strongpos)[sp] <- names(NAO_list[k])
          sp <- sp+1
        }
      }
      rm(NAO_list)
    }
  }
  
  td_strongpos[,,yearindex:(yearindex+1)]<-td_strongpos[,,yearindex:(yearindex+1)]+prop_grid(NAO_strongpos,year,year2,lonvec,latvec,res,maxdist,'td')
  td_strongneg[,,yearindex:(yearindex+1)]<-td_strongneg[,,yearindex:(yearindex+1)]+prop_grid(NAO_strongneg,year,year2,lonvec,latvec,res,maxdist,'td')
  td_neutral[,,yearindex:(yearindex+1)] <-td_neutral[,,yearindex:(yearindex+1)]+prop_grid(NAO_neutral,year,year2,lonvec,latvec,res,maxdist,'td')
  td_positive[,,yearindex:(yearindex+1)]<-td_positive[,,yearindex:(yearindex+1)]+prop_grid(NAO_positive,year,year2,lonvec,latvec,res,maxdist,'td')
  td_negative[,,yearindex:(yearindex+1)]<-td_negative[,,yearindex:(yearindex+1)]+prop_grid(NAO_negative,year,year2,lonvec,latvec,res,maxdist,'td')
  
} # end outer for loop over all years

# Calculate trackdensity mean for each grid point in ggplot format
tdlist <- dataGgplot2(td, nyears, lonvec, latvec)
tdlist_sneg <- dataGgplot2(td, nyears, lonvec, latvec)
tdlist_neg  <- dataGgplot2(td, nyears, lonvec, latvec)
tdlist_neu  <- dataGgplot2(td, nyears, lonvec, latvec)
tdlist_pos  <- dataGgplot2(td, nyears, lonvec, latvec)
tdlist_spos <- dataGgplot2(td, nyears, lonvec, latvec)


####------------------- PLOT -------------------------------####

plotTrackDensity(tdlist, lonmin, lonmax, latmin, latmax, startyear, endyear)
