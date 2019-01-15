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

# Count of tracks for each category
nr_tracks_nao <- c(0,0,0,0,0)
names(nr_tracks_nao) <- c("strongneg", "negative", "neutral", "positive", "strongpos")

# Array for tracklength
tl_strneg <- c(); tl_neg <- c(); tl_neu <- c(); tl_pos <- c(); tl_strpos <- c()
mp_strneg <- c(); mp_neg <- c(); mp_neu <- c(); mp_pos <- c(); mp_strpos <- c()

# Create NAO data
NAO_index <- createNAO()
NAO_index$Cyclones <- 0

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
  
  # remove counter
  rm(i, k, sn, n , p, sp, m)
  
  # Count tracks for each NAO category
  nr_tracks_nao <- nr_tracks_nao + c(length(NAO_strongneg), length(NAO_negative), length(NAO_neutral),
                                     length(NAO_positive), length(NAO_strongpos))

  # track density for each NAO category
  td_strongneg[,,yearindex:(yearindex+1)]<-td_strongneg[,,yearindex:(yearindex+1)]+prop_grid(NAO_strongneg,year,year2,lonvec,latvec,res,maxdist,'td')
  td_negative[,,yearindex:(yearindex+1)] <-td_negative[,,yearindex:(yearindex+1)]+prop_grid(NAO_negative,year,year2,lonvec,latvec,res,maxdist,'td')
  td_neutral[,,yearindex:(yearindex+1)]  <-td_neutral[,,yearindex:(yearindex+1)]+prop_grid(NAO_neutral,year,year2,lonvec,latvec,res,maxdist,'td')
  td_positive[,,yearindex:(yearindex+1)] <-td_positive[,,yearindex:(yearindex+1)]+prop_grid(NAO_positive,year,year2,lonvec,latvec,res,maxdist,'td')
  td_strongpos[,,yearindex:(yearindex+1)]<-td_strongpos[,,yearindex:(yearindex+1)]+prop_grid(NAO_strongpos,year,year2,lonvec,latvec,res,maxdist,'td')
  
  # Calculate number of cyclones for each day. Using starting day of cyclone as indicator
  cat("Counting tracks per day... \n")
  names_tracks <- names(tracks)
  sub_NAO <- subset(NAO_index, Year == year | Year == year2)
  for (d in 1:length(sub_NAO$Date)){
    for (trackid in 1:length(names_tracks)){
      if (sub_NAO$Date[d] == tracks[[names_tracks[trackid]]]$da[1]){
        NAO_index$Cyclones[which(NAO_index$Date == sub_NAO$Date[d])] <-  NAO_index$Cyclones[which(NAO_index$Date == sub_NAO$Date[d])] + 1
      }
    }
  }
  rm(sub_NAO, d, trackid)
  
  # Save tracklengths (row for tracks, column for phase)
  tl_strneg <- c(tl_strneg, getTracklength(NAO_strongneg))
  tl_neg    <- c(tl_neg, getTracklength(NAO_negative))
  tl_neu    <- c(tl_neu, getTracklength(NAO_neutral))
  tl_pos    <- c(tl_pos, getTracklength(NAO_positive))
  tl_strpos <- c(tl_strpos, getTracklength(NAO_strongpos))

  # Save Minimum pressure (row for tracks, column for phase)
  mp_strneg <- c(mp_strneg, getMinPressure(NAO_strongneg))
  mp_neg    <- c(mp_neg, getMinPressure(NAO_negative))
  mp_neu    <- c(mp_neu, getMinPressure(NAO_neutral))
  mp_pos    <- c(mp_pos, getMinPressure(NAO_positive))
  mp_strpos <- c(mp_strpos, getMinPressure(NAO_strongpos))
  
} # end outer for loop over all years


# Get NAO days for each category
nr_NAO_days <- calcNAOdays(NAO_index)

# Calculate trackdensity mean for each grid point in ggplot format
# td of all types per year
tdlist <- dataGgplot2_years(td, nyears, lonvec, latvec)
# Average over days
tdlist_days <- dataGgplot2_years(td, , lonvec, latvec)

# td for each NAO phase. Mean for single day.
tdlist_sneg <- dataGgplot2_days(td_strongneg, nr_NAO_days[1], lonvec, latvec)
tdlist_neg  <- dataGgplot2_days(td_negative, nr_NAO_days[2], lonvec, latvec)
tdlist_neu  <- dataGgplot2_days(td_neutral, nr_NAO_days[3], lonvec, latvec)
tdlist_pos  <- dataGgplot2_days(td_positive, nr_NAO_days[4], lonvec, latvec)
tdlist_spos <- dataGgplot2_days(td_strongpos, nr_NAO_days[5], lonvec, latvec)


####------------------- PLOT -------------------------------####
# Incase of NAO distinction add NAO = TRUE and NAO_phase ID
# contour lines have to be modified.
plotTrackDensity(tdlist, lonmin, lonmax, latmin, latmax, startyear, endyear, NAO_phase = 1, plot_save=FALSE)

plotNormalizedTd(tdlist_pos, lonmin, lonmax, latmin, latmax, startyear, endyear, NAO_phase = 4, plot_save = FALSE)

plotTdVsTd(tdlist_neg, tdlist_pos, lonmin, lonmax, latmin, latmax, startyear, endyear, NAO_phase = 2, 4, plot_save = FALSE)


####------------------ STATISTICS-------------------------####
# Some random stuff to get a better impression of the data
count <- 1
lat_td <- c()
for (l in latvec){
  lat_td[count] <- sum(subset(tdlist_neg, lat == l, select = Trackdensity))
  count <- count + 1
}
count <- 1
lon_td <- c()
for (l in lonvec){
  lon_td[count] <- sum(subset(tdlist_neg, long == l, select = Trackdensity))
  count <- count + 1
}
plot(lonvec, lon_td)

# create matrix 
data_matrix <- array(0, dim = c(length(latvec), length(lonvec)))
j = 1
for (i in 1:length(latvec)){
  data_matrix[i,1:length(lonvec)] <- tdlist[j:(j+length(lonvec)-1),3]
  j = j + length(lonvec)
}

### NAO phase and Cyclone count
nr_NAO_days_per <- as.numeric(nr_NAO_days)/sum(nr_NAO_days)*100
nr_tracks_nao_per <- as.numeric(nr_tracks_nao)/sum(nr_tracks_nao)*100

# Days and cyclones, in percent of all days 
plot( c(1:15), c(NA, nr_NAO_days_per[1], NA, NA, nr_NAO_days_per[2], NA, NA, 
                 nr_NAO_days_per[3], NA, NA, nr_NAO_days_per[4], NA, 
                 NA, nr_NAO_days_per[5], NA),  
      type= "h", col= "darkred", lwd= 5, 
      main= "Distribution NAO phase/Cyclones 1979-2009", 
      xaxt = "n", xlab = "NAO index", ylab= "Percentage", ylim = c(2,50))
axis (side = 1, at = c(2, 5, 8, 11, 14) , 
      labels = c("strneg", "neg", "neu", "pos", "strpos"),
      tick = TRUE, lwd.ticks = 0)
abline(h=seq(10,50,10), lty = "dashed", col="gray")
points(c(1:15), c(NA, nr_NAO_days_per[1], NA, NA, nr_NAO_days_per[2], NA, NA, 
       nr_NAO_days_per[3], NA, NA, nr_NAO_days_per[4], NA, 
       NA, nr_NAO_days_per[5], NA), type= "h", col= "darkred", lwd= 5)
points( c(1:15), c(NA, NA, nr_tracks_nao_per[1], NA, NA, nr_tracks_nao_per[2], NA, NA, 
                 nr_tracks_nao_per[3], NA, NA, nr_tracks_nao_per[4], NA, 
                 NA, nr_tracks_nao_per[5]), 
        col = "darkgreen", type = "h", lwd = 5)
legend("topleft", legend=c("NAO phase", "cyclones"), col = c("firebrick", "darkgreen"),
       lty = c("solid", "solid"), lwd = 2, bty = "n")

#### T-Test ####
## We want to know if the difference between the positive and negative phase is significant
## Are there more cyclones during the NAO positive phase?

## How many cyclones do we find for each day 
# separate negative and positive index
# Count the amount of cyclones for each day with each index.
# A cyclone covers several days... When does it qualify as belonging to a day? I will take the starting day as I will otherwise have issues with counting days twice 
# 
# VAR: NAO_*_days, All days that belong to a specific category of NAO index (the *)
NAO_pos_days <- subset(NAO_index, Index >= 0.5)
NAO_neg_days <- subset(NAO_index, Index <= -0.5)

count_neg <- c(rep(0,max(NAO_neg_days$Cyclones)+1) )
count_pos <- c(rep(0,max(NAO_pos_days$Cyclones)+1) )

# count number of cyclones per day
for ( c in 0:max(NAO_neg_days$Cyclones)){
  count_neg[c+1] <- nrow(subset(NAO_neg_days, Cyclones == c, select = Cyclones))
}
for ( c in 0:max(NAO_pos_days$Cyclones)){
  count_pos[c+1] <- nrow(subset(NAO_pos_days, Cyclones == c, select = Cyclones))
}
rm(c)

plot(c(0:17), count_neg)
plot(c(0:(length(count_pos)-1)), count_pos)

plot(c(0:17), count_neg/max(count_neg), type = "l", col = "darkblue", lwd = 2,
     ylab="Normalized occurrences", xlab = "Number of cyclones",
     main = "Number of cyclones counted per day")
points(c(0:14), count_pos/max(count_pos), type="l", col= "firebrick", lwd = 2)
legend("topright", legend = c("negative", "positive"), col = c("darkblue", "firebrick"), 
       lty = c("solid", "solid"), lwd = 2)

### Cyclone length ###-------------------
# negative phase
c <- 1
count_tl_neg <- c(0)
for ( tl in seq(30,max(tl_neg),6)){
  count_tl_neg[c] <-length(which(tl_strneg == tl)) + length(which(tl_neg == tl))
  c <- c + 1
}
# positive
c <- 1
count_tl_pos <- c(0)
for ( tl in seq(30,max(tl_pos),6)){
  count_tl_pos[c] <- length(which(tl_strpos == tl)) + length(which(tl_pos == tl))
  c <- c + 1
}
rm(tl,c)

plot( seq(30,max(tl_neg),6), count_tl_neg/max(count_tl_neg), type = "l", col = "darkblue", lwd = 2,
     ylab="Normalized occurrences", xlab = "Lifetime [h]",
     main = "Cyclone lifetime")
points(seq(30,max(tl_pos),6), count_tl_pos/max(count_tl_pos), type="l", col= "firebrick", lwd = 2)
legend("topright", legend = c("negative", " positive"), col = c("darkblue", "firebrick"), 
       lty = c("solid", "solid"), lwd = 2, bty = "n")

### Minimum pressure ###-------------------
# negative phase
c <- 1
count_mp_neg <- c(0)
for ( mp in seq(910,1025,0.50)){
  count_mp_neg[c] <-length(which(mp_neg <= mp & mp_neg > mp-0.50))
  c <- c + 1
}

# positive
c <- 1
count_mp_pos <- c(0)
for ( mp in seq(910,1025,0.50)){
  count_mp_pos[c] <- length(which(mp_pos <= mp & mp_pos > mp-0.50))
  c <- c + 1
}
rm(mp,c)

# plot for minimum pressure
plot( seq(910,1025,0.50), count_mp_neg/max(count_mp_neg), type = "l", col = "darkblue", lwd = 2,
      ylab="Normalized occurrences", xlab = "Pressure [hPa]",
      main = "Minimum Core Pressure")
points(seq(910,1025,0.50), count_mp_pos/max(count_mp_pos), type="l", col= "firebrick", lwd = 2)
legend("topleft", legend = c("negative", "positive"), col = c("darkblue", "firebrick"), 
       lty = c("solid", "solid"), lwd = 2, bty = "n")
abline(h=seq(0,1,0.2), lty = "dashed", col= "gray")
points(seq(910,1025,0.50), count_mp_neg/max(count_mp_neg), type = "l", col = "darkblue", lwd = 2)

## T-Test for this case
mp_pos_mean <- mean(mp_pos); mp_neg_mean <- mean(mp_neg)
mp_pos_sd <- sd(mp_pos); mp_neg_sd <- sd(mp_neg)

#zero hypothis

# Welch-Test (Standarddeviation not the same)
t <- (mp_neg_mean - mp_pos_mean) /sqrt(var(mp_neg)/length(mp_neg) + var(mp_pos)/length(mp_pos))

# degrees of freedom 
df <- (var(mp_neg)/length(mp_neg) + var(mp_pos)/length(mp_pos))^2 / ( var(mp_neg)^2/(length(mp_neg)^2*(length(mp_neg-1)))
                                                                     + var(mp_pos)^2/(length(mp_pos)^2*(length(mp_pos))) )
p_val <- pt(t, df)
