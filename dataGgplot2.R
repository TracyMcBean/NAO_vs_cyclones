### Project: NAO vs cyclones
# Compute mean for each grid point.
# Create dataframe for ggplot2 use.

dataGgplot2_years <- function(td_frame, nyears, lonvec, latvec){
  #winter mean
  mean_grid<-rowSums(td_frame,dims=2)/(nyears-1) #winter mean
  
  #format suitable for ggplot
  L<-list()
  L$lon<-lonvec
  L$lat<-latvec
  L$td<-mean_grid
  dimnames(L$td) <- list(long = L$lon, lat = L$lat)
  tmp_tdlist <- melt(L$td, value.name = "Trackdensity")
  
  return(tmp_tdlist)
}

dataGgplot2_days <- function(td_frame, ndays, lonvec, latvec){
  #winter mean
  mean_grid<-rowSums(td_frame,dims=2)/(ndays) #daily mean
  
  #format suitable for ggplot
  L<-list()
  L$lon<-lonvec
  L$lat<-latvec
  L$td<-mean_grid
  dimnames(L$td) <- list(long = L$lon, lat = L$lat)
  tmp_tdlist <- melt(L$td, value.name = "Trackdensity")
  
  return(tmp_tdlist)
}
