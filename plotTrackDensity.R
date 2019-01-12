### Project: NAO vs cyclones
# Plot the given track density over North Atlantic

#library("ggplot2", character.only = TRUE)         # for plots
#library("RColorBrewer", character.only=TRUE)
#library("mapdata", character.only = TRUE)

plotTrackDensity <- function(tdlist, lonmin, lonmax, latmin, latmax, syear, eyear, plot_save = TRUE){

  areamap <- map_data('world')
  
  plot <- ggplot()+
    geom_raster(data = tdlist, aes(x = long, y = lat, fill = Trackdensity),interpolate = TRUE) +
    scale_fill_gradientn(colours = brewer.pal(8, "OrRd"), na.value = NA) +
    # add contour lines
    geom_contour(data = tdlist,aes(x = long, y = lat, z=Trackdensity),color="gray",breaks=seq(20,100,10), show.legend = TRUE) +
    # TODO: add text to contours
    # geom_text(tdlist, aes(label=Trackdensity))
    # background
    theme_bw() +
    geom_polygon(data = areamap, inherit.aes= FALSE, aes(x=long, y = lat, group = group), color = "black", fill = "grey80", alpha=0)+
    # set plot region
    coord_fixed(xlim = c(lonmin+2,lonmax-2),  ylim = c(latmin,latmax), ratio = 1) +
    # add title
    ggtitle(paste0("Trackdensity of cyclones [", startyear, "-", endyear, "]"))
  
  # Save plot
  if (plot_save == TRUE){
    # I/O
    outfile <- paste0("td_", syear, "_", eyear, ".png")
    ggsave(plot,filename=outfile, path = "../Plots", device = "png")
  }
  
  plot(plot)
  
}

