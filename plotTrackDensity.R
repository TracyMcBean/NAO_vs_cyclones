### Project: NAO vs cyclones
# Plot the given track density over North Atlantic

#library("ggplot2", character.only = TRUE)         # for plots
#library("RColorBrewer", character.only=TRUE)
#library("mapdata", character.only = TRUE)

#' Plot trackdenisty of cyclones.
#' 
#' @param tdlist List containing longitude, latitude and Trackdensity
#' @param syear Starting year
#' @param eyear Ending year
#' @param NAO Logical indicator if tracks are only for specific NAO phase
#' @param NAO_phase Integer indicating which NAO phase: 1 = strong negative, 2 = negative, 3 = neutral, 4 = positive, 5 = strong positive, 6 = all
plotTrackDensity <- function(tdlist, lonmin, lonmax, latmin, latmax, syear, eyear, NAO_phase = 6, plot_save = FALSE){
  
  if (NAO_phase > 6| NAO_phase < 1){
    stop("NAO indicator must be between 1 and 6")
  }
  phase <- c("strneg", "negative", "neutral", "positive", "strpos", "all")
  
  areamap <- map_data('world')
  
  plot <- ggplot()+
    geom_raster(data = tdlist, aes(x = long, y = lat, fill = Trackdensity),interpolate = TRUE) +
    scale_fill_gradientn(colours = brewer.pal(8, "OrRd"), na.value = NA) +
    # add contour lines
    geom_contour(data = tdlist,aes(x = long, y = lat, z=Trackdensity),color="gray",breaks=seq(0,1,0.1), show.legend = TRUE) +
    # TODO: add text to contours
    # geom_text(tdlist, aes(label=Trackdensity))
    # background
    theme_bw() +
    geom_polygon(data = areamap, inherit.aes= FALSE, aes(x=long, y = lat, group = group), color = "black", fill = "grey80", alpha=0)+
    # set plot region
    coord_fixed(xlim = c(lonmin+2,lonmax-2),  ylim = c(latmin,latmax), ratio = 1) +
    # add title
    ggtitle(paste0("Trackdensity per day of cyclones [", startyear, "-", endyear, "] NAO ", phase[NAO_phase]))
  
  # Save plot
  if (plot_save == TRUE){
    # I/O
    if (NAO_phase < 6){
      outfile <- paste0("tdperday_NAO_", phase[NAO_phase],"_", syear, "_", eyear, ".png")
    } else (outfile <- paste0("tdperday_", syear, "_", eyear, ".png") )
    
    ggsave(plot,filename=outfile, path = "../Plots", device = "png")
  }
  
  plot(plot)
  
}

#'Plot normalized trackdensity
plotNormalizedTd <- function(tdlist, lonmin, lonmax, latmin, latmax, syear, eyear, NAO_phase = 6, plot_save = FALSE){
  
  if (NAO_phase > 6| NAO_phase < 1){
    stop("NAO indicator must be between 1 and 6")
  }
  phase <- c("strneg", "neg", "neu", "pos", "strpos", "all")
  
  areamap <- map_data('world')
  
  # Normalize
  tdlist$Trackdensity <- tdlist$Trackdensity/max(tdlist$Trackdensity)
  
  plot <- ggplot()+
    geom_raster(data = tdlist, aes(x = long, y = lat, fill = Trackdensity),interpolate = TRUE) +
    scale_fill_gradientn(colours = brewer.pal(8, "Greens"), na.value = NA) +
    # add contour lines
    geom_contour(data = tdlist,aes(x = long, y = lat, z=Trackdensity),color="gray",breaks=seq(0,1,0.2), show.legend = TRUE) +
    # TODO: add text to contours
    # geom_text(tdlist, aes(label=Trackdensity))
    # background
    theme_bw() +
    geom_polygon(data = areamap, inherit.aes= FALSE, aes(x=long, y = lat, group = group), color = "black", fill = "grey80", alpha=0)+
    # set plot region
    coord_fixed(xlim = c(lonmin+2,lonmax-2),  ylim = c(latmin,latmax), ratio = 1) +
    # add title
    ggtitle(paste0("Normalized trackdensity of cyclones [", startyear, "-", endyear, "] NAO ", phase[NAO_phase]))
  
  # Save plot
  if (plot_save == TRUE){
    # I/O
    if (NAO_phase < 6){
      outfile <- paste0("td_norm_NAO_", phase[NAO_phase],"_", syear, "_", eyear, ".png")
    } else (outfile <- paste0("td_norm", syear, "_", eyear, ".png") )
    
    ggsave(plot,filename=outfile, path = "../Plots", device = "png")
  }
  
  plot(plot)
  
}

#' Difference between two normalized trackdensities.
plotTdVsTd <- function(tdlist, tdlist2, lonmin, lonmax, latmin, latmax, syear, eyear, NAO_phase = 6, NAO_phase2 = 6, plot_save = FALSE){
  
  phase <- c("strneg", "neg", "neu", "pos", "strpos", "all")
  
  areamap <- map_data('world')
  
  # Normalize
  tdlist$Trackdensity <- tdlist$Trackdensity/max(tdlist$Trackdensity)
  tdlist2$Trackdensity <- tdlist2$Trackdensity/max(tdlist2$Trackdensity)
  
  # Eulerian distance
  tdlist$Trackdensity <-  sqrt(tdlist$Trackdensity^2 + tdlist2$Trackdensity^2)
  
  plot <- ggplot()+
    geom_raster(data = tdlist, aes(x = long, y = lat, fill = Trackdensity),interpolate = TRUE) +
    scale_fill_gradientn(colours = brewer.pal(8, "Greens"), na.value = NA) +
    # add contour lines
    geom_contour(data = tdlist,aes(x = long, y = lat, z=Trackdensity),color="gray",breaks=seq(0,1,0.2), show.legend = TRUE) +
    # TODO: add text to contours
    # geom_text(tdlist, aes(label=Trackdensity))
    # background
    theme_bw() +
    geom_polygon(data = areamap, inherit.aes= FALSE, aes(x=long, y = lat, group = group), color = "black", fill = "grey80", alpha=0)+
    # set plot region
    coord_fixed(xlim = c(lonmin+2,lonmax-2),  ylim = c(latmin,latmax), ratio = 1) +
    # add title
    ggtitle(paste0("Difference of norm. TD [", startyear, "-", endyear, "] NAO ", phase[NAO_phase], " vs ", phase[NAO_phase2]))
  
  # Save plot
  if (plot_save == TRUE){
    # I/O
    outfile <- paste0(phase[NAO_phase2], " vs ", phase[NAO_phase],"_", syear, "_", eyear, ".png")
    ggsave(plot,filename=outfile, path = "../Plots", device = "png")
  }
  
  plot(plot)
  
}
