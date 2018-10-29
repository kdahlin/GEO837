#### GEO 837 - Remote Sensing of the Biosphere ####
#### Author: Kyla M. Dahlin, MSU Geography ########
#### making climate plots from worldclim data #####

# note - in RStudio go to 'Tools > Global Options' to change background color,
# font size, and 'soft-wrap R source files' (in 'Code')

# load packages to do these analyses
# note if this is new to you you'll need to first run
#install.packages(c("raster", "rgdal", "maptools"))
library(raster)
library(rgdal)
library(maptools)

# set your working directory
setwd("where you want plots to get written")

# what day is it? use this to add to output file names to keep track of when you
# made something
today <- "20181029"

# because our data is stored elsewhere, let's also give that a name
# Kyla downloaded this data from worldclim.org, it is commonly used (though not
# perfect data for modern data climate)
# the reference for this data is Fick et al 2017, which is in the 'extra'
# readings section on D2L
data.path <- "where you stored the worldclim data"

# read in a file just to check it out
in.tavg <- raster(paste0(data.path, "wc2.0_10m_tavg/wc2.0_10m_tavg_06.tif"))

# take a look

plot(in.tavg)

# but what we really want is to get ALL of the data just for one location
# (the kml file of your path/row)
# note that this is assuming you have a kmz file (zipped kml) outlining the
# boundary of your study area, and it is located in your working directory
# first unzip the KMZ to a kml (this creates a file in your working directory,
# called doc.kml which I don't love...)
kml.unzipped <- unzip("a kmz file outlining your area of interest", 
                      files = "doc.kml",
                      unzip = "internal")

# this is UPDATED!!! turns out the other function is old, this is better.
# everything else will still run the same, ish.
in.kml <- readOGR(kml.unzipped)

# look at the output, which is now a 'spatialPolygonsDataFrame'
in.kml

# convert to a geographic extent object - NOTE: for us we know we have a kml
# with a single polygon in it, so getting the extent is more or less the same
# as looking at a rectangle. If you read in a kml with multiple polygons, this
# would give you the outer extent of ALL Of them.
in.kml.ext <- extent(in.kml)

# look at it to see format
in.kml.ext

# you can plot it on your temperature map (if you still have it open), too
plot(in.kml.ext, add = TRUE)

# now extract TAVG for your location
TAVG.loc <- extract(in.tavg, in.kml.ext, method = 'simple', fun = mean, 
                    na.rm = TRUE)

# did that give us a single, reasonable value?
TAVG.loc

# OK, but what we really want is tmin, tmean, tavg, and precip for each month
# so we need to do some more work!

# first we need a dataframe to put our outputs into
out.clim <- matrix(NA, nrow = 12, ncol = 5)
out.clim <- as.data.frame(out.clim)

# what does it look like?
out.clim

# let's give it some names
names(out.clim) <- c("month", "TMIN", "TAVG", "TMAX", "PRECIP")

# and let's assign number months to the first column
out.clim[,1] <- 1:12

# now what we want to do is open each raster file, extract the data for our kml
# polygon, and write it into the proper place in our table
# to do that we'll need to use a 'for-loop', but first we should get all of
# our filenames and paths sorted out
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11",
            "12")

TMIN.files <- paste0("wc2.0_10m_tmin_", months, ".tif")
TAVG.files <- paste0("wc2.0_10m_tavg_", months, ".tif")
TMAX.files <- paste0("wc2.0_10m_tmax_", months, ".tif")
PRECIP.files <- paste0("wc2.0_10m_prec_", months, ".tif")

for (m in 1:12) {
  in.tmin <- raster(paste0(data.path, 
                           "wc2.0_10m_tmin/", 
                           TMIN.files[m]))
  out.clim$TMIN[m] <- extract(in.tmin, 
                              in.kml.ext, 
                              method = 'simple', 
                              fun = mean, 
                              na.rm = TRUE)
  
  in.tavg <- raster(paste0(data.path, 
                           "wc2.0_10m_tavg/", 
                           TAVG.files[m]))
  out.clim$TAVG[m] <- extract(in.tavg, 
                              in.kml.ext, 
                              method = 'simple', 
                              fun = mean, 
                              na.rm = TRUE)
  
  in.tmax <- raster(paste0(data.path, 
                           "wc2.0_10m_tmax/", 
                           TMAX.files[m]))
  out.clim$TMAX[m] <- extract(in.tmax, 
                              in.kml.ext, 
                              method = 'simple', 
                              fun = mean, 
                              na.rm = TRUE)
  
  in.precip <- raster(paste0(data.path, 
                             "wc2.0_10m_prec/", 
                             PRECIP.files[m]))
  out.clim$PRECIP[m] <- extract(in.precip, 
                                in.kml.ext, 
                                method = 'simple', 
                                fun = mean, 
                                na.rm = TRUE)
  
  print(m)
}

# check and make sure you actually got values
out.clim

### NOW TO MAKE A PLOT! GRAYSCALE FIRST 

# first get max and min values for temperature
max.temp <- max(out.clim$TMAX)
min.temp <- min(out.clim$TMIN)
dummy.temp <- c(rep(min.temp, 6), rep(max.temp, 6))
title = "Climate of the Carrizo Plain, California (1970-2000 avg)"

months.letters <- c("J","F","M","A","M","J","J","A","S","O","N","D")

tiff(paste0("carrizo_climate_plot_", today, ".tif"), 
     units = "in", 
     width = 6.5, 
     height = 4.5, 
     res = 200)
  par(mar = c(4,4,4,4))

  barplot(c(rep(0,6), 
            rep(max(out.clim$PRECIP, na.rm = T) + 5, 6)), 
          col = NA, 
          border = NA, 
          xaxt = "n", 
          yaxt = "n", 
          axes = F)
  barplot(out.clim$PRECIP, 
          pch = 16, 
          col = "gray50", 
          border = "gray50", 
          yaxt = "n", 
          add = T, 
          axes = F, 
          names.arg = months.letters, 
          xlab = "Month")
  axis(side=4, at = pretty(range(out.clim$PRECIP, na.rm = T)))
  mtext("Precipitation (mm)", side = 4, line = 2.3, cex = 0.8)

  par(new = T)
  plot(1:12, dummy.temp, type = "n", xaxt = "n", 
       xlab = NA, ylab = NA, main = title)
  mtext("Temperature (C)", side = 2, line = 2.3, cex = 0.8)
  lines(out.clim$TMIN, col = "gray80", lty = 1, lwd = 1)
  lines(out.clim$TMAX, col = "gray80", lty = 1, lwd = 1)
  lines(out.clim$TAVG, col = "black", lty = 1, lwd = 2)

dev.off()







