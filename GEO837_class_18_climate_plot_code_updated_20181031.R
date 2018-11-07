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
setwd("C:/Users/kdahlin/Dropbox/MSU_GEO837/R_section/")

# what day is it? use this to add to output file names to keep track of when you
# made something
today <- "20181029"

# because our data is stored elsewhere, let's also give that a name
# Kyla downloaded this data from worldclim.org, it is commonly used (though not
# perfect data for modern data climate)
# the reference for this data is Fick et al 2017, which is in the 'extra'
# readings section on D2L
data.path <- "C:/Users/kdahlin/Dropbox/MSU_GEO837/R_section/worldclim2/"

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
kml.unzipped <- unzip("carrizo_box_kml.kmz", 
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

# make a plot!
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

# I'm leaving as an open exercise changing the colors to something more fun
# than grayscale. Check out colorbrewer2.org for good ideas.

###############################################################################
#################### BASIC RASTER MANIPULATION ################################
###############################################################################

# now let's read in and stack the temperature rasters to look at patterns.

# first read in the january tavg data as a single raster
tavg.stack <- raster(paste0(data.path, 
                            "wc2.0_10m_tavg/", 
                            TAVG.files[1]))

# take a look at the data in this single layer geotiff
tavg.stack

# one thing to check is where are the NA values
plot(is.na(tavg.stack))
# this will return a map where 1 = NA values, 0 = non-NA values

# then write a for-loop to read in each subsequent tavg layer and stack it on
# to the previous one.
for (m in 2:12) {
  
  in.tavg <- raster(paste0(data.path, 
                           "wc2.0_10m_tavg/", 
                           TAVG.files[m]))
  
  tavg.stack <- stack(tavg.stack, in.tavg)

  print(m)
}

# now look at the data stack
tavg.stack

# now we can do spatial and non-spatial calculations
# we can calculate the mean just by using the normal 'mean' command... this 
# doesn't work for fancier functions (like standard deviation)
annual.mean.temp <- mean(tavg.stack, na.rm = TRUE)

# take a look at the map
plot(annual.mean.temp)

# we can also calculate summary stats for each layer
monthly.means <- cellStats(tavg.stack, "mean", na.rm = TRUE)

monthly.means

# and calculate the global mean
global.mean <- mean(monthly.means)

# but you can also do 'raster math' to look at more interesting things

# like calculate the range of temperatures at each pixel
temp.range <- max(tavg.stack, na.rm = TRUE) - min(tavg.stack, na.rm = TRUE)

plot(temp.range)

# and THE BEST PART ABOUT R (in Kyla's opinion) is that you can write any
# function you can think of, and apply it to a raster stack using the 'calc'
# function!!

# easy one = calculate the standard deviation of average temperture at each
# pixel using the standard 'sd' function (this is really slow, so may skip
# this if running short on time - surprise! it calculates the sd)
temp.sd <- calc(tavg.stack, fun = sd, na.rm = TRUE)

plot(temp.sd)


# now to try this out we're going to write a function to find around which
# months the biggest change in temperature occurs from the next month, then 
# apply that to the whole timeseries

# first we write a function to do what we want (note: hardest part of this is
# figuring out where it may go wrong. this is often in exceptional cases like 
# a string of all NA values)

biggest.change <- function(x, na.rm = TRUE) {
  # how long is the vector?
  n <- length(x)
  
  # check if all the values in the vector are NA, return NA, do not try other
  # calculations! *else* proceed to figure out where the biggest change is
  if (sum(is.na(x)) == n ) {return(NA)} else {
  
    # let's make a new vector of 1 month later
    x.plus.1 <- c(x[-1], x[1])
    
    # calculate the absolute difference between each month's value and the next
    abs.diff <- abs(x - x.plus.1)
    
    # calculate the maximum of those values
    max.diff <- max(abs.diff, na.rm = TRUE)
    
    # figure out where that value falls in the list of months
    big.change <- which(abs.diff == max.diff)
    
    # return the first of those (could have 2 or more with same difference)
    return(big.change[1])
  }
}

# now to see if it works, try it on a test vector
test <- c(2,3,1,4,5,2,8,11,NA,9,1,7)

biggest.change(test)

# now try on a string of NAs to see what happens (trouble shooting!)
test.na <- c(NA, NA, NA, NA, NA)

# if this runs and returns an NA without an error, we win!
biggest.change(test.na)

# if it worked, proceed to try on the whole dataset!
big.change.month <- calc(tavg.stack, fun = biggest.change, na.rm = TRUE)

# and take a look
plot(big.change.month)

# to see just a single month, you can just plot it (like this is all the places
# where big change month is July)
plot(big.change.month == 7)

# the power of R is being able to write ANY FUNCTION YOU WANT and then be able
# to do it on a grid. this is do-able in other programs, but not as simple.



