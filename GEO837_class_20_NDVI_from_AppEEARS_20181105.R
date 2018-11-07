#### GEO 837 - Remote Sensing of the Biosphere ####
#### Author: Kyla M. Dahlin, MSU Geography ########
#### Class 20 - reading in and analyzing NDVI #####
# credit: some of this is loosely based on a workflow here:
# https://lpdaac.usgs.gov/sites/default/files/public/elearning/AppEEARS_NC_QualityFiltering_R.html

library(raster)
library(ncdf4) # need to install this if haven't already
library(RColorBrewer) # this one too

# set your working directory
setwd("YOUR WORKING DIRECTORY")

# what day is it? 
today <- format(Sys.Date(), "%Y%m%d")

# looking at MODIS NDVI data downloaded from AppEEARS on 11/5/2018
data.path <- "YOUR APPEEARS DATA PATH"

# read in the nc file (which contains all the NDVI layers and pixel quality)
in.data <- nc_open(paste0(data.path, "MOD13Q1.006_250m_aid0001.nc"))

# take a look
in.data

# whoa! lots of info. let's get some details
# what variables are in this data?
attributes(in.data$var)$names

# what TYPES of dimensions are there?
attributes(in.data$dim)$names

# let's read in some metadata for the NDVI data
v6.info <- ncatt_get(in.data, "_250m_16_days_NDVI")

# take a look
v6.info

# now let's actually read in the NDVI data
NDVI.nc <- ncvar_get(in.data, "_250m_16_days_NDVI")

# and the quality info
QA.nc <- ncvar_get(in.data, "_250m_16_days_VI_Quality")

# these are NOT rasters yet! what do they look like?
dim(NDVI.nc)

# what are they?
class(NDVI.nc)

# a 3D matrix! with 345 layers (yikes!) and no attached spatial or temporal info

# so let's get the lat and lon
lat <- ncvar_get(in.data, "lat")

# take a look
lat

# get the longitude, too
lon <- ncvar_get(in.data, "lon")

# let's also get the projection info
crs.nc <- ncatt_get(in.data, "crs")

# take a look
crs.nc

# extract the epsg code from this stuff
epsg.nc <- crs.nc$epsg_code

# turn this epsg code into a crs object
coord.system <- crs(paste0("+init=epsg:", epsg.nc))

# take a look
coord.system

# now let's take a look at the time variable
time <- in.data$dim$time

# take a look
time

# yikes! data is stored as Julian days since 2000-01-01 00:00:00. ok.
samples <- length(time$vals)

# since we know we have 15 years of data, how many samples do we have per year?
per.year <- samples/15

# let's make a vector of years so we have a year for each layer of our data
# we'll use this later to get single values for each pixel for each year
years <- rep(2001:2015, each = per.year)

# let's also get rid of those pesky fill values (faster to do this on a matrix
# than on a raster stack)
fill.value <- ncatt_get(in.data, "_250m_16_days_NDVI", "_FillValue")

# take a look
fill.value

# now let's replace all those fill values with NAs
NDVI.nc[NDVI.nc == fill.value$value] <- NA

# now let's rasterize the first layer, just to take a look
ndvi.1 <- raster(t(NDVI.nc[,,1]), xmn = min(lon), xmx = max(lon),
                 ymn=min(lat), ymx = max(lat), crs = coord.system)

plot(ndvi.1)

# and the same for the QA band, just to see
QA.1 <- raster(t(QA.nc[,,1]), xmn = min(lon), xmx = max(lon),
                 ymn=min(lat), ymx = max(lat), crs = coord.system)
plot(QA.1)

#################### MODIS QA/QC FILTERING #####################################
# note: this is an important step but in the interest of time we are going to
# skip it. If you plan on using MODIS data in your actual research, make sure
# to read the documentation and take a look at the link at the top of this code,
# which has a thorough treatment of MODIS VI QA codes.
###############################################################################

##### Now to make a raster stack of max ndvi per pixel per year ###############
# note: you could start by reading in the whole thing as a raster stack, but
# that crashed my computer :\

# per usual, we'll start by doing this on the first year, then loop in the 
# subsequent years

# first get a list of the #s corresponding to the layers that are in 2001
year.sub.list <- which(years == 2001)

# extract out just those layers
year.subset <- NDVI.nc[,,year.sub.list]

# check that this seems right
dim(year.subset)

# now turn that subset into a raster stack
year.raster <- brick(year.subset, xmn = min(lon), xmx = max(lon),
                     ymn=min(lat), ymx = max(lat), crs = coord.system,
                     transpose = TRUE)

# now calculate the maximum value for each pixel
max.per.year <- max(year.raster, na.rm = TRUE)

# take a look
plot(max.per.year)

# Yay! Now let's do the rest of the years in a for-loop
for (y in 2002:2015) {
  # first get a list of the #s corresponding to the layers that are in 2001
  year.sub.list <- which(years == y)
  
  # extract out just those layers
  year.subset <- NDVI.nc[,,year.sub.list]
  
  # now turn that subset into a raster stack
  year.raster <- brick(year.subset, xmn = min(lon), xmx = max(lon),
                       ymn=min(lat), ymx = max(lat), crs = coord.system,
                       transpose = TRUE)
  
  # now calculate the maximum value for each pixel
  max.this.year <- max(year.raster, na.rm = TRUE)
  
  # and stack on to the one you generated above
  max.per.year <- stack(max.per.year, max.this.year)
  
  print(y)
}

# if we wanted to stay organized we could make sure these have sensible names
names(max.per.year) <- paste0("max.ndvi.", 2001:2015)

# take a look
plot(max.per.year)

############### NOW FOR SOME SCIENCE! #########################################
# we want to know which pixels have a significant positive trend, which have a 
# signifiant negative trend, and which have no trend, and the actual slopes. so 
# we need to write some functions

x <- c(1,2,3,1,2,3,1,5,3,1,2)

slope.fun <- function(x, na.rm = TRUE) {
  if (sum(is.na(x)) == length(x)) {return(NA)} else {
    timesteps <- seq(from = 1, to = length(x), by = 1)
    model <- lm(x ~ timesteps)
    model.sum <- summary(model)
    slope.out <- model.sum$coefficients[2,1]
    return(slope.out)
  }
}

pval.fun <- function(x, na.rm = TRUE) {
  if (sum(is.na(x)) == length(x)) {return(NA)} else {
    timesteps <- seq(from = 1, to = length(x), by = 1)
    model <- lm(x ~ timesteps)
    model.sum <- summary(model)
    pval.out <- model.sum$coefficients[2,4]
    return(pval.out)
  }
}

# now try running these on the raster stack (took about 9 min on my computer)
date()
slope.ndvi <- calc(max.per.year, fun = slope.fun, na.rm = TRUE)
pval.ndvi <- calc(max.per.year, fun = pval.fun, na.rm = TRUE)
date()

# now turn the pvalues into a mask to throw them out of the analysis
pval.mask <- pval.ndvi

# set the nonsignificant values to NA
pval.mask[pval.mask > 0.05] <- NA

# set the rest of the values to 1
pval.mask <- pval.mask > 0

# check that this looks right
plot(pval.mask)

# mask out those higher p-values
sig.slopes <- slope.ndvi * pval.mask

# take a look! but make it pretty
color.palette <- brewer.pal(11, "PuOr")
x11()
plot(sig.slopes, col = color.palette)

# now write to a KML so we can look in GoogleEarth!
KML(sig.slopes, 
    paste0("carrizo_ndvi_slope2_",today,".kml"),
    blur = 0,
    col = color.palette)

# and write as a raster if you want to make a pretty map in Arc or QGIS
writeRaster(sig.slopes,
            paste0("carrizo_ndvi_slope_",today,".tif"))


