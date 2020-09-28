mat2ras=function(mat, Longitude, Latitude, plot_ind){

row.names(mat)=Longitude
colnames(mat)=Latitude
xyz=melt(mat)
ncol <-length(Longitude)
nrow <- length(Latitude)

colnames(xyz)= c("x", "y", "z")

dat <- raster(
  xmn = 1, xmx = ncol,
  ymn = 1, ymx = nrow,
  nrows = nrow, 
  ncols = ncol,
  vals = xyz$z)

library(maps)
# specify CRS to be used
crs.latlon <- CRS("+proj=longlat +datum=WGS84")
nrow <- dim(dat)[1]
ncol <- dim(dat)[2]

# get extent of the domain
ext <- extent(xyz[, c('x', 'y')])

# create a raster for extent with nrow and ncol
rast <- raster(
  ext, nrow = nrow, ncol = ncol,
  crs = crs.latlon)

# rasterize the data values
rast <- rasterize(
  xyz[, c('x', 'y')], rast,
  xyz[, 'z'], fun=mean)

# source("C:/Users/vinit/Documents/R/fill-raster-NA.R")
# rast.no.na <- fill.raster.NA(rast)
if (plot_ind==TRUE){
  plot(rast)
}
return(rast)
}
