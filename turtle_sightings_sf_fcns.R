library(sf)

load('RData/studyArea.RData')

getCoastLine <- function(filename, lon.limits, lat.limits){
  data1 <- read.table(filename, header = F, row.names = NULL, sep = ',')
  # fix longitudes to -180 to 180
  lon.limits[lon.limits > 180] <- lon.limits[lon.limits > 180] - 360
  out.df <- data1[data1[,1] >= min(lon.limits) &
                    data1[,1] <= max(lon.limits) &
                    data1[,2] >= min(lat.limits) &
                    data1[,2] <= max(lat.limits),]
  colnames(out.df) <- c('Longitude', 'Latitude')
  out.df$idx <- NA
  idx.rows <- 1:nrow(out.df)
  na.idx <- is.na(out.df$Longitude)  # T/F for NA
  dif.na.idx <- na.idx[2:length(na.idx)] - na.idx[1:(length(na.idx)-1)]
  idx.neg1 <- idx.rows[dif.na.idx == -1]  # beginning of segments
  idx.pos1 <- idx.rows[dif.na.idx == 1]   # end of segments
  
  for (k in 1:length(idx.neg1)) {
    out.df[idx.neg1[k]:idx.pos1[k], 'idx'] <- k
  }
  
  # change index to a factor variable
  out.df$idx <- as.factor(out.df$idx)
  out.df <- na.omit(out.df)
  # this splits all segments into separate dataframes
  out.list <- split(out.df, out.df$idx)
  
  for (k in 1:length(out.list)){
    line.1 <- out.list[[k]][1,]
    line.end <- out.list[[k]][nrow(out.list[[k]]),]
    if (line.1$Longitude != line.end$Longitude){
      tmp <- out.list[[k]]
      tmp <- rbind(c(max(tmp$Longitude), max(tmp$Latitude), k),
                   tmp, c(max(tmp$Longitude), max(tmp$Latitude), k))
      out.list[[k]] <- tmp
    }
  }
  return(out.list)
  
}

# function to convert lat/lon data frame into spatial data frame
latlon2sp <- function(in.df, center.UTM = center.UTM){
  in.df <- st_as_sf(in.df, coords = c("X", "Y"),
                     crs = "+proj=longlat +datum=WGS84")

  out.df <- st_transform(in.df, 
                         crs = "+proj=utm +zone=10 ellps=WGS84")
  out.df$newX <- (out.df$X - center.UTM$X)/1000
  out.df$newY <- (out.df$Y - center.UTM$Y)/1000
  return(out.df)
}

# convert the lat/lon into northing/easting
# the study area covers zones 10 and 11. An arbitrary center point
# was created here.
approx.center <- st_as_sf(data.frame(X=-119.967, Y=33.092),
                          coords = c("X", "Y"),
                          crs = "+proj=longlat +datum=WGS84")

center.UTM <- st_transform(approx.center,
                           crs = "+proj=utm +zone=10 ellps=WGS84")

