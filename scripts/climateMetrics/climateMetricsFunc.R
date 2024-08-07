#' Creating multiple climate-related metrics, using current and future mean temperatures

#' @name climate_metrics
#' @param cube_current, a GDAL data cube proxy object for current temperature
#' @param cube_future, a GDAL data cube proxy object for  future temperature
#' @param t_match, float, plus/minus threshold to define climate match, by default 0.50
#' @param metric, a character vector indicating climate metrics.  Options include: "local", "forward", "backward", "rarity"
#' @param movingWindow, float indicating number of cells (must be an odd number) to search for similar climates
#' @return a raster stack
#' @import gdalcubes rstac tibble sp sf dplyr rgbif tidyr stars


# Local climate-change velocity
  climate_metrics <- function(cube_current,
                              cube_future,
                              t_match = 0.25,
                              metric = "local",
                              moving_window = 11
                              ){
  
  # Calculate mean current temperature
    sum_bands <-  paste(names(cube_current), collapse="+")
    
    mean_bands <- sprintf("(%s)/%i", sum_bands, length(names(cube_current)))
   
    cube_bands <-  gdalcubes::select_bands(cube_current, bands =  names(cube_current)) # select bands
    
    tmean_cube <- gdalcubes::apply_pixel(cube_bands, mean_bands, names = "mean_tmean") # apply to each pixel the next function
    
    tmean_cube_r <- stacatalogue::cube_to_raster(tmean_cube, format = "raster")
    
    tmean_current_C <- (tmean_cube_r/10) - 273.15
  
  # Calculate mean future temperature
    raster_future <- stacatalogue::cube_to_raster(cube_future, format = "raster")
    tmean_future_C <- (raster::calc(raster_future, mean)/10) - 273.15
    
    
  # Time span
    years_dif <- as.numeric(substr(gdalcubes::dimensions(cube_future)[[1]][2], 1,4)) - 
      as.numeric(substr(gdalcubes::dimensions(tmean_cube)[[1]][2], 1,4))
    
   srs_cube <- raster::crs(raster_future) 
    
  if(metric == "local"){
  
  # Spatial gradient (meters/?C)
    
    # Neighborhood Slope Algorithm, average maximum technique
      f <- matrix(1, nrow=3, ncol=3)
      x <- tmean_current_C$mean_tmean
      spatial_tmean_current <- raster::focal(x, w=f, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8, pad=TRUE, padValue=NA)%>%`/`(raster::res(x)[1]/raster::res(x)[1])
      
    # Truncating zero values
      spatial_tmean_current_0 <- spatial_tmean_current
      spatial_tmean_current_0[spatial_tmean_current_0 <= 0.00001] <- 0.00001
      
  # Temporal gradient (?C/year)
    Temporal_tmean <- (tmean_future_C - tmean_current_C)/years_dif
    
  
  # Local climate-change velocity (meters/year)
    local_velocity <- Temporal_tmean/spatial_tmean_current_0
      names(local_velocity) <- "local_climate_velocity"
      
    return(local_velocity)
  }
  
 
  
  if(metric == "forward"){
    
    # t_match                                                         # plus/minus threshold to define climate match
    t_match <- 1/(t_match*2)                                          # inverse for rounding, double for plus/minus
    
    x <-  raster::coordinates(tmean_current_C)[,1]                    # vector of grid cell x coordinates
    y <-  raster::coordinates(tmean_current_C)[,2]                    # vector of grid cell y coordinates
    
    p <- round(raster::getValues(tmean_current_C)*t_match)/t_match    # vector of present climate values for xy coordinates
    
    f <- round(raster::getValues(tmean_future_C)*t_match)/t_match     # vector of future climate values for xy coordinates 
    d <- vector(length=length(p))                                     # empty vector to write distance to climate match
    
    u     <- unique(p)[order(unique(p))]                              # list of unique climate values in p

    match <- function(u){c(which(u==f))}                              # function finding climate matches of u with f
    
    m     <- sapply(u, match)                                         # list of climate matches for unique values
    
    
      for (i in 1:length(p)) {                                        # loop for all grid cells of p
        mi   <- m[[which(u==p[i])]]                                   # recalls list of climate matches for p[i]
        d[i] <- sqrt(min((x[i]-x[mi])^2 + (y[i]-y[mi])^2))            # distance to closest match
        
      }
    
    
    # Create matrix with coordinates x and y and distance values
      d[d==Inf] <- 10000000                                           # sets no analogue to 10,000km
      out=cbind(x,y, distance=d) 
    
    # forward_velocity
      forward_velocity <- raster::rasterFromXYZ(out, res=raster::res(tmean_current_C)[1], crs = srs_cube)%>%
        raster::setExtent(tmean_current_C)%>%`/`(1000)%>%
        raster::reclassify(c(raster::res(tmean_current_C)[1], raster::res(tmean_current_C)[1], NA), right=NA)%>%`/`(years_dif)%>%
        raster::reclassify(c(NA, NA, raster::res(tmean_current_C)[1]), right=raster::res(tmean_current_C)[1])
      names(forward_velocity) <- "forward_climate_velocity"
      
    return(forward_velocity)
  
  }
  
   
  if(metric == "backward"){
    
   # Let's swap `p` and `f` (i.e., tmean_current_C and tmean_future_C)

    # t_match                                                         # plus/minus threshold to define climate match
    t_match <- 1/(t_match*2)                                          # inverse for rounding, double for plus/minus
    
    x <-  raster::coordinates(tmean_current_C)[,1]                    # vector of grid cell x coordinates
    y <-  raster::coordinates(tmean_current_C)[,2]                    # vector of grid cell y coordinates
    
    p <- round(raster::getValues(tmean_future_C)*t_match)/t_match     # vector of present climate values for xy coordinates
    
    f <- round(raster::getValues(tmean_current_C)*t_match)/t_match    # vector of future climate values for xy coordinates 
    d <- vector(length=length(p))                                     # empty vector to write distance to climate match
    
    u     <- unique(p)[order(unique(p))]                              # list of unique climate values in p
    
    match <- function(u){c(which(u==f))}                              # function finding climate matches of u with f
    
    m     <- sapply(u, match)                                         # list of climate matches for unique values
    
   
      for (i in 1:length(p)) {                                        # loop for all grid cells of p
        mi   <- m[[which(u==p[i])]]                                   # recalls list of climate matches for p[i]
        d[i] <- sqrt(min((x[i]-x[mi])^2 + (y[i]-y[mi])^2))            # distance to closest match
        
      }
   
      
      # Create matrix with coordinates x and y and distance values
      d[d==Inf] <- 10000000                                           # sets no analogue to 10,000km
      out=cbind(x,y, distance=d) 
      
      # forward_velocity
      backward_velocity <- raster::rasterFromXYZ(out, res=raster::res(tmean_current_C)[1], crs = srs_cube)%>%
        raster::setExtent(tmean_current_C)%>%`/`(1000)%>%
        raster::reclassify(c(raster::res(tmean_current_C)[1], raster::res(tmean_current_C)[1], NA), right=NA)%>%`/`(years_dif)%>%
        raster::reclassify(c(NA, NA, raster::res(tmean_current_C)[1]), right=raster::res(tmean_current_C)[1])
      names(backward_velocity) <- "backward_climate_velocity"
      
      return(backward_velocity)
      
    }
    
    
    
  if(metric == "rarity"){
    
    tmean_current_rarity <- raster::focal(tmean_current_C$mean_tmean,
                                    w=matrix(1, moving_window, moving_window),
                                    na.rm=TRUE,
                                    fun=function(x, focalW = moving_window, tol=t_match, ...) {
                                             focalCell <- ceiling((focalW)*(focalW)/2)
                                             sum((x[focalCell]-x) <= tol, ...)/sum(!is.na(x))
                                    }
    )
    names(tmean_current_rarity) <- "climate_current_rarity"
                                    
    tmean_future_rarity <- raster::focal(tmean_future_C,
                                          w=matrix(1, moving_window, moving_window),
                                          na.rm=TRUE,
                                          fun=function(x, focalW = moving_window, tol=t_match, ...) {
                                            focalCell <- ceiling((focalW)*(focalW)/2)
                                            sum((x[focalCell]-x) <= tol, ...)/sum(!is.na(x))
                                          }             
                                    
    )
    names(tmean_future_rarity) <- "climate_future_rarity"
    
    climate_rarity <- raster::stack(tmean_current_rarity, tmean_future_rarity)
    
    
    return(climate_rarity)
    
    
  }
   
   
  }
  
  
 
  
