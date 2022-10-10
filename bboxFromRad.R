bboxFromRad <- function(lon, lat, kmDist){
  top <- geosphere::destPointRhumb(c(lon,lat),0,kmDist*1000)
  left <- geosphere::destPointRhumb(c(lon,lat),270,kmDist*1000)
  bottom <- geosphere::destPointRhumb(c(lon,lat),180,kmDist*1000)
  right <- geosphere::destPointRhumb(c(lon,lat),90,kmDist*1000)
  
  bottom_left <- c(left[1],bottom[2])
  top_right <-c(right[1],top[2])
  
  bbox <- c(bottom_left, top_right)
  return(bbox)
}