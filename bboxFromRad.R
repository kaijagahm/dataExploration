bboxFromRad <- function(lon, lat, kmDist){
  top <- geosphere::destPointRhumb(c(lon,lat),0,kmDist*1000)
  left <- geosphere::destPointRhumb(c(lon,lat),270,kmDist*1000)
  bottom <- geosphere::destPointRhumb(c(lon,lat),180,kmDist*1000)
  right <- geosphere::destPointRhumb(c(lon,lat),90,kmDist*1000)
  
  bottom_left <- c("lon" = left[1], "lat" = bottom[2])
  top_right <-c("lon" = right[1], "lat" = top[2])
  
  bbox <- list("bottom_left" = bottom_left,
               "top_right" = top_right,
               "bbox" = c(bottom_left, top_right),
               "xlims" = c(left[1], right[1]),
               "ylims" = c(bottom[2], top[2]))
  return(bbox)
}
