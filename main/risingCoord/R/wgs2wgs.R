##' wgs2wgs
##' 
##' WGS to WGS
##' 
##' Has no practical effect, in order to batch similar transactions(multi2wgs)
##' BD-09 formulated by the Chinese State Bureau of Mapping GIS coordinate system(Uesd by BaiduMap)
##' GCJ-02 formulated by the Chinese State Bureau of Mapping GIS coordinate system(Uesd byAmap/QQmap/Google map)
##' WGS-84 is an internationally adopted geocentric coordinate system
##' Referencing wandergis/coordTransform_py <\url{https://github.com/wandergis/coordTransform_py}> 
##' 
##' @param lng WGS longitude
##' @param lat WGS latitude
##' @return location
##' @author t.s.helianthus
##' @examples  location <- wgs2wgs(lng=121.89080, lat = 31.9479)
##' @export

wgs2wgs <- function(lng, lat) {
  location <- list(trans_lng = lng, trans_lat = lat)
  return(location)
}