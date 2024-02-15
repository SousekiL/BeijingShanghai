##' bd2wgs
##'
##' BD-09 to WGS-48
##'
##' BD-09 formulated by the Chinese State Bureau of Mapping GIS coordinate system(Uesd by BaiduMap)
##' GCJ-02 formulated by the Chinese State Bureau of Mapping GIS coordinate system(Uesd byAmap/QQmap/Google map)
##' WGS-84 is an internationally adopted geocentric coordinate system
##' Referencing wandergis/coordTransform_py <\url{https://github.com/wandergis/coordTransform_py}>
##'
##' @param lng BD-09 longitude
##' @param lat BD-09 latitude
##' @return location
##' @author t.s.helianthus
##' @examples  location <- bd2wgs(lng=121.89080, lat = 31.9479)
##' @export


bd2wgs <- function(lng, lat) {
    lng <- as.numeric(lng)
    lat <- as.numeric(lat)
    lng[is.na(lng)] <- 0
    lat[is.na(lat)] <- 0
    gcj_location <- bd2gcj(lng = lng, lat = lat)
    location <- gcj2wgs(lng = gcj_location$trans_lng, lat = gcj_location$trans_lat)
    return(location)
}