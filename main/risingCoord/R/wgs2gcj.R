##' wgs2gcj
##'
##' WGS-84 to GCJ-02
##'
##' BD-09 formulated by the Chinese State Bureau of Mapping GIS coordinate system(Uesd by BaiduMap)
##' GCJ-02 formulated by the Chinese State Bureau of Mapping GIS coordinate system(Uesd byAmap/QQmap/Google map)
##' WGS-84 is an internationally adopted geocentric coordinate system
##' Referencing wandergis/coordTransform_py <\url{https://github.com/wandergis/coordTransform_py}>
##'
##' @param lng WGS-84 longitude
##' @param lat WGS-84 latitude
##' @return location
##' @author t.s.helianthus
##' @examples  location <- wgs2gcj(lng=121.89080, lat = 31.9479)
##' @export


wgs2gcj <- function(lng, lat) {
    ee <- 0.00669342162296594 # eccentricity squared
    a <- 6378245 # semi-major axis
    lng <- as.numeric(lng)
    lat <- as.numeric(lat)
    lng[is.na(lng)] <- 0
    lat[is.na(lat)] <- 0

    dlat <- transformlat(lng = lng - 105, lat = lat - 35)
    dlng <- transformlng(lng = lng - 105, lat = lat - 35)
    radlat <- lat / 180 * pi
    magic <- sin(radlat)
    magic <- 1 - ee * magic * magic
    sqrtmagic <- sqrt(magic)
    dlat <- (dlat * 180) / ((a * (1 - ee)) / (magic * sqrtmagic) * pi)
    dlng <- (dlng * 180) / (a / sqrtmagic * cos(radlat) * pi)
    mglat <- lat + dlat
    mglng <- lng + dlng

    location <- list(trans_lng = mglng, trans_lat = mglat)
    return(location)
}