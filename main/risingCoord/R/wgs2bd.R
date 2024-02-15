##' wgs2bd
##'
##' WGS-84 to BD-09
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
##' @examples  location <- wgs2bd(lng=121.89080, lat = 31.9479)
##' @export


wgs2bd <- function(lng, lat) {
    lng <- as.numeric(lng)
    lat <- as.numeric(lat)
    lng[is.na(lng)] <- 0
    lat[is.na(lat)] <- 0
    gcj_location <- wgs2gcj(lng = lng, lat = lat)
    location <- gcj2bd(
        lng = gcj_location$trans_lng,
        lat = gcj_location$trans_lat
    )
    return(location)
}