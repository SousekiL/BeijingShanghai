##' gcj2bd
##'
##' GCJ-02 to BD-09
##'
##' BD-09 formulated by the Chinese State Bureau of Mapping GIS coordinate system(Uesd by BaiduMap)
##' GCJ-02 formulated by the Chinese State Bureau of Mapping GIS coordinate system(Uesd byAmap/QQmap/Google map)
##' WGS-84 is an internationally adopted geocentric coordinate system
##' Referencing wandergis/coordTransform_py <\url{https://github.com/wandergis/coordTransform_py}>
##'
##' @param lng GCJ-02 longitude
##' @param lat GCJ-02 latitude
##' @return location
##' @author t.s.helianthus
##' @examples  location <- gcj2bd(lng=121.89080, lat = 31.9479)
##' @export


gcj2bd <- function(lng, lat) {
    lng <- as.numeric(lng)
    lat <- as.numeric(lat)
    lng[is.na(lng)] <- 0
    lat[is.na(lat)] <- 0

    z <- sqrt(lng * lng + lat * lat) + 2e-05 * sin(lat * x_pi)
    theta <- atan2(lat, lng) + 3e-06 * cos(lng * x_pi)
    bd_lng <- z * cos(theta) + 0.0065
    bd_lat <- z * sin(theta) + 0.006

    bd_lng[is.na(lng)] <- lng[is.na(lng)]
    bd_lat[is.na(lat)] <- lat[is.na(lat)]

    re_coord <- not_china(lng, lat) & (!is.na(lng) | !is.na(lat))
    bd_lng[re_coord] <- lng[re_coord]
    bd_lat[re_coord] <- lat[re_coord]

    location <- list(trans_lng = bd_lng, trans_lat = bd_lat)

    return(location)
}