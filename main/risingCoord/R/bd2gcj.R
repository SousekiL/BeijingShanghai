##' bd2gcj
##'
##' BD-09 to GCJ-02
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
##' @examples  location <- bd2gcj(lng=121.89080, lat = 31.9479)
##' @export


bd2gcj <- function(lng, lat) {
    lng <- as.numeric(lng)
    lat <- as.numeric(lat)
    lng[is.na(lng)] <- 0
    lat[is.na(lat)] <- 0

    x <- lng - 0.0065
    y <- lat - 0.006
    z <- sqrt(x * x + y * y) - 2e-04 * sin(y * x_pi)
    theta <- atan2(y, x) - 3e-06 * cos(x * x_pi)
    gcj_lng <- z * cos(theta)
    gcj_lat <- z * sin(theta)

    gcj_lng[is.na(lng)] <- lng[is.na(lng)]
    gcj_lat[is.na(lat)] <- lat[is.na(lat)]

    re_coord <- not_china(lng, lat) & (!is.na(lng) | !is.na(lat))

    gcj_lng[re_coord] <- lng[re_coord]
    gcj_lat[re_coord] <- lat[re_coord]

    location <- list(trans_lng = gcj_lng, trans_lat = gcj_lat)
    return(location)
}