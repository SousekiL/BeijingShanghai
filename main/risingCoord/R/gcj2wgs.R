##' gcj2wgs
##'
##' GCJ-02 to WGS-48
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
##' @examples  location <- gcj2wgs(lng=121.89080, lat = 31.9479)
##' lngs <- c(121.89080, 0, "", 119.28383)
##' lats <- c(31.9479, 0, "", 26.293484)
##' location <- gcj2wgs(lng=lngs, lat = lats)
##' @export
# define ellipsoid
a = 6378245.0
f = 1 / 298.3
b = a * (1 - f)
ee = 1 - (b * b) / (a * a)

gcj2wgs <- function(lng, lat) {
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

    trans_lng <- lng * 2 - mglng
    trans_lat <- lat * 2 - mglat

    re_coord <- not_china(lng, lat) & (!is.na(lng) | !is.na(lat))
    trans_lng[re_coord] <- lng[re_coord]
    trans_lat[re_coord] <- lat[re_coord]

    trans_lng[is.na(lng)] <- lng[is.na(lng)]
    trans_lat[is.na(lat)] <- lat[is.na(lat)]

    location <- list(trans_lng = trans_lng, trans_lat = trans_lat)
    return(location)
}
