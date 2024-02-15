##' transformlat
##' 
##' process of GCJ-02 exchange to WGS-84 
##' 
##' Referencing wandergis/coordTransform_py <\url{https://github.com/wandergis/coordTransform_py}> 
##' 
##' @param lng query longitude
##' @param lat query latitude
##' @return ret
##' @author t.s.helianthus
##' @examples  ret <- transformlat(lng=121.89080, lat = 31.9479)
##' @export


transformlat <- function(lng, lat) {
    ret <- -100 + 2 * lng + 3 * lat + 0.2 * lat * lat + 0.1 * lng * lat + 0.2 * sqrt(abs(lng))
    ret <- ret + (20 * sin(6 * lng * pi) + 20 * sin(2 * lng * pi)) * 2/3
    ret <- ret + (20 * sin(lat * pi) + 40 * sin(lat/3 * pi)) * 2/3
    ret <- ret + (160 * sin(lat/12 * pi) + 320 * sin(lat * pi/30)) * 2/3
    return(ret)
}
