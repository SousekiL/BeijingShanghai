##' transformlng
##' 
##' process of GCJ-02 exchange to WGS-84 
##' 
##' Referencing wandergis/coordTransform_py <\url{https://github.com/wandergis/coordTransform_py}> 
##' 
##' @param lng query longitude
##' @param lat query latitude
##' @return ret
##' @author t.s.helianthus
##' @examples  ret <- transformlng(lng=121.89080, lat = 31.9479)
##' @export


transformlng <- function(lng, lat) {
    ret <- 300 + lng + 2 * lat + 0.1 * lng * lng + 0.1 * lng * lat + 0.1 * sqrt(abs(lng))
    ret <- ret + (20 * sin(6 * lng * pi) + 20 * sin(2 * lng * pi)) * 2/3
    ret <- ret + (20 * sin(lng * pi) + 40 * sin(lng/3 * pi)) * 2/3
    ret <- ret + (150 * sin(lng/12 * pi) + 300 * sin(lng/30 * pi)) * 2/3
    return(ret)
}
