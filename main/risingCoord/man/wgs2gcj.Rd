% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wgs2gcj.R
\name{wgs2gcj}
\alias{wgs2gcj}
\title{wgs2gcj}
\usage{
wgs2gcj(lng, lat)
}
\arguments{
\item{lng}{WGS-84 longitude}

\item{lat}{WGS-84 latitude}
}
\value{
location
}
\description{
WGS-84 to GCJ-02
}
\details{
BD-09 formulated by the Chinese State Bureau of Mapping GIS coordinate system(Uesd by BaiduMap)
GCJ-02 formulated by the Chinese State Bureau of Mapping GIS coordinate system(Uesd byAmap/QQmap/Google map)
WGS-84 is an internationally adopted geocentric coordinate system
Referencing wandergis/coordTransform_py <\url{https://github.com/wandergis/coordTransform_py}>
}
\examples{
 location <- wgs2gcj(lng=121.89080, lat = 31.9479)
}
\author{
t.s.helianthus
}
