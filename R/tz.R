.s <- new.env(emptyenv())

.load <- function() {
    if (is.null(.s$shp)) .s$shp <- fastshp::read.shp(system.file("combined_shapefile.shp", package="tzinfo"), "poly")
    if (is.null(.s$names)) .s$names <- as.character(foreign::read.dbf(system.file("combined_shapefile.dbf", package="tzinfo"))[[1]])
}

location2offset <- function(lat, lon, base="2017-01-01") {
    .load()
    i <- rcgal::inside(.s$shp, lon, lat)
    ui <- na.omit(unique(i))
    utz <- .s$names[ui]
    if (!is.character(base)) base <- as.character(base)
    utzo <- as.vector(sapply(utz, function(o) as.numeric(as.POSIXct(base, "GMT")) - as.numeric(as.POSIXct(base, o))))
    utzo[match(i, ui)]
}

location2tz <- function(lat, lon) {
    .load()
    i <- rcgal::inside(.s$shp, lon, lat)
    .s$names[i]
}

tzoffset <- function(tzname, base="2017-01-01") {
    if (!is.character(base)) base <- as.character(base)
    utz <- na.omit(unique(tzname))
    utzo <- as.vector(sapply(utz, function(o) as.numeric(as.POSIXct(base, "GMT")) - as.numeric(as.POSIXct(base, o))))
    utzo[match(tzname, utz)]
}
