#functions to write kml files. 

.kml_header <- function(docname="kml document") {
  sprintf('<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
    <Document>
        <name>%s</name>', docname)
}

.kml_footer <- function() {
  return('
    </Document>
</kml>')
}

#fully vectorized due to sprintf being vectorized
.kml_placemark <- function(lon, lat, alt=0, name="placemark", description="") {
  sprintf('
        <Placemark>
  		    <name>%s</name>
          <description><![CDATA[%s]]></description>
          <Point>
            <coordinates>%s,%s,%s</coordinates>
          </Point>
        </Placemark>', name, description, lon, lat, alt)
}

#' Write Simple KML File With Placemarks
#' 
#' Writes a simple KML File with placemarks that have names, locations, and descriptions.
#'
#' @param lons The longitude(s)
#' @param lats The latitude(s)
#' @param filename The filename to write
#' @param docname The title of the document
#' @param names The name(s) for each placemark
#' @param descriptions The description(s) for each placemark
#'
#' @export
#'
#' @examples
#' locs <- c("wolfville, ns", "halifax, ns", "yarmouth, ns", "sydney, ns", "amherst, ns")
#' lons <- c(-64.35984, -63.57532, -66.11738, -60.19422, -64.21672)
#' lats <- c(45.09176, 44.64886, 43.83746, 46.13679, 45.81667)
#' 
#' write_kml(lons, lats, filename="mykml.kml", names=locs)
#' 
#' 
write_kml <- function(lons, lats, filename, docname="kml_document", names="placemark", descriptions="") {
  write(paste(.kml_header(docname), 
              paste(.kml_placemark(lons, lats, name=names, description = descriptions), collapse="\n"), 
              .kml_footer(), collapse="\n"), file=filename)
}
