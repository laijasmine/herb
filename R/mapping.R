#' Mapping
#'
#' @param ds dataframe - a herbarium spreadsheet
#' @param DMS logical - whether or not the coordinates are in degree minutes and seconds
#'
#' @return Web Map
#' @export
#'
#' @examples
mapping <- function(ds, DMS = FALSE) {

  if (DMS) {
    ds_coord <- dplyr::mutate(ds,
                              Lat_d = ds$VLatDegree,
                              Long_d = ds$VLongDegree)
  }
  else{ds_coord <- dplyr::mutate(ds,
                                 Lat_d = paste(ds$VLatDegree, ds$VLatMinute, ds$VLatSecond),
                                 Long_d = paste(ds$VLongDegree, ds$VLongMinute, ds$VLongSecond))

  # convert from decimal minutes to decimal degrees
  ds_coord$Lat_d = measurements::conv_unit(ds_coord$Lat_d,
                                           from = 'deg_min_sec',
                                           to = 'dec_deg')
  ds_coord$Long_d = measurements::conv_unit(ds_coord$Long_d,
                                            from = 'deg_min_sec',
                                            to = 'dec_deg')}

  map <- leaflet::leaflet()

  map_tile <- leaflet::addProviderTiles(map, "OpenStreetMap.Mapnik")

  map_marker <- leaflet::addCircleMarkers(map_tile, lng = as.numeric(ds_coord$Long_d), lat = as.numeric(ds_coord$Lat_d),
                              popup = paste(ds_coord$Genus, ds_coord$Species,
                                            ", Location:", ds_coord$Location,
                                            ", Collect no.:",ds_coord$`Collector Number`,
                                            ", Lat:",ds_coord$Lat_d,
                                            ", Long:", ds_coord$Long_d),
                              clusterOptions = leaflet::markerClusterOptions())

  map_button <- leaflet::addEasyButton(map_marker, leaflet::easyButton(
    icon = "fa-crosshairs", title = "Locate Me",
    onClick = leaflet::JS("function(btn, map){ map.setZoom(2);}")))
  return(map_button)
}
