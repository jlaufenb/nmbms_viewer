################################################################################
# Global functions used in NMBMS transect-viewer shiny app
# Jared Laufenberg <jared_laufenberg@fws.gov>
################################################################################



#----
## Aesthetics

# Color palatte used in the application
color_pal <- c("#3366CC", "#DC3912", "#FF9900", "#109618", "#990099", "#0099C6",
               "#DD4477", "#66AA00", "#B82E2E", "#316395", "#994499", "#22AA99",
               "#AAAA11", "#6633CC", "#E67300", "#8B0707", "#651067", "#329262",
               "#5574A6", "#3B3EAC")



#----
## Mapping functions


#' Leaflet map of Kodiak NWR NMBMS data
#'
#' @description Map distance sampling transect data. First and last
#' points.
#'
#' @author Jared Laufenberg <jared_laufenberg@@fws.gov>
#'
#' @param nmbms A dataframe of GPS collar fixes containing lat, lon, and animal_id, site and fix_time fields
#'
#' @return A leaflet map of fix locations, subsetted to one daily fix.
#' @export
#'
#' @examples collar_map(dat)
#'
nmbms_map <- function(nmbms) {
  df <- nmbms %>%
    filter(!(is.na(lon) | is.na(lat))) %>%
    arrange(id, datetime) #%>%
##    group_by(id, lubridate::as_date(datetime)) %>%
##    slice(1) %>%  # takes the first fix of the day for each collar
##    ungroup()

  ids <- unique(df$id)
  pal <- rep_len(color_pal, length(ids))

  map <- leaflet() %>%
    addProviderTiles("Esri.WorldTopoMap", options = providerTileOptions(attribution = NA)) %>%
    addMeasure(primaryLengthUnit="kilometers", secondaryLengthUnit="kilometers") %>%
    addScaleBar(position="bottomleft") %>%
    setView(lng = -153, lat = 57.5, zoom = 9)


  for (i in seq_along(ids)) {
    d <- df %>% filter(id == ids[i])
    dp <- d[c(1, nrow(d)), ]
    dp$endpoints <- c("START","STOP")
    map <- addPolylines(map, lng = d$lon, lat = d$lat,
                        weight = 1,
                        color = pal[i],
                        opacity = .4)
    map <- addCircleMarkers(map, lng = dp$lon, lat = dp$lat,
                            stroke = FALSE,
                            radius = 4,
                            color = c("green","red"),
                            fillOpacity = 1,
                            popup = paste(sep = "<br>",
                                          paste("<b>Position:<b>", dp$endpoints),
                                          paste("<b>Transect ID:<b>", dp$transect),
                                          paste("<b>Survey date:<b>", dp$date))
                            )
    o <- d %>% filter(spp == "MAMU")
    map <- addCircleMarkers(map, lng = o$lon, lat = o$lat,
                            radius = o$count,
                            color = "grey",
                            fillOpacity = 1,
                            popup = paste(sep = "<br>",
                                          paste("<b>Count:<b>", o$count),
                                          paste("<b>Distance:<b>", o$distance))
    )
  }
  return(map)
}





