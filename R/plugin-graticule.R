leafletGraticuleDependencies <- function() {
  list(
    htmltools::htmlDependency(
      "leaflet.Graticule",
      # "0.1.0",
      "0cc4e4d07149b2dc111f48628c1a2e742bc0d70d",
      system.file("htmlwidgets/bower/Leaflet.Graticule/src", package = "leaflet"),
      script = c("L.Graticule.js")
    ),
    htmltools::htmlDependency(
      "leaflet.Graticule-binding",
      "1.3.1",
      system.file("htmlwidgets/bindings", package = "leaflet"),
      script = c("graticule-binding.js")
    )
  )
}

#' Add a Graticule on the map
#' see \url{https://github.com/turban/Leaflet.Graticule}
#'
#' @param map a map widget object
#' @param interval The spacing in map units between horizontal and vertical lines.
#' @param sphere boolean. Default FALSE
#' @param style path options for the generated lines. See \url{http://leafletjs.com/reference-1.2.0.html#path-option}
#' @param layerId the layer id
#' @param group the name of the group this layer belongs to.
#' @param options the path options for the graticule layer
#' @examples
#' library(leaflet)
#'
#' leaf <- leaflet() %>%
#'   addTiles() %>%
#'   addGraticule()
#'
#' @export
addGraticule <- function(
  map,
  interval = 20,
  sphere = FALSE,
  style = list(color = "#333", weight = 1),
  layerId = NULL,
  group = NULL,
  options = pathOptions(pointerEvents = "none", clickable = FALSE) # Default unclickable
) {
  map$dependencies <- c(map$dependencies, leafletGraticuleDependencies())
  invokeMethod(
    map,
    getMapData(map),
    "addGraticule",
    interval,
    sphere,
    style,
    layerId,
    group,
    options
  )
}
