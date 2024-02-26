#' tibble friendly sf, ppp, and raster operations
#'
#' @description functions ot extract coordinates from a point geometry, convert a sf to ppp and create a raster
#'
#' @describeIn st_coordinates_tidy retrieve coordinates within the original sf/data.frame object. an alternative from [st_coordinates()](https://r-spatial.github.io/sf/reference/st_coordinates.html)
#'
#' @param sf_object sf_object input
#'
#' @return sf and data.frame object X and Y extracted from geometry column
#'
#' @import dplyr
#' @import tidyr
#'
#' @examples
#'
#' \dontrun{
#'
#' # st_coordinates_tidy --------------------------------------
#'
#' library(tidyverse)
#' library(sf)
#' library(epihelper)
#'
#' sites <- tibble(gpx_point = c("a","b"),
#'                 longitude = c(-80.144005, -80.109),
#'                 latitude = c(26.479005,26.83)) %>% print()
#'
#' sites_sf <- sites %>%
#'   st_as_sf(coords = c("longitude", "latitude"),
#'            remove = T,
#'            crs = 4326) %>% print()
#'
#' # current solution
#' sites_sf %>%
#'   st_coordinates()
#'
#' # our proposal
#' sites_sf %>%
#'   st_coordinates_tidy()
#'
#' # sf_to_ppp ------------------------------------------
#'
#' # data packages
#' library(tidyverse)
#' library(epihelper)
#' library(sf)
#'
#' # import sample data
#' library(spatstat)
#' data("flu")
#' flu_one <- flu$pattern$`wt M2-M1 13`
#' flu_one %>% plot()
#'
#' # reverse engineering
#'
#' # extract window from ppp
#' flu_one_window <- st_as_sf(flu_one) %>%
#'   filter(label=="window") %>%
#'   pull(geom)
#' # extract points from ppp
#' flu_one_points <- flu_one %>%
#'   as_tibble() %>%
#'   #tibble to sf
#'   st_as_sf(coords = c("y", "x"),
#'            remove = T,
#'            crs = 4326,
#'            agr = "constant")
#'
#' # /deprecated/ re-create a ppp from points and bbox
#' # sf_as_ppp(sf_geometry_input = flu_one_points,
#' #           sf_polygon_boundary = flu_one_window) %>%
#' #   plot()
#'
#' # /this works/ re-create a ppp from points and bbox
#' as.ppp(st_coordinates(flu_one_points),
#'        st_bbox(flu_one_window)) %>%
#'   plot()
#'
#' # tibble_as_raster -------------------------------------
#'
#' set.seed(33)
#'
#' expand_grid(x=1:10,
#'        y=1:10) %>%
#'   mutate(z=rnorm(100)) %>%
#'
#'   # convert tibble to raster
#'   tibble_as_raster() %>%
#'
#'   # plot to verify
#'   plot()
#'
#'   }
#'
#' @export st_coordinates_tidy
#' @export tibble_as_raster

st_coordinates_tidy <- function(sf_object) {

  sf_object %>%
    rownames_to_column() %>%
    left_join(
      sf_object %>%
        sf::st_coordinates() %>%
        as_tibble() %>%
        rownames_to_column()
    ) %>%
    select(-rowname)
}

#' @describeIn st_coordinates_tidy transform a x,y,z tibble to a raster
#' @param data tibble with three column names: x, y, z (for longitud, latitud, and grid value)

tibble_as_raster <- function(data) {

  gam_humber_viz_raster_id <-
    data %>%
    as_tibble() %>%
    arrange(x,y) %>%
    #filter(z>1) %>%
    group_by(y) %>%
    mutate(y_id=group_indices()) %>%
    ungroup() %>%
    group_by(x) %>%
    mutate(x_id=group_indices()) %>%
    ungroup()

  gam_humber_viz_raster_matrix <-
    gam_humber_viz_raster_id %>%
    select(z,ends_with("_id")) %>%
    pivot_wider(names_from = x_id,values_from = z) %>%
    #naniar::vis_miss()
    select(-y_id) %>%
    as.matrix() %>%
    t()

  gam_humber_viz_raster_id_x <- gam_humber_viz_raster_id %>%
    select(x,x_id) %>%
    distinct()

  gam_humber_viz_raster_id_y <- gam_humber_viz_raster_id %>%
    select(y,y_id) %>%
    distinct()

  gam_humber_viz_raster_list <-
    list(x = gam_humber_viz_raster_id_x$x,
         y = gam_humber_viz_raster_id_y$y,
         z = gam_humber_viz_raster_matrix)

  #image(gam_humber_viz_raster_list)

  raster <- raster::raster(gam_humber_viz_raster_list)

  raster::projection(raster) <- CRS("+proj=longlat +datum=WGS84")

  return(raster)
}
