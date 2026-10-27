#' Visualize Mean Arrival Delays Across US Airports
#'
#' @description
#' Creates a geographic visualization of mean flight arrival delays
#' for destination airports using data from the nycflights13 package.
#'
#' @return A ggplot object showing airport locations colored by
#' mean arrival delay in minutes.
#'
#' @export
visualize_airport_delays <- function() {
  df <- nycflights13::flights |>
    dplyr::select(dest, arr_delay) |>
    dplyr::group_by(dest) |>
    dplyr::summarise(
      mean_arr_delay = mean(arr_delay, na.rm = TRUE)
    ) |>
    dplyr::left_join(
      nycflights13::airports |>
        dplyr::select(faa, lat, lon),
      by = c("dest" = "faa")
    ) |>
    dplyr::filter(
      !is.na(lon),
      !is.na(lat),
      !is.na(mean_arr_delay)
    )

  usa <- rnaturalearth::ne_states(
    country = "United States of America",
    returnclass = "sf"
  )

  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = usa) +
    ggplot2::coord_sf(
      xlim = c(-180, -60),
      ylim = c(1, 80),
      default_crs = sf::st_crs(4326)
    ) +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(x = lon, y = lat, color = mean_arr_delay)
    ) +
    ggplot2::scale_color_gradientn(
      colours = c(
        "#00fffb",
        "blue",
        "darkgreen",
        "green",
        "yellow",
        "orange",
        "red"
      ),
      name = "Mean arrival delay (min)"
    ) +
    ggplot2::labs(
      y = "Latitude",
      x = "Longitude",
      title = "Mean Arrival Flight Delays",
      caption = "Source: nycflights13"
    )
  return(plot)
}
