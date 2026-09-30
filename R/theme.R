#' Linköping University theme for ggplot2
#'
#' Creates a custom ggplot2 theme based on the visual identity of
#' Linköping University.
#'
#' @param base_size Numeric. Base font size used for text elements
#' in the plot.
#'
#' @return A list of ggplot2 components that can be added to a
#' ggplot object using the `+` operator.
#'
#' @references
#' \url{https://www.ida.liu.se/~732A94/wwwfiles/filer/LiU_grafisk_manual.pdf}
#'
#' @import ggplot2
#' @import png
#' @import grid
#' @export
#'
theme <- function(base_size = 10) {
  liu_theme <- ggplot2::theme_minimal()

  liu_theme <- liu_theme + ggplot2::theme(
    text = ggplot2::element_text(
      family = "Arial",
      color = "white",
      face = "plain"
    ),
    plot.title = ggplot2::element_text(
      size = base_size * 1.5,
      margin = ggplot2::margin(b = 10),
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(
      size = base_size,
      margin = ggplot2::margin(b = 7.5)
    ),
    panel.background = ggplot2::element_rect(
      fill = "#ffffff"
    ),
    panel.grid.major = ggplot2::element_line(
      color = "#6a7e9154",
      linewidth = 0.3
    ),
    panel.border = ggplot2::element_rect(
      color = "#ffffff",
      fill = NA
    ),
    axis.text = ggplot2::element_text(
      color = "#ffffff",
    ),
    axis.title.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 5),
      size = base_size
    ),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 5),
      size = base_size
    ),
    axis.text.x = ggplot2::element_text(
      size = base_size * 0.9
    ),
    axis.text.y = ggplot2::element_text(
      size = base_size * 0.9
    ),
    legend.position = "right",
    legend.title = ggplot2::element_text(
      size = base_size * 0.9,
    ),
    legend.text = ggplot2::element_text(
      size = base_size * 0.75
    ),
    plot.caption = ggplot2::element_text(
      size = base_size * 0.5,
      hjust = 1,
      margin = ggplot2::margin(t = 45)
    ),
    plot.margin = ggplot2::margin(
      t = 40,
      r = 40,
      b = 80,
      l = 40
    ),
    plot.background = ggplot2::element_rect(
      fill = "#00b9e7"
    ),
  )
  logo_path <- system.file("logos", "liu_sec.png", package = "Lab4")
  png <- png::readPNG(logo_path, native = TRUE) #|>
  # grid::rasterGrob()

  grob <- grid::rasterGrob(
    png,
    x = grid::unit(0.255, "npc"),
    y = grid::unit(-0.35, "npc"),
    width = grid::unit(0.3, "npc"),
    just = c("right", "bottom")
  )

  liu_logo <- ggplot2::annotation_custom(
    grob = grob,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf
  )
  return(
    list(
      liu_theme,
      liu_logo,
      ggplot2::coord_cartesian(clip = "off")
    )
  )
}
