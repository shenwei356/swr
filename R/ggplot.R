#' shenwei356.theme
#'
#' shenwei356's ggplot2 theme
#'
#' @import ggplot2
#' @export
shenwei356.theme <- function() {
  shenwei356.theme <- theme_bw() +
    theme(
      text = element_text(
        size = 14,
        color = "black",
        # face = "bold",
        family = "arial"
      ),
      title = element_text(size = 16),

      panel.border = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      axis.title = element_text(size = 14),
      axis.line = element_line(color = "black", size = 0.8),
      axis.ticks = element_line(size = 0.8),
      axis.text = element_text(size = 11, color = "black"),

      legend.key = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 14),
      legend.background = element_rect(fill = "transparent"),

      strip.background = element_rect(
        colour = "white",
        fill = "white",
        size = 0.2
      ),
      strip.text = element_text(size = 14)
    )
  shenwei356.theme
}


#' blank.theme
#'
#' @import ggplot2
#' @export
blank.theme <- function() {
  blank.theme <- theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    text = element_text(
      size = 14,
      family = "arial",
      face = "bold"
    ),
    plot.title = element_text(
      size = 16,
      family = "arial",
      face = "bold"
    )
  )
  blank.theme
}

#' plot.image
#'
#' @import ggplot2
#' @importFrom png readPNG
#' @importFrom grid rasterGrob
#' @export
plot.png <- function(file) {
  img <- readPNG(file)
  g <- rasterGrob(img, interpolate = TRUE)

  df <- data.frame(x = c(0, 0.5), y = c(0, 0.5))
  p <- ggplot(df, aes(x = x, y = y)) +
    # geom_blank() +
    xlim(0, 1) +
    ylim(0, 1) +
    # ggtitle("Groups") +
    annotation_custom(
      g,
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf
    ) +
    blank.theme
  p
}
