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
      axis.line = element_line(
        color = "black",
        size = 0.8,
        lineend = "round"
      ),
      axis.ticks = element_line(
        color = "black",
        size = 0.8,
        lineend = "round"
      ),
      axis.text = element_text(color = "black", size = 12),

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

#' plot_png
#'
#' @import ggplot2
#' @importFrom png readPNG
#' @importFrom grid rasterGrob
#' @export
plot_png <- function(file) {
  img <- readPNG(file)
  g <- rasterGrob(img, interpolate = TRUE)

  df <- data.frame(x = c(0, 0.5), y = c(0, 0.5))
  p <- ggplot(df, aes(x = x, y = y)) +
    # geom_blank() +
    xlim(0, 1) +
    ylim(0, 1) +
    annotation_custom(
      g,
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf
    ) +
    blank.theme()
  p
}

#' add.anova.pairwise.comparison
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @export
add.anova.pairwise.comparison <-
  function(# colname must be "value" and "group"
    df,
    # ymin of hline
    ymin,
    # space between hlines
    space,
    # space between label and hline
    spaceLabel,
    # length of tips
    lenTip,
    # comparisons to include
    includeComps = c()) {
    # group => x
    keys <- levels(df$group)
    group2x <- as.list(seq(1, length(keys)))
    names(group2x) <- keys

    # stats
    pc <- TukeyHSD(aov(value ~ group, df))$group
    pc <- as.data.frame(pc)
    pc$comp <- rownames(pc)
    rownames(pc) <- NULL
    colnames(pc) <- c("diff", "lwr", "upr", "p.adj", "comp")
    pc <- pc %>%
      select(comp, p.adj) %>%
      filter(p.adj <= 0.05) %>%
      separate(comp, c("a", "b"), sep = "-", remove = FALSE)

    if (length(includeComps) > 0) {
      pc <- pc %>% filter(comp %in% includeComps)
    }

    # hlines
    X <- c()
    Y <- c()
    Xend <- c()
    Ylabel <- c()
    P <- c()
    for (i in 1:nrow(pc)) {
      P <- append(P, pc$p.adj[i])
      x1 <- group2x[[pc$a[i]]]
      x2 <- group2x[[pc$b[i]]]
      y <- ymin + i * space

      X = append(X, x1)
      Xend = append(Xend, x2)
      Y = append(Y, y)

      Ylabel = append(Ylabel, ymin + i * space + spaceLabel)
    }
    df <- data.frame(
      x = X,
      y = Y,
      xend = Xend,
      yend = Y,
      ylabel = Ylabel,
      p = P
    )

    # tips
    X <- c()
    Y <- c()
    Xend <- c()
    Yend <- c()
    for (i in 1:nrow(pc)) {
      x1 <- group2x[[pc$a[i]]]
      x2 <- group2x[[pc$b[i]]]
      y <- ymin + i * space

      X = append(X, c(x1, x2))
      Xend = append(Xend, c(x1, x2))
      Y = append(Y, c(y, y))
      Yend = append(Yend, c(y - lenTip, y - lenTip))
    }

    df2 <- data.frame(
      x = X,
      y = Y,
      xend = Xend,
      yend = Yend
    )

    p <- list(
      geom_segment(
        data = df,
        aes(
          x = x,
          y = y,
          xend = xend,
          yend = yend
        ),
        color = "black",
        lineend = "round"
      ),
      geom_segment(
        data = df2,
        aes(
          x = x,
          y = y,
          xend = xend,
          yend = yend
        ),
        color = "black",
        lineend = "round"
      ),
      geom_text(data = df,
                aes(
                  x = (x + xend) / 2,
                  y = ylabel,
                  label = ifelse(p < 0.01,
                                 ifelse(p < 0.001, "***", "**"),
                                 "*")
                ))
    )
  }
