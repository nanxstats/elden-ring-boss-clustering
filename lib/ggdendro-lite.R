# From <https://atrebas.github.io/post/2019-06-08-lightweight-dendrograms/>

dendro_data_k <- function(hc, k) {
  hcdata <- ggdendro::dendro_data(hc, type = "rectangle")
  seg <- hcdata$segments
  labclust <- cutree(hc, k)[hc$order]
  segclust <- rep(0L, nrow(seg))
  heights <- sort(hc$height, decreasing = TRUE)
  height <- mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)

  for (i in 1:k) {
    xi <- hcdata$labels$x[labclust == i]
    idx1 <- seg$x >= min(xi) & seg$x <= max(xi)
    idx2 <- seg$xend >= min(xi) & seg$xend <= max(xi)
    idx3 <- seg$yend < height
    idx <- idx1 & idx2 & idx3
    segclust[idx] <- i
  }

  idx <- which(segclust == 0L)
  segclust[idx] <- segclust[idx + 1L]
  hcdata$segments$clust <- segclust
  hcdata$segments$line <- as.integer(segclust < 1L)
  hcdata$labels$clust <- labclust

  hcdata
}

set_labels_params <- function(
    nbLabels,
    direction = c("tb", "bt", "lr", "rl"),
    fan = FALSE) {
  if (fan) {
    angle <- 360 / nbLabels * 1:nbLabels + 90
    idx <- angle >= 90 & angle <= 270
    angle[idx] <- angle[idx] + 180
    hjust <- rep(0, nbLabels)
    hjust[idx] <- 1
  } else {
    angle <- rep(0, nbLabels)
    hjust <- 0
    if (direction %in% c("tb", "bt")) {
      angle <- angle + 45
    }
    if (direction %in% c("tb", "rl")) {
      hjust <- 1
    }
  }
  list(angle = angle, hjust = hjust, vjust = 0.5)
}

plot_ggdendro <- function(
    hcdata,
    direction = c("lr", "rl", "tb", "bt"),
    fan = FALSE,
    scale.color = NULL,
    branch.size = 1,
    label.size = 3,
    nudge.label = 0.01,
    expand.y = 0.1) {
  direction <- match.arg(direction)
  ybreaks <- pretty(ggdendro::segment(hcdata)$y, n = 5)
  ymax <- max(ggdendro::segment(hcdata)$y)

  ## branches
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = ggdendro::segment(hcdata),
      ggplot2::aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend,
        linetype = factor(line),
        colour = factor(clust)
      ),
      lineend = "round",
      show.legend = FALSE,
      linewidth = branch.size
    )

  ## orientation
  if (fan) {
    p <- p +
      ggplot2::coord_polar(direction = -1) +
      ggplot2::scale_x_continuous(
        breaks = NULL,
        limits = c(0, nrow(ggdendro::label(hcdata)))
      ) +
      ggplot2::scale_y_reverse(breaks = ybreaks)
  } else {
    p <- p + ggplot2::scale_x_continuous(breaks = NULL)
    if (direction %in% c("rl", "lr")) {
      p <- p + ggplot2::coord_flip()
    }
    if (direction %in% c("bt", "lr")) {
      p <- p + ggplot2::scale_y_reverse(breaks = ybreaks)
    } else {
      p <- p + ggplot2::scale_y_continuous(breaks = ybreaks)
      nudge.label <- -(nudge.label)
    }
  }

  # labels
  labelParams <- set_labels_params(nrow(hcdata$labels), direction, fan)
  hcdata$labels$angle <- labelParams$angle

  p <- p +
    ggplot2::geom_text(
      data = ggdendro::label(hcdata),
      ggplot2::aes(
        x = x,
        y = y,
        label = label,
        colour = factor(clust),
        angle = angle
      ),
      vjust = labelParams$vjust,
      hjust = labelParams$hjust,
      nudge_y = ymax * nudge.label,
      size = label.size,
      show.legend = FALSE
    )

  # colors and limits
  if (!is.null(scale.color)) {
    p <- p + ggplot2::scale_color_manual(values = scale.color)
  }

  ylim <- -round(ymax * expand.y, 1)
  p <- p + ggplot2::expand_limits(y = ylim)

  p
}
