# From <https://nanx.me/blog/post/ggplot2-color-interpolation/>

#' Adaptive palette (discrete)
#'
#' Create a discrete palette that will use the first `n` colors from
#' the supplied color values when the palette has enough colors.
#' Otherwise, use an interpolated color palette.
#'
#' @param values Color values.
pal_ramp <- function(values) {
  force(values)
  function(n) {
    if (n <= length(values)) {
      values[seq_len(n)]
    } else {
      colorRampPalette(values, alpha = TRUE)(n)
    }
  }
}

#' Adaptive color palette generator
#'
#' Adaptive color palette generator for ggsci color palettes using `pal_ramp()`.
#'
#' @param name Color palette name in ggsci
#' @param palette Color palette type in ggsci
#' @param alpha Transparency level, a real number in (0, 1].
#'
#' @details See `names(ggsci:::ggsci_db)` for all color palette names in ggsci.
#' See `names(ggsci:::ggsci_db$"pal")` for available palette types under
#' the palette `pal`.
pal_adaptive <- function(name, palette, alpha = 1) {
  if (alpha > 1L | alpha <= 0L) stop("alpha must be in (0, 1]")

  raw_cols <- ggsci:::ggsci_db[[name]][[palette]]
  raw_cols_rgb <- col2rgb(raw_cols)
  alpha_cols <- rgb(
    raw_cols_rgb[1L, ], raw_cols_rgb[2L, ], raw_cols_rgb[3L, ],
    alpha = alpha * 255L, names = names(raw_cols),
    maxColorValue = 255L
  )

  pal_ramp(unname(alpha_cols))
}

#' Adaptive color scales
#'
#' @inheritParams pal_adaptive
#' @param ... additional parameters for [ggplot2::discrete_scale()].
scale_color_adaptive <- function(name, palette, alpha = 1, ...) {
  ggplot2::discrete_scale("colour", name, pal_adaptive(name, palette, alpha), ...)
}

scale_fill_adaptive <- function(name, palette, alpha = 1, ...) {
  ggplot2::discrete_scale("fill", name, pal_adaptive(name, palette, alpha), ...)
}
