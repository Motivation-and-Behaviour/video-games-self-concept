#' Path diagram of a fitted panel model
#'
#' Handles both model families. For the RI-CLPM it draws the conventional
#' layout — observed variables in squares, the within-person components and
#' random intercepts in ellipses — with the lagged paths running between the
#' within-person components. For the CLPM, which has no latent layer, the
#' lagged paths run between the observed variables directly and the figure is
#' correspondingly flatter. Which layout to use is detected from the fit.
#'
#' Paths constrained equal across lags carry the same estimate on both arrows.
#' Covariates, where present, are estimated but not drawn; the annotation says
#' so.
#'
#' Built directly in ggplot2 rather than with a dedicated SEM plotting package
#' so the figures match the rest of the report's styling.
#'
#' @title plot_path_diagram
#' @param fit A fitted model from `fit_riclpm()` or `fit_clpm()`.
#' @param waves Age bands, in order.
#' @return A ggplot object.
#' @author Taren Sanders
#' @export
plot_path_diagram <- function(fit, waves = c(10, 12, 14)) {
  require(ggplot2)

  accent <- "#4269D0"
  grey <- "grey40"
  bands <- age_band_labels(waves)
  xpos <- 1 + (seq_along(waves) - 1) * 2.5
  mid <- mean(xpos)
  std <- lavaan::standardizedSolution(fit)
  riclpm <- all(c("RI_x", "RI_y") %in% lavaan::lavNames(fit, "lv"))

  # Names of the nodes the lagged paths connect: the within-person components
  # in an RI-CLPM, the observed variables in a CLPM.
  xn <- lavaan_within_names(fit, "vg", waves)
  yn <- lavaan_within_names(fit, "sdq", waves)
  # Vertical position and half-height of those nodes, used to place the
  # within-wave correlation curves.
  row_y <- if (riclpm) 0.50 else 0.75
  row_rb <- if (riclpm) 0.26 else 0.30

  # ---- nodes ------------------------------------------------------------
  nodes <- if (riclpm) {
    dplyr::bind_rows(
      node_row(
        paste0("vg_", waves),
        xpos,
        1.45,
        "rect",
        paste0("Video games\n", bands)
      ),
      node_row(xn, xpos, row_y, "ellipse", "VG", 0.40, row_rb),
      node_row(yn, xpos, -row_y, "ellipse", "SDQ", 0.40, row_rb),
      node_row(
        paste0("sdq_", waves),
        xpos,
        -1.45,
        "rect",
        paste0("SDQ\n", bands)
      ),
      node_row("RI_x", mid, 2.55, "ellipse", "RI\nvideo games", 0.70, 0.36),
      node_row("RI_y", mid, -2.55, "ellipse", "RI\nSDQ", 0.70, 0.36)
    )
  } else {
    dplyr::bind_rows(
      node_row(
        xn,
        xpos,
        row_y,
        "rect",
        paste0("Video games\n", bands),
        0.62,
        row_rb
      ),
      node_row(yn, xpos, -row_y, "rect", paste0("SDQ\n", bands), 0.62, row_rb)
    )
  }

  # ---- edges ------------------------------------------------------------
  lags <- seq_len(length(waves) - 1)
  # Autoregressions and cross-lags. Cross-lag labels sit at different points
  # along their paths so the two crossing diagonals do not collide at the
  # centre.
  lagged_edges <- purrr::map(lags, function(i) {
    dplyr::bind_rows(
      edge_row(xn[i], xn[i + 1], lag_est(std, "ar_x", i), 0.5, 0.14),
      edge_row(yn[i], yn[i + 1], lag_est(std, "ar_y", i), 0.5, -0.14),
      edge_row(xn[i], yn[i + 1], lag_est(std, "cl_xy", i), 0.30, 0),
      edge_row(yn[i], xn[i + 1], lag_est(std, "cl_yx", i), 0.70, 0)
    )
  }) |>
    dplyr::bind_rows()

  # Measurement structure: in the RI-CLPM each observed variable is the sum of
  # its random intercept and its within-person component. The CLPM has none.
  fixed_edges <- if (riclpm) {
    purrr::map(waves, function(w) {
      dplyr::bind_rows(
        edge_row(paste0("wvg_", w), paste0("vg_", w), NA, 0.5, 0, "fixed"),
        edge_row(paste0("wsdq_", w), paste0("sdq_", w), NA, 0.5, 0, "fixed"),
        edge_row("RI_x", paste0("vg_", w), NA, 0.5, 0, "fixed"),
        edge_row("RI_y", paste0("sdq_", w), NA, 0.5, 0, "fixed")
      )
    }) |>
      dplyr::bind_rows()
  } else {
    NULL
  }

  structural <- dplyr::bind_rows(lagged_edges, fixed_edges)

  edges <- structural |>
    dplyr::left_join(
      dplyr::select(
        nodes,
        from = id,
        x0 = x,
        y0 = y,
        s0 = shape,
        a0 = ra,
        b0 = rb
      ),
      by = "from"
    ) |>
    dplyr::left_join(
      dplyr::select(
        nodes,
        to = id,
        x1 = x,
        y1 = y,
        s1 = shape,
        a1 = ra,
        b1 = rb
      ),
      by = "to"
    ) |>
    dplyr::mutate(
      dx = x1 - x0,
      dy = y1 - y0,
      len = sqrt(dx^2 + dy^2),
      t0 = trim_len(dx, dy, s0, a0, b0),
      t1 = trim_len(dx, dy, s1, a1, b1),
      xs = x0 + dx * t0 / len,
      ys = y0 + dy * t0 / len,
      xe = x1 - dx * t1 / len,
      ye = y1 - dy * t1 / len,
      lx = xs + (xe - xs) * at,
      ly = ys + (ye - ys) * at + nudge
    )

  # Within-wave (residual) correlations, drawn as curves bowing to the right
  # of each column so they clear the crossing lagged paths.
  cors <- tibble::tibble(
    x = xpos,
    y = row_y - row_rb,
    label = purrr::map_chr(
      seq_along(waves),
      ~ star_label(std, xn[.x], "~~", yn[.x])
    )
  )

  note <- if (riclpm) {
    sprintf(
      "Random intercept correlation = %s",
      star_label(std, "RI_x", "~~", "RI_y")
    )
  } else {
    "Adjusted for sex and socioeconomic position (paths not shown)"
  }
  note_y <- if (riclpm) 3.25 else row_y + 3 * row_rb

  ggplot() +
    ggplot2::geom_polygon(
      data = node_shapes(nodes),
      aes(x = x, y = y, group = id),
      fill = "white",
      colour = "grey35",
      linewidth = 0.4
    ) +
    ggplot2::geom_segment(
      data = dplyr::filter(edges, type == "fixed"),
      aes(x = xs, y = ys, xend = xe, yend = ye),
      colour = "grey70",
      linewidth = 0.3,
      linetype = "22",
      arrow = arrow(length = unit(0.10, "cm"), type = "closed")
    ) +
    ggplot2::geom_curve(
      data = cors,
      aes(x = x + 0.05, y = y, xend = x + 0.05, yend = -y),
      curvature = 0.9,
      colour = grey,
      linewidth = 0.35,
      arrow = arrow(length = unit(0.09, "cm"), ends = "both", type = "closed")
    ) +
    ggplot2::geom_segment(
      data = dplyr::filter(edges, type == "path"),
      aes(x = xs, y = ys, xend = xe, yend = ye),
      colour = accent,
      linewidth = 0.45,
      arrow = arrow(length = unit(0.16, "cm"), type = "closed")
    ) +
    ggplot2::geom_text(
      data = cors,
      aes(x = x + 0.58, y = 0, label = label),
      size = 2.6,
      colour = grey
    ) +
    ggplot2::geom_label(
      data = dplyr::filter(edges, type == "path"),
      aes(x = lx, y = ly, label = label),
      size = 2.6,
      colour = accent,
      fill = "white",
      linewidth = 0,
      label.padding = unit(0.10, "lines")
    ) +
    ggplot2::geom_text(
      data = nodes,
      aes(x = x, y = y, label = label),
      size = 2.6,
      lineheight = 0.95
    ) +
    ggplot2::annotate(
      "text",
      x = mid,
      y = note_y,
      label = note,
      size = 2.7,
      colour = grey
    ) +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.margin = margin(2, 6, 2, 6))
}

#' Build a row per node, with position and shape half-extents
#' @noRd
node_row <- function(ids, x, y, shape, label, ra = 0.62, rb = 0.30) {
  tibble::tibble(
    id = ids,
    x = x,
    y = y,
    shape = shape,
    label = label,
    ra = ra,
    rb = rb
  )
}

#' Build a row per directed edge
#' @noRd
edge_row <- function(from, to, label, at, nudge, type = "path") {
  tibble::tibble(
    from = from,
    to = to,
    label = label,
    at = at,
    nudge = nudge,
    type = type
  )
}

#' Distance from a node centre to its boundary along a given direction
#'
#' `ra`/`rb` are the half-extents; the ellipse and rectangle cases have
#' different closed forms.
#' @noRd
trim_len <- function(dx, dy, shape, ra, rb) {
  scale <- ifelse(
    shape == "ellipse",
    1 / sqrt((dx / ra)^2 + (dy / rb)^2),
    pmin(
      ifelse(dx == 0, Inf, abs(ra / dx)),
      ifelse(dy == 0, Inf, abs(rb / dy))
    )
  )
  scale * sqrt(dx^2 + dy^2)
}

#' Polygon outlines for every node
#' @noRd
node_shapes <- function(nodes) {
  purrr::pmap(nodes, function(id, x, y, shape, label, ra, rb) {
    if (identical(shape, "ellipse")) {
      t <- seq(0, 2 * pi, length.out = 60)
      tibble::tibble(id = id, x = x + ra * cos(t), y = y + rb * sin(t))
    } else {
      tibble::tibble(
        id = id,
        x = x + c(-ra, ra, ra, -ra),
        y = y + c(-rb, -rb, rb, rb)
      )
    }
  }) |>
    dplyr::bind_rows()
}

#' Standardised estimate for a lagged path, with significance stars
#'
#' Looks for the constrained label first (no lag suffix), then the
#' lag-specific one, so the same call works for either specification.
#' @noRd
lag_est <- function(std, base, lag) {
  row <- std[std$label == base, ]
  if (nrow(row) == 0) {
    row <- std[std$label == paste0(base, lag), ]
  }
  if (nrow(row) == 0) {
    return(NA_character_)
  }
  paste0(fmt_fit(row$est.std[1]), sig_stars(row$pvalue[1]))
}

#' Standardised (co)variance estimate with significance stars
#' @noRd
star_label <- function(std, lhs, op, rhs) {
  row <- std[std$lhs == lhs & std$op == op & std$rhs == rhs, ]
  if (nrow(row) == 0) {
    return(NA_character_)
  }
  paste0(fmt_fit(row$est.std[1]), sig_stars(row$pvalue[1]))
}

#' Conventional significance stars
#' @noRd
sig_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < .001 ~ "***",
    p < .01 ~ "**",
    p < .05 ~ "*",
    TRUE ~ ""
  )
}
