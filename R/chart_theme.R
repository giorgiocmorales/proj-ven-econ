ven_chart_specs <- list(
  width_in = 8,
  height_in = 6,
  dpi = 300,
  panel_width_in = 6.5,
  panel_height_in = 6.5 / 1.56,
  base_family = "serif",
  base_size = 12
)

ven_theme <- function(base_family = ven_chart_specs$base_family,
                      base_size = ven_chart_specs$base_size) {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      axis.text = ggplot2::element_text(color = "grey20"),
      axis.title = ggplot2::element_text(color = "grey30"),
      plot.title = ggplot2::element_text(
        size = base_size + 3,
        face = "plain",
        hjust = 0,
        margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size,
        hjust = 0,
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        size = base_size - 3,
        hjust = 0,
        color = "grey35",
        margin = ggplot2::margin(t = 6)
      ),
      axis.line.x = ggplot2::element_line(color = "#2b3033", linewidth = 0.35, linetype = "solid"),
      axis.line.y = ggplot2::element_line(color = "#2b3033", linewidth = 0.35, linetype = "solid"),
      axis.ticks = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(0, "pt"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        color = "grey84",
        linetype = "dashed",
        linewidth = 0.3
      ),
      panel.border = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.key.height = grid::unit(0.16, "in"),
      plot.margin = ggplot2::margin(10, 34, 10, 12),
      axis.text.y.right = ggplot2::element_text(color = "grey20"),
      axis.title.y.right = ggplot2::element_text(color = "grey30", margin = ggplot2::margin(l = 6)),
      axis.ticks.y.right = ggplot2::element_blank()
    )
}

ven_set_theme <- function() {
  ggplot2::theme_set(ven_theme())
}

ven_blank_secondary_axis <- function() {
  ggplot2::dup_axis(name = NULL, labels = function(x) rep("", length(x)))
}

ven_panel_plot <- function(plot,
                           panel_width_in = ven_chart_specs$panel_width_in,
                           panel_height_in = ven_chart_specs$panel_height_in) {
  egg::set_panel_size(
    plot,
    width = grid::unit(panel_width_in, "in"),
    height = grid::unit(panel_height_in, "in")
  )
}

ven_save_plot <- function(filename,
                          plot,
                          width_in = ven_chart_specs$width_in,
                          height_in = ven_chart_specs$height_in,
                          dpi = ven_chart_specs$dpi,
                          panel_width_in = ven_chart_specs$panel_width_in,
                          panel_height_in = ven_chart_specs$panel_height_in,
                          device = ragg::agg_png,
                          ...) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    filename = filename,
    plot = ven_panel_plot(plot, panel_width_in, panel_height_in),
    width = width_in,
    height = height_in,
    units = "in",
    dpi = dpi,
    device = device,
    ...
  )
}
