## R/cl_themes.R

#' ggplot2 theme based on \code{theme_cowplot}
#'
#' Modified version of \code{theme_cowplot} for publication quality
#' graph creation.
#'
#' @param font.size the main font size for the graph. Equivalent to
#'   the \code{font_size} parameter in \code{theme_cowplot}
#' @export
#' @examples
#' library(ggplot2)
#' data(mtcars)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'  geom_point(aes(color = hp)) +
#'  theme_cowplot_cl() +
#'  labs(title = "Graph title",
#'       subtitle = "Graph Subtitle", 
#'       caption = "Graph caption")
theme_cowplot_cl <- function(font.size = 11) {
  rel_med_small = 13 / 14
  cowplot::theme_cowplot(font.size, rel_large = 15 / 14) %+replace%
    theme(
      legend.background = element_rect(color = "black", size = 0.3, linetype = "solid"),
      legend.justification = "center",
      ##legend.key.size to increase space between legend items
      ##from https://stackoverflow.com/a/50593988
      legend.key.height = unit(1.0, 'lines'),
      ##to undo the legend margins from cowplot
      legend.margin = margin(3, 4, 3, 3),
      legend.title = element_text(size = rel(rel_med_small), hjust = 0)
    ) +
    cowplot::background_grid(major = "xy", minor = "none", size.major = 0.1,
                           color.major = "gray90")
}

#' ggplot2 theme based on \code{theme_bw} and \code{theme_cowplot}
#' that works well with facets
#'
#' Modified version of \code{theme_bw} and \code{theme_cowplot} for
#' publication quality graph creation that works well with facets
#'
#' @param font.size the main font size for the graph. Equivalent to
#'   the \code{font_size} parameter in \code{theme_bw}
#' @export
#' @examples
#' library(ggplot2)
#' data(mtcars)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point(aes(color = hp)) +
#'   facet_grid(rows = vars(cyl)) + 
#'   theme_bw_cl() +
#'   labs(title = "Graph title",
#'        subtitle = "Graph Subtitle", 
#'        caption = "Graph caption") 
theme_bw_cl <- function(font.size = 11) {

  ##based on theme_cowplot()

  ##parameters to theme_cowplot
  font_size = font.size; font_family = ""; line_size = 0.5;
  rel_med_small = 13 / 14; 
  rel_small = 12/14; rel_tiny = 11/14; rel_large = 15/14;
  half_line <- font_size/2
  small_size <- rel_small * font_size



  theme_bw(base_size = font_size, base_family = family) %+replace%
    theme(## line = element_line(color = "black", size = line_size,
          ##                     linetype = 1, lineend = "butt"),
          ## rect = element_rect(fill = NA,
          ##                     color = NA, size = line_size, linetype = 1),
          text = element_text(family = font_family,
                              face = "plain", color = "black", size = font_size,
                              hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9,
                              margin = margin(), debug = FALSE),
          ## axis.line = element_line(color = "black",
          ##                          size = line_size, lineend = "square"), axis.line.x = NULL,
          ## axis.line.y = NULL, axis.text = element_text(color = "black",
          ##                                              size = small_size),
          axis.text.x = element_text(margin = margin(t = small_size/4), vjust = 1, color = "black"),
          axis.text.x.top = element_text(margin = margin(b = small_size/4), vjust = 0, color = "black"),
          axis.text.y = element_text(margin = margin(r = small_size/4), hjust = 1, color = "black"),
          axis.text.y.right = element_text(margin = margin(l = small_size/4), hjust = 0, color = "black"),
          axis.ticks = element_line(color = "black",
                                    size = line_size), axis.ticks.length = unit(half_line/2,
                                                                                "pt"),
          axis.title.x = element_text(margin = margin(t = half_line/2),
                                      vjust = 1),
          axis.title.x.top = element_text(margin = margin(b = half_line/2),
                                          vjust = 0),
          axis.title.y = element_text(angle = 90,
                                      margin = margin(r = half_line/2), vjust = 1),
          axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line/2),
                                            vjust = 0),
          legend.background = element_rect(color = "black", size = 0.3, linetype = "solid"),
          legend.spacing = unit(font_size, "pt"),
          legend.spacing.x = NULL,
          legend.spacing.y = NULL,
          legend.margin = margin(3, 3, 3, 3), ##Different from theme_cowplot()!!!
          legend.key = element_blank(),
          legend.key.size = unit(1.1 * font_size, "pt"),
          legend.key.height = unit(1.0, 'lines'), ##Different from theme_cowplot()!!!
          legend.key.width = NULL,
          legend.text = element_text(size = rel(rel_small)),
          legend.text.align = NULL, legend.title = element_text(size = rel(rel_med_small), hjust = 0),
          legend.title.align = NULL, legend.position = "right",
          legend.direction = NULL, legend.justification = c("left", "center"),
          legend.box = NULL, legend.box.margin = margin(0, 0, 0, 0),
          legend.box.background = element_blank(),
          legend.box.spacing = unit(font_size, "pt"), panel.background = element_blank(),
          ##Commented out from cowplot theme
          ## panel.border = element_blank(), panel.grid = element_blank(),
          ## panel.grid.major = NULL, panel.grid.minor = NULL,
          ## panel.grid.major.x = NULL, panel.grid.major.y = NULL,
          ## panel.grid.minor.x = NULL, panel.grid.minor.y = NULL,
          ## panel.spacing = unit(half_line, "pt"), panel.spacing.x = NULL,
          ## panel.spacing.y = NULL, panel.ontop = FALSE, strip.background = element_rect(fill = "grey80"),
          strip.text = element_text(size = rel(rel_small),
                                    margin = margin(half_line/2, half_line/2, half_line/2, half_line/2)),
          strip.text.x = NULL, strip.text.y = element_text(angle = -90),
          ## strip.placement = "inside", strip.placement.x = NULL,
          ## strip.placement.y = NULL,
          ## strip.switch.pad.grid = unit(half_line/2, "pt"),
          ## strip.switch.pad.wrap = unit(half_line/2, "pt"),
          ##plot.background = element_blank(),
          plot.title = element_text(face = "bold", size = rel(rel_large), hjust = 0,
                                    vjust = 1, margin = margin(b = half_line)),
          plot.subtitle = element_text(size = rel(rel_small),
                                       hjust = 0, vjust = 1, margin = margin(b = half_line)),
          plot.caption = element_text(size = rel(rel_tiny),
                                      hjust = 1, vjust = 1, margin = margin(t = half_line)),
          plot.tag = element_text(face = "bold", hjust = 0, vjust = 0.7),
          plot.tag.position = c(0, 1), plot.margin = margin(half_line, half_line, half_line, half_line), complete = TRUE)
}
