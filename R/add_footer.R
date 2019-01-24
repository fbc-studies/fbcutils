#' Add a footer with a caption and a logo into a ggplot
#' @param x a ggplot object
#' @param caption text to display in the footer
#' @param logo path to a jpg/jpeg/png file. Set to `NA` to disable. Defaults to
#'   a short THL pearl logo.
#' @export
#' @import ggplot2
add_footer <- function(x, caption = "", logo = NULL, height = 0.5) {

  if (is.null(logo)) {
    logo <- thl_logo
  } else if (!is.na(logo)) {
    stopifnot(is.character(logo))
    ext <- tools::file_ext(logo)

    if (ext %in% c("jpg", "jpeg")) {
      logo <- jpeg::readJPEG(logo)
    } else if (file_ext_ == "png") {
      logo <- png::readPNG(logo)
    } else {
      warning(
        "No logo file or a non-working logo file was provided.\n",
        "You can place one (logo.png/logo.jpg) in the working directory\n",
        "or provide one with the logo argument."
      )
      logo <- NA
    }
  }

  capt <- grid::textGrob(caption, hjust = 0, x = grid::unit(0, "npc"),
                         gp = grid::gpar(fontsize = x$theme$axis.text.x$size))

  logo <- grid::rasterGrob(logo, x = grid::unit(1, "npc"), hjust = 1)
  grobs <- grid::grobTree(logo, capt)

  gt <- ggplot_gtable(ggplot_build(x))

  gt %>%
   gtable::gtable_add_rows(grid::unit(last(gt$heights), "points") * 2, nrow(.) - 1) %>%
   gtable::gtable_add_grob(
     grid::linesGrob(y = grid::unit(0.5, "npc")),
     name = "footer-line",
     t = nrow(.) - 1,
     l = 2,
     r = ncol(.) - 1
  ) %>%
   gtable::gtable_add_rows(grid::unit(height, "cm"), nrow(.) - 1) %>%
   gtable::gtable_add_grob(
     grobs,
     name = "footer",
     t = nrow(.) - 1,
     l = 2,
     r = ncol(.) - 1
   )
}

ggsave_footer <- function(gt, file, height, width) {
  png(file, h = height, w = width, units = "in", res = 300)
  grid::grid.newpage()
  grid::grid.draw(gt)
  dev.off()
}

print_footer <- function(gt) {
  grid::grid.newpage()
  grid::grid.draw(gt)
}