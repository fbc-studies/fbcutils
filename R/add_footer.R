add_footer <- function(x, caption = "", logo = NULL, height = 0.5) {
  
  probable_files <- c("logo.png", "logo.jpg", "//helfs01.thl.fi/documents/fbc_projects/p/r-helpers/R/logo.jpg")
  
  no_logo_warning <- "No logo file or a non-working logo file was provided.
  You can place one (logo.png/logo.jpg) in the working directory or provide one
  with the logo argument."
  
  if (is.null(logo) & any(file.exists(probable_files))) {
    logo <- subset(probable_files, file.exists(probable_files))[1]
  }

  if (!is.null(logo)) {

    if (tools::file_ext(logo) %in% c("jpg", "jpeg", "png")) {
      file_ext_ <- tools::file_ext(logo)

      if (file_ext_ %in% c("jpg", "jpeg")) {
          logo_img <- jpeg::readJPEG(logo)
        } else if (file_ext_ == "png") {
          logo_img <- png::readPNG(logo)
        }
    } else {
        warning(no_logo_warning)
      }
  } else if (is.null(logo)) {
    warning(no_logo_warning)
  }
  
  capt <- grid::textGrob(caption, hjust = 0, x = grid::unit(0, "npc"), gp = grid::gpar(fontsize = x$theme$axis.text.x$size))

  if (exists("logo_img")) {
    logo <- grid::rasterGrob(logo_img, x = grid::unit(1, "npc"), hjust = 1)
    grobs <- grid::grobTree(logo, capt)
  } else {
    grobs <- grid::grobTree(capt)
  }

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