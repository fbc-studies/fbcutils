thl_colors <- c(
  `sininen1` = "#2f62ad",
  `sininen2` = "#2f62ad",
  `sininen3` = "#0e1e47",
  `sininen4` = "#4a8aba",
  `sininen5` = "#93b4ce",
  `turkoosi1` = "#7cd0d8",
  `turkoosi2` = "#11414c",
  `vihrea1` = "#519b2f",
  `vihrea2` = "#7bc143",
  `harmaa` = "#dcdfe2",
  `punainen1` = "#be3f72",
  `punainen2` = "#c97498",
  `punainen3` = "#d2aabd",
  `roosa` = "#cc77ac",
  `orange1` = "#faa61a",
  `orange2` = "#f0b95d",
  `orange3` = "#e6cc9f",
  `cyanide1` = "#29a0c1",
  `cyanide2` = "#7cb4cb",
  `cyanide3` = "#afc9d6",
  `haivutus1` = "#fce4c3",
  `haivutus2` = "#d3eaed",
  `GrBu3` = "#94be7b",
  `GrBu4` = "#b8ceae",
  puuttuva = "#c3c2c6"
)

thl_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (thl_colors)

  thl_colors[cols]
}

thl_palettes <- list(
  AreaSet1 = thl_cols("vihrea1", "sininen2", "roosa",
                      "cyanide1", "punainen1", "orange1"),
  AreaPaired1 = thl_cols("vihrea1", "vihrea2"),
  AreaPaired2 = thl_cols("sininen2", "cyanide1"),
  AreaPaired3 = thl_cols("punainen1", "roosa"),
  AreaPaired4 = thl_cols("vihrea1", "sininen2"),
  AreaPaired5 = thl_cols("cyanide1", "orange1"),
  AreaPaired6 = thl_cols("vihrea1", "orange1"),
  AreaTriplet1 = thl_cols("vihrea1", "sininen2", "orange1"),
  AreaTriplet2 = thl_cols("sininen2", "cyanide1", "roosa"),
  AreaTriplet3 = thl_cols("vihrea1", "sininen2", "roosa"),
  AreaTriplet4 = thl_cols("sininen2", "punainen1", "orange1"),
  AreaTriplet5 = thl_cols("vihrea1", "orange1", "roosa"),
  AreaTriplet6 = thl_cols("punainen1", "orange1", "roosa"),
  Set1 = thl_cols("vihrea1", "sininen2", "punainen1",
                  "cyanide1", "orange1", "roosa"),
  Paired1 = thl_cols("vihrea1", "sininen2"),
  Paired2 = thl_cols("sininen2", "punainen1"),
  Paired3 = thl_cols("vihrea1", "punainen1"),
  Paired4 = thl_cols("vihrea1", "cyanide1"),
  Paired5 = thl_cols("cyanide1", "punainen1"),
  Paired6 = thl_cols("vihrea1", "orange1"),
  Triplet1 = thl_cols("vihrea1", "sininen2", "punainen1"),
  Triplet2 = thl_cols("vihrea1", "cyanide1", "punainen1"),
  Triplet3 = thl_cols("vihrea1", "sininen2", "orange1"),
  Triplet4 = thl_cols("vihrea1", "cyanide1", "orange1"),
  Triplet5 = thl_cols("vihrea1", "sininen2", "roosa"),
  Triplet6 = thl_cols("sininen2", "cyanide1", "roosa"),
  `BuRd` = thl_cols("sininen2", "sininen4", "sininen5",
                    "harmaa", "punainen3", "punainen2", "punainen1"),
  `BuOr` = thl_cols("sininen2", "sininen4", "sininen5",
                    "harmaa", "orange3", "orange2", "orange1"),
  `CyRd` = thl_cols("cyanide1", "cyanide2", "cyanide3", "harmaa",
                    "punainen3", "punainen2", "punainen1"),
  `CyOr` = thl_cols("cyanide1", "cyanide2", "cyanide3", "harmaa",
                    "orange3", "orange2", "orange1"),
  `GrRd` = thl_cols("vihrea1", "GrBu3", "GrBu4", "harmaa",
                    "punainen3", "punainen2", "punainen1"),
  `GrOr` = thl_cols("vihrea1", "GrBu3", "GrBu4", "harmaa",
                    "orange3", "orange2", "orange1"),
  `Greens`  = thl_cols("haivutus1", "vihrea1"),
  `Greens2` = thl_cols("haivutus2", "vihrea1"),
  `Blues`   = thl_cols("haivutus1", "sininen1"),
  `Blues2`  = thl_cols("haivutus2", "sininen2"),
  `Reds`    = thl_cols("haivutus1", "punainen1"),
  `Reds2`   = thl_cols("haivutus2", "punainen1"),
  `Turqoises`  = thl_cols("turkoosi1", "turkoosi2"),
  `Turquoises` = thl_cols("turkoosi1", "turkoosi2")
)

#' @export
thl_pal <- function(palette = "Set1", reverse = FALSE, ..., direction = 1) {
  pal <- thl_palettes[[palette]]

  stopifnot(abs(direction) == 1)
  if (reverse || (direction == -1)) pal <- rev(pal)

  if (is_qualitative(palette)) {
    simple_pal(pal, default = thl_cols("puuttuva"))
  } else {
    colorRampPalette(pal, ...)
  }
}

is_qualitative <- function(palette) {
  grepl("set|paired|triplet", palette, ignore.case = TRUE)
}

simple_pal <- function(values, default = NA) {
  force(values)
  force(default)

  function(n) {
    cols <- unname(values)[seq_len(n)]
    replace(cols, is.na(cols), default)
  }
}

#' @export
scale_color_thl <- function(palette = "Set1", discrete = TRUE,
                            reverse = FALSE, ...,
                            na.value = thl_cols("puuttuva"),
                            direction = 1) {
  pal <- thl_pal(palette = palette, reverse = reverse, direction = direction)

  if (discrete) {
    discrete_scale("colour", paste0("thl_", palette), palette = pal, ...,
                   na.value = na.value)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' @export
scale_fill_thl <- function(palette = "Set1", discrete = TRUE,
                           reverse = FALSE, ...,
                           na.value = thl_cols("puuttuva"),
                           direction = 1) {
  pal <- thl_pal(palette = palette, reverse = reverse, direction = direction)

  if (discrete) {
    discrete_scale("fill", paste0("thl_", palette), palette = pal, ...,
                   na.value = na.value)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

#' @export
thl_green <- "#519b2f"

#' Plot the colors in THL palettes
#' @export
display_thl_pal <- function() {
  thl_palettes %>%
    purrr::map(tibble::enframe) %>%
    bind_rows(.id = "palette") %>%
    group_by(palette = palette %>%
               forcats::fct_inorder() %>%
               forcats::fct_rev()) %>%
    mutate(i = row_number()) %>%
    ggplot(aes(i, palette, fill = value)) +
      scale_fill_identity() +
      geom_tile() +
      theme_void() +
      theme(
        axis.text.y = element_text(),
        plot.margin = margin(1, 1, 1, 1, unit = "line")
      )
}

show_cols <- function(colors) {
  n <- length(colors)
  ggplot() +
    geom_tile(aes(1:n, 1, fill = colors)) +
    scale_fill_identity() +
    theme_void()
}
