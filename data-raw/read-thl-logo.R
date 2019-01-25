library(magrittr)

thl_logo <- here::here("data-raw", "short_thl_logo.pdf") %>%
  magick::image_read_pdf(density = 36) %>%
  as.raster()

devtools::use_data(thl_logo, internal = TRUE, overwrite = TRUE)
