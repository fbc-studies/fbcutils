legend_skipper <- function(ggob, value = NULL, label = NULL, dodge = 0.9, kerroin = 0.95, 
                           size = 10, angle = 90, color = NULL, hjust = 1) {
  
  col <- ggob$mapping$x
  measure <- ggob$mapping$y
  
  pohja <- ggob$data %>% 
    mutate(kerroin = max(!!measure) - kerroin * max(!!measure)) %>% 
    mutate(y = !!measure - kerroin)
  
  if (missing(color)) {
    pohja <- pohja %>% 
      arrange(!!col) %>% 
      mutate(color = ggplot_build(ggob)$data[[1]]$fill) %>% 
      group_by(!!col) %>% 
      mutate(color = lag(color, default = tail(color, 1))) %>% 
      ungroup()
  } 
  
  if (!missing(value)) {
    pohja <- pohja %>% 
      filter(str_detect(!!col, str_c(value, collapse = "|")))
  }
  
  if (missing(color)) {
    suppressWarnings(color <- pohja$color)
  }
  
  if (nrow(pohja) == 0) {
    stop(paste("No values found for", str_c(value, collapse = ", ")))
  }
  
  if (missing(label)) {
    pohja <- pohja %>%
      mutate(fill_var = !!ggob$mapping[[3]])
  } else {
    label <- enquo(label)
    
    pohja <- pohja %>% 
      mutate(fill_var = !!label)
  }
  
  list(geom_text(data = pohja, aes_(y = ~ y, label = ~ fill_var),
                 position = position_dodge(width = dodge),
                 size = size, 
                 angle = angle,
                 color = color,
                 hjust = hjust),
       theme(legend.position = "none"))
}
