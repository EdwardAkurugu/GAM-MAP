
#===============================================
# PLOTTING THEMES
#===============================================
theme_ts_decompose <- function(base_size = 12) {
  theme_classic(base_size = base_size) +
    theme(
      axis.title.y = element_text(face = "bold", size = base_size),
      axis.text.y  = element_text(face = "bold", size = base_size),
      axis.title.x = element_text(face = "bold"),
      axis.text.x  = element_text(face = "bold", angle = 0, hjust = 1, size = base_size),
      panel.background = element_rect(fill = "#f0f0f0")
    )
}

theme_seasonality <- function(base_size = 12) {
  theme_classic(base_size = base_size) +
    theme(
      axis.title.y = element_text(face = "bold", size = base_size),
      axis.text.y  = element_text(face = "bold", size = base_size),
      axis.title.x = element_text(face = "bold"),
      axis.text.x  = element_text(face = "bold", angle = 0, hjust = 1, size = base_size),
      panel.background = element_rect(fill = "#f0f0f0")
    )
}


theme_residuals <- function(base_size = 12) {
  theme_classic(base_size = base_size) +
    theme(
      axis.title.y = element_text(face = "bold", size = base_size),
      axis.text.y  = element_text(face = "bold", size = base_size),
      axis.title.x = element_text(face = "bold"),
      axis.text.x  = element_text(face = "bold", angle = 0, hjust = 1, size = base_size),
      panel.background = element_rect(fill = "#f0f0f0"),
      legend.position = "none"
    )
}


theme_gam_plots <- function(base_size = 12) {
  theme_classic(base_size = base_size) +
    theme(
      axis.title.y = element_text(face = "bold", size = base_size),
      axis.text.y  = element_text(face = "bold", size = base_size),
      axis.title.x = element_text(face = "bold"),
      axis.text.x  = element_text(face = "bold", angle = 0, hjust = 1, size = base_size),
      panel.background = element_rect(fill = "#f0f0f0")
    )
}


theme_app_series <- function(base_size = 12) {
  theme_classic(base_size = base_size) +
    theme(
      axis.title.y = element_text(face = "bold", size = base_size),
      axis.text.y  = element_text(face = "bold", size = base_size),
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x  = element_blank(),
      panel.background = element_rect(fill = "#f0f0f0"),
      legend.position = "none"
    )
}

theme_heatmap1 <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(
      axis.title.y  = element_text(face = "bold", size = base_size),
      axis.text.y   = element_text(face = "bold", size = base_size),
      axis.title.x  = element_blank(),
      axis.text.x   = element_blank(),
      axis.ticks.x  = element_blank(),
      panel.background = element_rect(fill = "#f0f0f0"),
      legend.key.height = unit(1.3, "cm"),
      legend.title  = element_text(face = "bold", angle = 90, hjust = 0,
                                   margin = margin(t = 20, r = -50, b = -150, l = 0)),  
      legend.text   = element_text(size = base_size)   
    )
}



theme_heatmap2 <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(
      axis.title.y  = element_text(face = "bold", size = base_size),
      axis.text.y   = element_text(face = "bold", size = base_size),
      axis.title.x  = element_blank(),
      axis.text.x   = element_blank(),
      axis.ticks.x  = element_blank(),
      panel.background = element_rect(fill = "#f0f0f0"),
      legend.key.height = unit(1.3, "cm"),
      legend.title  = element_text(face = "bold", angle = 90, hjust = 0,
                                   margin = margin(t = 20, r = -50, b = -180, l = 8.5)),  
      legend.text   = element_text(size = base_size)   
    )
}


theme_heatmap3 <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(
      axis.title.y  = element_text(face = "bold", size = base_size),
      axis.text.y   = element_text(face = "bold", size = base_size),
      axis.title.x  = element_blank(),
      axis.text.x   = element_text(face = "bold", size = 10),  #12.5
      axis.ticks = element_line(color = "black"),
      panel.background = element_rect(fill = "#f0f0f0"),
      legend.key.height = unit(1.1, "cm"),
      legend.title  = element_text(face = "bold", angle = 90, hjust = 0,
                                   margin = margin(t = 30, r = -50, b = -120, l = 5)),  
      legend.text   = element_text(size = base_size)   
    )
}





