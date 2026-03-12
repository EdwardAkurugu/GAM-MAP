
# Static Plot Function 
make_static_plotly <- function(p, filename = NULL) {
  ggplotly(p) %>%
    style(hoverinfo = "none") %>%
    layout(
      xaxis = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE)
    ) %>%
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        "zoom2d", "pan2d", "select2d", "lasso2d",
        "zoomIn2d", "zoomOut2d", "autoScale2d",
        "hoverClosestCartesian", "hoverCompareCartesian",
        "sendDataToCloud", "toggleSpikelines"
      ),
      toImageButtonOptions = list(
        format = "png",
        filename = filename
      ),
      displaylogo = FALSE
    )
}
