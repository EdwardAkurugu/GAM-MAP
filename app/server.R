

# ==========================================================================
# SERVER
# ==========================================================================
server <- function(input, output, session) {
  session$onFlushed(function() {
    shinyjs::hide("loading-overlay")
  }, once = TRUE)
  
  # ============================================
  # DESCRIPTIVES 
  # ============================================
  # Descriptive statistics function 
  descriptive_stats <- function(df) {
    min_val  <- round(min(df,  na.rm = TRUE), 2)
    max_val  <- round(max(df,  na.rm = TRUE), 2)
    mean_val <- round(mean(df, na.rm = TRUE), 2)
    sum_val  <- round(sum(df,  na.rm = TRUE), 2)
    sd_val   <- round(sd(df,   na.rm = TRUE), 2)
    cv_val   <- round(((sd_val / mean_val) * 100), 2)
    return(c(min = min_val, max = max_val, mean = mean_val, sum = sum_val, sd = sd_val, cv = cv_val))
  }
  
  # ============================================
  # ANNUAL DESCRIPTIVE STATISTICS 
  # ============================================
  filtered_descriptive_data_annual <- reactive({
    result <- data[data$year == input$year & data$region == input$desc_region, ]
    
    return(result)
  })
  
  # Calculate statistics across the 12 months of the selected year
  stats_annual <- reactive({
    selected_data <- filtered_descriptive_data_annual()
    
    # Calculate statistics for each variable across the 12 monthly observations
    stats_list <- list()
    
    # For malaria cases: show monthly distribution + annual total
    uncom_stats <- descriptive_stats(selected_data$uncom)
    uncom_stats["sum"] <- sum(selected_data$uncom, na.rm = TRUE)  # Annual total
    stats_list$uncom <- uncom_stats
    
    # For minimum temperature: show range across months
    mintem_stats <- descriptive_stats(selected_data$mintem)
    mintem_stats["sum"] <- NA  # Sum doesn't make sense for temperature
    stats_list$mintem <- mintem_stats
    
    # For average temperature: show range across months
    avgtemp_stats <- descriptive_stats(selected_data$avgtemp)
    avgtemp_stats["sum"] <- NA  # Sum doesn't make sense for temperature
    stats_list$avgtemp <- avgtemp_stats
    
    # For maximum temperature: show range across months
    maxtem_stats <- descriptive_stats(selected_data$maxtem)
    maxtem_stats["sum"] <- NA  # Sum doesn't make sense for temperature
    stats_list$maxtem <- maxtem_stats
    
    # For rainfall: show monthly distribution + annual total
    rainfall_stats <- descriptive_stats(selected_data$rainfall)
    rainfall_stats["sum"] <- sum(selected_data$rainfall, na.rm = TRUE)  # Annual total
    stats_list$rainfall <- rainfall_stats
    
    return(do.call(cbind, stats_list))
  })
  
  # Convert to data frame for display
  reactive_stats_df_annual <- reactive({
    df <- as.data.frame(t(stats_annual()))
    colnames(df) <- c("Min", "Max", "Mean", "Sum/Total", "SD", "CV(%)")
    rownames(df) <- c("Uncomplicated Malaria", "Minimum Temperature", "Average Temperature",
                      "Maximum Temperature", "Rainfall")
    df <- cbind(Variables = rownames(df), df)
    
    # Format all numeric columns to 2 decimal places before replacing NA
    numeric_cols <- c("Min", "Max", "Mean", "Sum/Total", "SD", "CV(%)")
    for (col in numeric_cols) {
      df[[col]] <- ifelse(is.na(df[[col]]), NA, sprintf("%.2f", as.numeric(df[[col]])))
    }
    
    # Replace NA with dash AFTER formatting
    df[is.na(df)] <- "—"
    
    return(df)
  })
  
  
  # ============================================
  # OVERALL DESCRIPTIVE STATISTICS 
  # ============================================
  filtered_descriptive_data_overall <- reactive({
    year_start <- input$year_range[1]
    year_end   <- input$year_range[2]
    
    region_data <- data[data$region == input$desc_region &
                          data$year >= year_start &
                          data$year <= year_end, ]
    
    return(region_data)
  })
  
  # Calculate statistics using monthly data (only sum malaria & rainfall)
  stats_overall <- reactive({
    selected <- filtered_descriptive_data_overall()
    
    stats_list <- list()
    
    # MALARIA 
    uncom_stats <- descriptive_stats(selected$uncom)
    uncom_stats["sum"] <- sum(selected$uncom, na.rm = TRUE)
    stats_list$uncom <- uncom_stats
    
    # MIN TEMP 
    mintem_stats <- descriptive_stats(selected$mintem)
    mintem_stats["sum"] <- NA
    stats_list$mintem <- mintem_stats
    
    # AVG TEMP 
    avgtemp_stats <- descriptive_stats(selected$avgtemp)
    avgtemp_stats["sum"] <- NA
    stats_list$avgtemp <- avgtemp_stats
    
    # MAX TEMP 
    maxtem_stats <- descriptive_stats(selected$maxtem)
    maxtem_stats["sum"] <- NA
    stats_list$maxtem <- maxtem_stats
    
    # RAINFALL 
    rainfall_stats <- descriptive_stats(selected$rainfall)
    rainfall_stats["sum"] <- sum(selected$rainfall, na.rm = TRUE)
    stats_list$rainfall <- rainfall_stats
    
    return(do.call(cbind, stats_list))
  })
  
  reactive_stats_df_overall <- reactive({
    df <- as.data.frame(t(stats_overall()))
    colnames(df) <- c("Min", "Max", "Mean", "Sum/Total", "SD", "CV(%)")
    rownames(df) <- c("Uncomplicated Malaria", "Minimum Temperature",
                      "Average Temperature", "Maximum Temperature", "Rainfall")
    df <- cbind(Variables = rownames(df), df)
    
    # Format all numeric columns to 2 decimal places before replacing NA
    numeric_cols <- c("Min", "Max", "Mean", "Sum/Total", "SD", "CV(%)")
    for (col in numeric_cols) {
      df[[col]] <- ifelse(is.na(df[[col]]), NA, sprintf("%.2f", as.numeric(df[[col]])))
    }
    
    # Replace NA with dash AFTER formatting
    df[is.na(df)] <- "—"
    
    return(df)
  })
  
  
  # ===========================================================
  # RENDER TABLES - ANNUAL AND OVERALL DESCRIPTIVE STATISTICS
  # ===========================================================
  output$statsTableAnnual <- DT::renderDataTable({
    year_selected <- input$year   # capture selected year
    
    DT::datatable(
      reactive_stats_df_annual(),
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 10, 20),
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 1:6)
        )
      ),
      rownames = FALSE,
      caption = tags$caption(
        style = "caption-side: bottom; text-align: left; margin: -5px 0;",
        tags$span(
          style = "color: black; font-weight: bold; font-style: italic;",
          paste0("Footnote: Statistics describe monthly values for ", year_selected,
                 ". Sum/Total shows annual aggregate for malaria cases and rainfall.",
                 " Min = Minimum, Max = Maximum, SD = Standard deviation, CV = Coefficient of variation."
          )
        )
      ),
      
      callback = JS(
        sprintf(
          "table.on('init.dt', function () {
           $('<tr><th colspan=\"7\" style=\"border-top: 0px solid black; padding-bottom: 0;\">Table 2: Annual Descriptive Statistics of Malaria and Climate Variables (%s)</th></tr>').insertBefore($('#statsTableAnnual thead tr:first'));
         });", year_selected
        )
      )
    )
  })
  
  
  output$statsTableOverall <- DT::renderDataTable({
    DT::datatable(
      reactive_stats_df_overall(),
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 10, 20),
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 1:6)
        )
      ),
      rownames = FALSE,
      caption = tags$caption(
        style = "caption-side: bottom; text-align: left; margin: -5px 0;",
        tags$span(
          style = "color: black; font-weight: bold; font-style: italic;",
          paste0(
            "Footnote: Statistics describe annual aggregates across ",
            input$year_range[1], "-", input$year_range[2],
            ". Min/Max show the lowest/highest annual values. Mean shows average annual value. ",
            "Sum/Total shows cumulative total. Min = Minimum, Max = Maximum, SD = Standard deviation, CV = Coefficient of variation."
          )
        )
      ),
      callback = JS(
        sprintf(
          "table.on('init.dt', function () {
           $('<tr><th colspan=\"7\" style=\"border-top: 0px solid black; padding-bottom: 0;\">Table 3: Overall Descriptive Statistics of Malaria and Climate Variables (%s-%s)</th></tr>').insertBefore($('#statsTableOverall thead tr:first'));
         });", input$year_range[1], input$year_range[2]
        )
      )
    )
  })
  
  
  # ============================================
  # TIME SERIES DECOMPOSITION 
  # ============================================
  filtered_data <- reactive({
    data %>%
      dplyr::filter(region == input$ts_region,
                    date   >= input$dateRange[1],
                    date   <= input$dateRange[2])
  })
  
  malaria_ts <- reactive({
    ts(filtered_data()$uncom,
       frequency = 12,
       start = c(lubridate::year(min(filtered_data()$date)),
                 lubridate::month(min(filtered_data()$date))))
  })
  
  # Decompose the time series
  malaria_decomposed <- reactive({
    stl(malaria_ts(), s.window = "periodic")
  })
  
  # Extract the decomposed components
  decomposed_df <- reactive({
    trend <- malaria_decomposed()$time.series[, "trend"]
    seasonal <- malaria_decomposed()$time.series[, "seasonal"]
    remainder <- malaria_decomposed()$time.series[, "remainder"]

    # Create a data frame with the decomposed components
    data.frame(
      date = seq.Date(from = min(filtered_data()$date), to = max(filtered_data()$date), by = "month"),
      trend = as.vector(trend),
      seasonal = as.vector(seasonal),
      remainder = as.vector(remainder),
      observed = as.vector(malaria_ts())
    )
  })
  
  #Observed Component
  output$observed_plot <- renderPlotly({
    plot <- ggplot(decomposed_df(), aes(x = date, y = observed/1000)) +
      geom_line(color = "black") +
      labs(x = "", y = "Observed(x10\u00b3)") +
      scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
      theme_ts_decompose()

    make_static_plotly(plot,
                       filename = paste0("observed_", input$ts_region))
  })
  
  # Trend Component
  output$trend_plot <- renderPlotly({
    plot <- ggplot(decomposed_df(), aes(x = date, y = trend/1000)) +
      geom_line(color = "#06752b") +
      labs(x = "", y = "Trend(x10\u00b3)") +
      scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
      theme_ts_decompose()
    
    make_static_plotly(plot,
                       filename = paste0("trend_", input$ts_region))
  })
  
  # Seasonal Component
  output$seasonal_plot <- renderPlotly({
    plot <- ggplot(decomposed_df(), aes(x = date, y = seasonal/1000)) +
      geom_line(color = "#2c63ab") +
      labs(x = "", y = "Seasonal(x10\u00b3)") +
      scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
      theme_ts_decompose()
    
    make_static_plotly(plot,
                       filename = paste0("seasonal_", input$ts_region))
  })
  
  # Remainder Component
  output$remainder_plot <- renderPlotly({
    plot <- ggplot(decomposed_df(), aes(x = date, y = remainder/1000)) +
      geom_line(color = "#00AFBB") +
      labs(x = "", y = "Remainder(x10\u00b3)") +
      scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
      theme_ts_decompose()
    
    make_static_plotly(plot,
                       filename = paste0("remainder_", input$ts_region))
  })
  
  
  # ============================================
  # SEASONALITY 
  # ============================================
  data_filtered <- reactive({
    year_range <- as.numeric(format(input$dateRange, "%Y"))
    region_selected <- input$season_region
    
    data %>%
      filter(year >= year_range[1] & year <= year_range[2],
             region == region_selected)
    
  })
  
  # Malaria Boxplot
  output$boxplot1 <- renderPlotly({
    data_filtered_month <- data_filtered() %>%
      mutate(
        month = factor(month, levels = month.abb),
        uncom_scaled = uncom / 1000
      )
    
    plot <- ggplot(data_filtered_month, aes(x = month, y = uncom_scaled)) +
      geom_boxplot() +
      labs(x = "", y = "Uncomplicated Malaria(x10\u00b3)") +
      theme_seasonality()
    
    make_static_plotly(plot,
                       filename = paste0("boxplot1_", input$season_region))

  })
  
  # Minimum Temperature Boxplot
  output$boxplot2 <- renderPlotly({
    data_filtered_month <- data_filtered() %>%
      mutate(month = factor(month, levels = month.abb))
    
    plot <- ggplot(data_filtered_month, aes(x = month, y = mintem)) +
      geom_boxplot(color = "#FF6664") +
      labs(x = "", y = "Minimum Temperature(°C)") +
      theme_seasonality()
    
    make_static_plotly(plot,
                       filename = paste0("boxplot2_", input$season_region))
  })
  
  # Average Temperature Boxplot
  output$boxplot3 <- renderPlotly({
    data_filtered_month <- data_filtered() %>%
      mutate(month = factor(month, levels = month.abb))
    
    plot <- ggplot(data_filtered_month, aes(x = month, y = avgtemp, color = region)) +
      geom_boxplot(color = "steelblue") +
      labs(x = "", y = "Average Temperature(°C)") +
      theme_seasonality()
    
    make_static_plotly(plot,
                       filename = paste0("boxplot3_", input$season_region))
  })
  
  # Maximum Temperature Boxplot
  output$boxplot4 <- renderPlotly({
    data_filtered_month <- data_filtered() %>%
      mutate(month = factor(month, levels = month.abb))
    
    plot <- ggplot(data_filtered_month, aes(x = month, y = maxtem)) +
      geom_boxplot(color = "orange") +
      labs(x = "", y = "Maximum Temperature(°C)") +
      theme_seasonality()
    
    make_static_plotly(plot,
                       filename = paste0("boxplot4_", input$season_region))
  })
  
  # Rainfall Temperature Boxplot
  output$boxplot5 <- renderPlotly({
    data_filtered_month <- data_filtered() %>%
      mutate(month = factor(month, levels = month.abb))
    
    plot <- ggplot(data_filtered_month, aes(x = month, y = rainfall)) +
      geom_boxplot(color = "#00AFBB") +
      labs(x = "", y = "Rainfall(mm)") +
      theme_seasonality()
    
    make_static_plotly(plot,
                       filename = paste0("boxplot5_", input$season_region))
  })
  
  #============================================
  # TEST OF SEASONALITY
  #============================================
  output$results_table <- DT::renderDataTable({
    filtered_data <- data %>%
      filter(region == input$season_region,
             date >= input$dateRange[1],
             date <= input$dateRange[2])
    
    # Kruskal-Wallis test
    kruskal_result <- kruskal.test(uncom ~ months, data = filtered_data)
    df_kruskal <- length(unique(filtered_data$months)) - 1
    
    # --- Ensure complete block design for Friedman ---
    wide <- tidyr::pivot_wider(filtered_data,
                               id_cols = year,
                               names_from = months,
                               values_from = uncom)
    mat <- as.matrix(wide[,-1])
    friedman_result <- friedman.test(mat)
    df_friedman <- ncol(mat) - 1
    
    # Build results table with renamed columns
    result_table <- data.frame(
      Test = c("Kruskal-Wallis", "Friedman"),
      Statistic = round(c(kruskal_result$statistic, friedman_result$statistic), 2),
      Degrees_of_Freedom = c(df_kruskal, df_friedman),  # renamed column       
      P_Value = sprintf("%.2f", c(kruskal_result$p.value, friedman_result$p.value))
    )
    
    DT::datatable(result_table,
                  options = list(
                    #container=table,
                    pageLength = 10,
                    lengthMenu = c(10, 10, 20),
                    striped = TRUE,
                    hover = TRUE,
                    bordered = TRUE,
                    columnDefs = list(
                      list(className = 'dt-center', targets = 1:3)  # Center columns 2-4 (indices 1-3)
                    ),
                    columns = list(
                      list(title = "Statistic"),
                      list(title = "Chi Square"),
                      list(title = "df"),
                      list(title = "p-value")
                    )
                  ),
                  rownames = FALSE,
                  caption = tags$caption(
                    style = "caption-side: bottom; text-align: left; margin: -5px 0;",
                    tags$span(
                      style = "color: black; font-weight: bold; font-style: italic;",
                      paste0(
                        "Footnote: df = Degrees of freedom. ",
                        "The Kruskal-Wallis test detects differences across independent monthly groups, while the Friedman test accounts for the repeated measures structure ",
                        "(12 months across ", length(unique(filtered_data$year)), " years), making it more appropriate for this data."
                      )
                    )
                  ),
                  callback = JS(
                    "table.on('init.dt', function () {",
                    " $('<tr><th colspan=\"7\" style=\"border-top: 0px solid black; padding-bottom: 0;\">Table 3: Test of Seasonality </th></tr>').insertBefore($('#results_table thead tr:first'));",
                    "});"
                  )
    )
    
  })
  
  
  #============================================
  # REGIONAL GAM FITTING
  #============================================
  
  # ---------- Fitting Upper East (UE) regional models ---------- 
  # model_UE_1 <- gam(uncom ~ s(time, k=13) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Upper East"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_UE_2 <- gam(uncom ~ s(months, k=12) +
  #                     s(rainfall, k=22) + s(avgtemp) +                                
  #                     ti(time, months, k=c(4, 12)) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Upper East"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_UE_3 <- gam(uncom ~ s(time, k=22) + s(rainfall) +
  #                     s(avgtemp) +
  #                     ti(time, months, k=c(4, 12)) + ti(avgtemp,rainfall),    
  #                   data = subset(data, region == "Upper East"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_UE_4 <- gam(uncom ~ s(time, k=11) + s(months) +
  #                     s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Upper East"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_UE_5 <- gam(uncom ~ s(time, k=11) + s(months) +
  #                     s(rainfall) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Upper East"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_UE_6 <- gam(uncom ~ s(time, k=25) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Upper East"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_UE_7 <- gam(uncom ~ s(time, k=11) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months),
  #                   data = subset(data, region == "Upper East"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_UE_8 <- gam(uncom ~ s(time, k=27) + s(months) +
  #                     s(rainfall) + s(avgtemp),
  #                   data = subset(data, region == "Upper East"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # saveRDS(model_UE_1, file = "model_UE_1.rds")
  # saveRDS(model_UE_2, file = "model_UE_2.rds")
  # saveRDS(model_UE_3, file = "model_UE_3.rds")
  # saveRDS(model_UE_4, file = "model_UE_4.rds")
  # saveRDS(model_UE_5, file = "model_UE_5.rds")
  # saveRDS(model_UE_6, file = "model_UE_6.rds")
  # saveRDS(model_UE_7, file = "model_UE_7.rds")
  # saveRDS(model_UE_8, file = "model_UE_8.rds")
  
  model_UE_1 <- readRDS("model_UE_1.rds")
  model_UE_2 <- readRDS("model_UE_2.rds")
  model_UE_3 <- readRDS("model_UE_3.rds")
  model_UE_4 <- readRDS("model_UE_4.rds")
  model_UE_5 <- readRDS("model_UE_5.rds")
  model_UE_6 <- readRDS("model_UE_6.rds")
  model_UE_7 <- readRDS("model_UE_7.rds")
  model_UE_8 <- readRDS("model_UE_8.rds")
  
  
  # ---------- Fitting Upper West (UW) regional models ----------
  # model_UW_1 <- gam(uncom ~ s(time, k=34) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Upper West"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_UW_2 <- gam(uncom ~ s(months, k=12) +
  #                     s(rainfall, k=16) + s(avgtemp) +                      
  #                     ti(time, months, k=c(16,12)) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Upper West"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_UW_3 <- gam(uncom ~ s(time, k=16) + s(rainfall) +                 
  #                     s(avgtemp) +
  #                     ti(time, months, k=c(16, 12)) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Upper West"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_UW_4 <- gam(uncom ~ s(time, k=34) + s(months) +
  #                     s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Upper West"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_UW_5 <- gam(uncom ~ s(time, k=33) + s(months) +     
  #                     s(rainfall) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Upper West"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_UW_6 <- gam(uncom ~ s(time, k=33) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Upper West"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_UW_7 <- gam(uncom ~ s(time, k=34) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months),
  #                   data = subset(data, region == "Upper West"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_UW_8 <- gam(uncom ~ s(time, k=33) + s(months) +
  #                     s(rainfall) + s(avgtemp),
  #                   data = subset(data, region == "Upper West"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  
  # saveRDS(model_UW_1, file = "model_UW_1.rds")
  # saveRDS(model_UW_2, file = "model_UW_2.rds")
  # saveRDS(model_UW_3, file = "model_UW_3.rds")
  # saveRDS(model_UW_4, file = "model_UW_4.rds")
  # saveRDS(model_UW_5, file = "model_UW_5.rds")
  # saveRDS(model_UW_6, file = "model_UW_6.rds")
  # saveRDS(model_UW_7, file = "model_UW_7.rds")
  # saveRDS(model_UW_8, file = "model_UW_8.rds")
  
  model_UW_1 <- readRDS("model_UW_1.rds")
  model_UW_2 <- readRDS("model_UW_2.rds")
  model_UW_3 <- readRDS("model_UW_3.rds")
  model_UW_4 <- readRDS("model_UW_4.rds")
  model_UW_5 <- readRDS("model_UW_5.rds")
  model_UW_6 <- readRDS("model_UW_6.rds")
  model_UW_7 <- readRDS("model_UW_7.rds")
  model_UW_8 <- readRDS("model_UW_8.rds")
  
  
  # ---------- Fitting Northern region (NO) models ----------
  # model_NO_1 <- gam(uncom ~ s(time, k=16) + s(months) +
  #                   s(rainfall) + s(avgtemp) +
  #                   ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Northern"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_NO_2 <- gam(uncom ~ s(months, k=12) +
  #                     s(rainfall, k=16) + s(avgtemp) +
  #                     ti(time, months, k=c(16, 12)) + ti(avgtemp,rainfall),  
  #                   data = subset(data, region == "Northern"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_NO_3 <- gam(uncom ~ s(time, k=16) + s(rainfall) +
  #                     s(avgtemp) +
  #                     ti(time, months, k=c(16, 12)) + ti(avgtemp,rainfall),  
  #                   data = subset(data, region == "Northern"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_NO_4 <- gam(uncom ~ s(time, k=16) + s(months) +
  #                     s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Northern"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_NO_5 <- gam(uncom ~ s(time, k=16) + s(months) +
  #                     s(rainfall) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Northern"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_NO_6 <- gam(uncom ~ s(time, k=25) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Northern"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_NO_7 <- gam(uncom ~ s(time, k=16) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months),
  #                   data = subset(data, region == "Northern"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_NO_8 <- gam(uncom ~ s(time, k=25) + s(months) +
  #                     s(rainfall) + s(avgtemp),
  #                   data = subset(data, region == "Northern"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  
  # saveRDS(model_NO_1, file = "model_NO_1.rds")
  # saveRDS(model_NO_2, file = "model_NO_2.rds")
  # saveRDS(model_NO_3, file = "model_NO_3.rds")
  # saveRDS(model_NO_4, file = "model_NO_4.rds")
  # saveRDS(model_NO_5, file = "model_NO_5.rds")
  # saveRDS(model_NO_6, file = "model_NO_6.rds")
  # saveRDS(model_NO_7, file = "model_NO_7.rds")
  # saveRDS(model_NO_8, file = "model_NO_8.rds")
  
  model_NO_1 <- readRDS("model_NO_1.rds")
  model_NO_2 <- readRDS("model_NO_2.rds")
  model_NO_3 <- readRDS("model_NO_3.rds")
  model_NO_4 <- readRDS("model_NO_4.rds")
  model_NO_5 <- readRDS("model_NO_5.rds")
  model_NO_6 <- readRDS("model_NO_6.rds")
  model_NO_7 <- readRDS("model_NO_7.rds")
  model_NO_8 <- readRDS("model_NO_8.rds")
  
  
  # ---------- Fitting Brong Ahafo region (BA) models ----------
  # model_BA_1 <- gam(uncom ~ s(time, k=17) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Brong Ahafo"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_BA_2 <- gam(uncom ~ s(months, k=12) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months, k=c(16,12)) + ti(avgtemp,rainfall),  
  #                   data = subset(data, region == "Brong Ahafo"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_BA_3 <- gam(uncom ~ s(time) + s(rainfall) +
  #                     s(avgtemp) +
  #                     ti(time, months, k=c(12,12)) + ti(avgtemp,rainfall),      
  #                   data = subset(data, region == "Brong Ahafo"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_BA_4 <- gam(uncom ~ s(time, k=17) + s(months) +
  #                     s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Brong Ahafo"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_BA_5 <- gam(uncom ~ s(time, k=17) + s(months) +
  #                     s(rainfall) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Brong Ahafo"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_BA_6 <- gam(uncom ~ s(time, k=23) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Brong Ahafo"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_BA_7 <- gam(uncom ~ s(time, k=17) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months),
  #                   data = subset(data, region == "Brong Ahafo"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_BA_8 <- gam(uncom ~ s(time, k=23) + s(months) +
  #                     s(rainfall) + s(avgtemp),
  #                   data = subset(data, region == "Brong Ahafo"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  
  # saveRDS(model_BA_1, file = "model_BA_1.rds")
  # saveRDS(model_BA_2, file = "model_BA_2.rds")
  # saveRDS(model_BA_3, file = "model_BA_3.rds")
  # saveRDS(model_BA_4, file = "model_BA_4.rds")
  # saveRDS(model_BA_5, file = "model_BA_5.rds")
  # saveRDS(model_BA_6, file = "model_BA_6.rds")
  # saveRDS(model_BA_7, file = "model_BA_7.rds")
  # saveRDS(model_BA_8, file = "model_BA_8.rds")
  
  model_BA_1 <- readRDS("model_BA_1.rds")
  model_BA_2 <- readRDS("model_BA_2.rds")
  model_BA_3 <- readRDS("model_BA_3.rds")
  model_BA_4 <- readRDS("model_BA_4.rds")
  model_BA_5 <- readRDS("model_BA_5.rds")
  model_BA_6 <- readRDS("model_BA_6.rds")
  model_BA_7 <- readRDS("model_BA_7.rds")
  model_BA_8 <- readRDS("model_BA_8.rds")
  

  # ---------- Fitting Ashanti region (AS) models ----------
  # model_AS_1 <- gam(uncom ~ s(time, k=18) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Ashanti"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_AS_2 <- gam(uncom ~ s(months, k=12) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months, k=c(16,12)) + ti(avgtemp,rainfall),  
  #                   data = subset(data, region == "Ashanti"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_AS_3 <- gam(uncom ~ s(time) + s(rainfall) +
  #                     s(avgtemp) +
  #                     ti(time, months, k=c(22,12)) + ti(avgtemp,rainfall),    
  #                   data = subset(data, region == "Ashanti"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_AS_4 <- gam(uncom ~ s(time, k=18) + s(months) +
  #                     s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Ashanti"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_AS_5 <- gam(uncom ~ s(time, k=17) + s(months) +
  #                     s(rainfall) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Ashanti"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_AS_6 <- gam(uncom ~ s(time, k=28) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Ashanti"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_AS_7 <- gam(uncom ~ s(time, k=18) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months),
  #                   data = subset(data, region == "Ashanti"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_AS_8 <- gam(uncom ~ s(time, k=28) + s(months) +
  #                     s(rainfall) + s(avgtemp),
  #                   data = subset(data, region == "Ashanti"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  
  # saveRDS(model_AS_1, file = "model_AS_1.rds")
  # saveRDS(model_AS_2, file = "model_AS_2.rds")
  # saveRDS(model_AS_3, file = "model_AS_3.rds")
  # saveRDS(model_AS_4, file = "model_AS_4.rds")
  # saveRDS(model_AS_5, file = "model_AS_5.rds")
  # saveRDS(model_AS_6, file = "model_AS_6.rds")
  # saveRDS(model_AS_7, file = "model_AS_7.rds")
  # saveRDS(model_AS_8, file = "model_AS_8.rds")
  
  model_AS_1 <- readRDS("model_AS_1.rds")
  model_AS_2 <- readRDS("model_AS_2.rds")
  model_AS_3 <- readRDS("model_AS_3.rds")
  model_AS_4 <- readRDS("model_AS_4.rds")
  model_AS_5 <- readRDS("model_AS_5.rds")
  model_AS_6 <- readRDS("model_AS_6.rds")
  model_AS_7 <- readRDS("model_AS_7.rds")
  model_AS_8 <- readRDS("model_AS_8.rds")
  
  
  # ---------- Fitting Eastern region (EA) models ----------
  # model_EA_1 <- gam(uncom ~ s(time, k=37) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp, rainfall),
  #                   data = subset(data, region == "Eastern"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_EA_2 <- gam(uncom ~ s(months, k=12) +                                   
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months, k=c(12,12)) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Eastern"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_EA_3 <- gam(uncom ~ s(time, k=25) + s(rainfall) +
  #                     s(avgtemp) +
  #                     ti(time, months)  + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Eastern"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_EA_4 <- gam(uncom ~ s(time, k=25) + s(months) +
  #                     s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp, rainfall),
  #                   data = subset(data, region == "Eastern"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_EA_5 <- gam(uncom ~ s(time, k=21) + s(months) +
  #                     s(rainfall) +
  #                     ti(time, months) + ti(avgtemp, rainfall),
  #                   data = subset(data, region == "Eastern"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_EA_6 <- gam(uncom ~ s(time, k=28) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Eastern"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_EA_7 <- gam(uncom ~ s(time, k=18) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months),
  #                   data = subset(data, region == "Eastern"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_EA_8 <- gam(uncom ~ s(time, k=25) + s(months) +
  #                     s(rainfall) + s(avgtemp),
  #                   data = subset(data, region == "Eastern"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  
  # saveRDS(model_EA_1, file = "model_EA_1.rds")
  # saveRDS(model_EA_2, file = "model_EA_2.rds")
  # saveRDS(model_EA_3, file = "model_EA_3.rds")
  # saveRDS(model_EA_4, file = "model_EA_4.rds")
  # saveRDS(model_EA_5, file = "model_EA_5.rds")
  # saveRDS(model_EA_6, file = "model_EA_6.rds")
  # saveRDS(model_EA_7, file = "model_EA_7.rds")
  # saveRDS(model_EA_8, file = "model_EA_8.rds")
  
  model_EA_1 <- readRDS("model_EA_1.rds")
  model_EA_2 <- readRDS("model_EA_2.rds")
  model_EA_3 <- readRDS("model_EA_3.rds")
  model_EA_4 <- readRDS("model_EA_4.rds")
  model_EA_5 <- readRDS("model_EA_5.rds")
  model_EA_6 <- readRDS("model_EA_6.rds")
  model_EA_7 <- readRDS("model_EA_7.rds")
  model_EA_8 <- readRDS("model_EA_8.rds")
  

  # ---------- Fitting Volta region (VO) models ----------
  # model_VO_1 <- gam(uncom ~ s(time, k=19) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Volta"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_VO_2 <- gam(uncom ~ s(months, k=12) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months, k=c(16,12)) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Volta"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_VO_3 <- gam(uncom ~ s(time, k=19) + s(rainfall) +
  #                     s(avgtemp) +
  #                     ti(time, months, k=c(16,12)) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Volta"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_VO_4 <- gam(uncom ~ s(time, k=18) + s(months) +
  #                     s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Volta"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_VO_5 <- gam(uncom ~ s(time, k=19) + s(months) +
  #                     s(rainfall) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Volta"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_VO_6 <- gam(uncom ~ s(time, k=25) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Volta"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_VO_7 <- gam(uncom ~ s(time, k=19) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months),
  #                   data = subset(data, region == "Volta"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_VO_8 <- gam(uncom ~ s(time, k=25) + s(months) +
  #                     s(rainfall) + s(avgtemp),
  #                   data = subset(data, region == "Volta"),
  #                   family = "quasipoisson", method = "GCV.Cp")
   
  # saveRDS(model_VO_1, file = "model_VO_1.rds")
  # saveRDS(model_VO_2, file = "model_VO_2.rds")
  # saveRDS(model_VO_3, file = "model_VO_3.rds")
  # saveRDS(model_VO_4, file = "model_VO_4.rds")
  # saveRDS(model_VO_5, file = "model_VO_5.rds")
  # saveRDS(model_VO_6, file = "model_VO_6.rds")
  # saveRDS(model_VO_7, file = "model_VO_7.rds")
  # saveRDS(model_VO_8, file = "model_VO_8.rds")
  
  model_VO_1 <- readRDS("model_VO_1.rds")
  model_VO_2 <- readRDS("model_VO_2.rds")
  model_VO_3 <- readRDS("model_VO_3.rds")
  model_VO_4 <- readRDS("model_VO_4.rds")
  model_VO_5 <- readRDS("model_VO_5.rds")
  model_VO_6 <- readRDS("model_VO_6.rds")
  model_VO_7 <- readRDS("model_VO_7.rds")
  model_VO_8 <- readRDS("model_VO_8.rds")
  

  # ---------- Fitting Greater Accra region (GA) models ----------
  # model_GA_1 <- gam(uncom ~ s(time, k=18) + s(months) +
  #                      s(rainfall) + s(avgtemp) +
  #                      ti(time, months) + ti(avgtemp,rainfall),
  #                    data = subset(data, region == "Greater Accra"),
  #                    family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_GA_2 <- gam(uncom ~ s(months) +
  #                      s(rainfall) + s(avgtemp) +
  #                      ti(time, months) + ti(avgtemp,rainfall),
  #                    data = subset(data, region == "Greater Accra"),
  #                    family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_GA_3 <- gam(uncom ~ s(time, k=16) + s(rainfall) +
  #                      s(avgtemp) +                                         
  #                      ti(time, months, k=c(20,12)) + ti(avgtemp,rainfall),
  #                    data = subset(data, region == "Greater Accra"),
  #                    family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_GA_4 <- gam(uncom ~ s(time, k=18) + s(months) +
  #                      s(avgtemp) +
  #                      ti(time, months) + ti(avgtemp,rainfall),
  #                    data = subset(data, region == "Greater Accra"),
  #                    family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_GA_5 <- gam(uncom ~ s(time, k=18) + s(months) +
  #                      s(rainfall) +
  #                      ti(time, months) + ti(avgtemp,rainfall),
  #                    data = subset(data, region == "Greater Accra"),
  #                    family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_GA_6 <- gam(uncom ~ s(time, k=28) + s(months) +
  #                      s(rainfall) + s(avgtemp) +
  #                      ti(avgtemp,rainfall),
  #                    data = subset(data, region == "Greater Accra"),
  #                    family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_GA_7 <- gam(uncom ~ s(time, k=18) + s(months) +
  #                      s(rainfall) + s(avgtemp) +
  #                      ti(time, months),
  #                    data = subset(data, region == "Greater Accra"),
  #                    family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_GA_8 <- gam(uncom ~ s(time, k=28) + s(months) +
  #                      s(rainfall) + s(avgtemp),
  #                    data = subset(data, region == "Greater Accra"),
  #                    family = "quasipoisson", method = "GCV.Cp")
  
  # saveRDS(model_GA_1, file = "model_GA_1.rds")
  # saveRDS(model_GA_2, file = "model_GA_2.rds")
  # saveRDS(model_GA_3, file = "model_GA_3.rds")
  # saveRDS(model_GA_4, file = "model_GA_4.rds")
  # saveRDS(model_GA_5, file = "model_GA_5.rds")
  # saveRDS(model_GA_6, file = "model_GA_6.rds")
  # saveRDS(model_GA_7, file = "model_GA_7.rds")
  # saveRDS(model_GA_8, file = "model_GA_8.rds")
  
  model_GA_1 <- readRDS("model_GA_1.rds")
  model_GA_2 <- readRDS("model_GA_2.rds")
  model_GA_3 <- readRDS("model_GA_3.rds")
  model_GA_4 <- readRDS("model_GA_4.rds")
  model_GA_5 <- readRDS("model_GA_5.rds")
  model_GA_6 <- readRDS("model_GA_6.rds")
  model_GA_7 <- readRDS("model_GA_7.rds")
  model_GA_8 <- readRDS("model_GA_8.rds")
  

  # ---------- Fitting Central region (CE) models ----------
  # model_CE_1 <- gam(uncom ~ s(time, k=18) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Central"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_CE_2 <- gam(uncom ~ s(months, k=12) +
  #                   s(rainfall) + s(avgtemp) +
  #                   ti(time, months, k=c(16,12)) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Central"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_CE_3 <- gam(uncom ~ s(time, k=18) + s(rainfall) +                     
  #                     s(avgtemp) +
  #                     ti(time, months, k=c(12, 12)) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Central"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_CE_4 <- gam(uncom ~ s(time, k=18) + s(months) +
  #                     s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Central"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_CE_5 <- gam(uncom ~ s(time, k=18) + s(months) +
  #                     s(rainfall) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Central"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_CE_6 <- gam(uncom ~ s(time, k=27) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Central"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_CE_7 <- gam(uncom ~ s(time, k=18) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months),
  #                   data = subset(data, region == "Central"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_CE_8 <- gam(uncom ~ s(time, k=27) + s(months) +
  #                   s(rainfall) + s(avgtemp),
  #                   data = subset(data, region == "Central"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  
  # saveRDS(model_CE_1, file = "model_CE_1.rds")
  # saveRDS(model_CE_2, file = "model_CE_2.rds")
  # saveRDS(model_CE_3, file = "model_CE_3.rds")
  # saveRDS(model_CE_4, file = "model_CE_4.rds")
  # saveRDS(model_CE_5, file = "model_CE_5.rds")
  # saveRDS(model_CE_6, file = "model_CE_6.rds")
  # saveRDS(model_CE_7, file = "model_CE_7.rds")
  # saveRDS(model_CE_8, file = "model_CE_8.rds")
  
  model_CE_1 <- readRDS("model_CE_1.rds")
  model_CE_2 <- readRDS("model_CE_2.rds")
  model_CE_3 <- readRDS("model_CE_3.rds")
  model_CE_4 <- readRDS("model_CE_4.rds")
  model_CE_5 <- readRDS("model_CE_5.rds")
  model_CE_6 <- readRDS("model_CE_6.rds")
  model_CE_7 <- readRDS("model_CE_7.rds")
  model_CE_8 <- readRDS("model_CE_8.rds")
  

  # ---------- Fitting Western region (WE) models ----------
  # model_WE_1 <- gam(uncom ~ s(time, k=19) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Western"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_WE_2 <- gam(uncom ~ s(months, k=12) +                                 
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months, k=c(16,12)) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Western"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_WE_3 <- gam(uncom ~ s(time, k=12) + s(rainfall) +                   
  #                     s(avgtemp) +
  #                     ti(time, months, k=c(16,12)) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Western"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_WE_4 <- gam(uncom ~ s(time, k=17) + s(months) +
  #                     s(avgtemp) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Western"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_WE_5 <- gam(uncom ~ s(time, k=22) + s(months) +
  #                     s(rainfall) +
  #                     ti(time, months) + ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Western"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_WE_6 <- gam(uncom ~ s(time, k=22) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(avgtemp,rainfall),
  #                   data = subset(data, region == "Western"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_WE_7 <- gam(uncom ~ s(time, k=19) + s(months) +
  #                     s(rainfall) + s(avgtemp) +
  #                     ti(time, months),
  #                   data = subset(data, region == "Western"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  # 
  # model_WE_8 <- gam(uncom ~ s(time, k=25) + s(months) +
  #                     s(rainfall) + s(avgtemp),
  #                   data = subset(data, region == "Western"),
  #                   family = "quasipoisson", method = "GCV.Cp")
  
  # saveRDS(model_WE_1, file = "model_WE_1.rds")
  # saveRDS(model_WE_2, file = "model_WE_2.rds")
  # saveRDS(model_WE_3, file = "model_WE_3.rds")
  # saveRDS(model_WE_4, file = "model_WE_4.rds")
  # saveRDS(model_WE_5, file = "model_WE_5.rds")
  # saveRDS(model_WE_6, file = "model_WE_6.rds")
  # saveRDS(model_WE_7, file = "model_WE_7.rds")
  # saveRDS(model_WE_8, file = "model_WE_8.rds")
  
  model_WE_1 <- readRDS("model_WE_1.rds")
  model_WE_2 <- readRDS("model_WE_2.rds")
  model_WE_3 <- readRDS("model_WE_3.rds")
  model_WE_4 <- readRDS("model_WE_4.rds")
  model_WE_5 <- readRDS("model_WE_5.rds")
  model_WE_6 <- readRDS("model_WE_6.rds")
  model_WE_7 <- readRDS("model_WE_7.rds")
  model_WE_8 <- readRDS("model_WE_8.rds")
  
  
  #============================================
  # REGIONAL GAM ESTIMATES
  #============================================
  observeEvent(input$model_region, {
    region <- input$model_region
    
    #print(region)      #Debugging
    
    models <- switch(region,
                     "Upper East"    = list(model_UE_1, model_UE_2, model_UE_3, model_UE_4, model_UE_5, model_UE_6, model_UE_7, model_UE_8), 
                     "Upper West"    = list(model_UW_1, model_UW_2, model_UW_3, model_UW_4, model_UW_5, model_UW_6, model_UW_7, model_UW_8),
                     "Northern"      = list(model_NO_1, model_NO_2, model_NO_3, model_NO_4, model_NO_5, model_NO_6, model_NO_7, model_NO_8),
                     "Brong Ahafo"   = list(model_BA_1, model_BA_2, model_BA_3, model_BA_4, model_BA_5, model_BA_6, model_BA_7, model_BA_8),
                     "Ashanti"       = list(model_AS_1, model_AS_2, model_AS_3, model_AS_4, model_AS_5, model_AS_6, model_AS_7, model_AS_8),  
                     "Eastern"       = list(model_EA_1, model_EA_2, model_EA_3, model_EA_4, model_EA_5, model_EA_6, model_EA_7, model_EA_8),
                     "Volta"         = list(model_VO_1, model_VO_2, model_VO_3, model_VO_4, model_VO_5, model_VO_6, model_VO_7, model_VO_8),
                     "Greater Accra" = list(model_GA_1, model_GA_2, model_GA_3, model_GA_4, model_GA_5, model_GA_6, model_GA_7, model_GA_8),
                     "Central"       = list(model_CE_1, model_CE_2, model_CE_3, model_CE_4, model_CE_5, model_CE_6, model_CE_7, model_CE_8),
                     "Western"       = list(model_WE_1, model_WE_2, model_WE_3, model_WE_4, model_WE_5, model_WE_6, model_WE_7, model_WE_8)       
    )
    
    # print(models)   ##Debugging
    
    model_metrics <- data.frame(
      Model = paste0("Model ", 1:length(models)),
      GCV = sapply(models, function(model) sprintf("%.2f", round(model$gcv.ubre, 2))),
      Adj_R2_num = sapply(models, function(model) round(summary(model)$r.sq * 100, 2)),     
      Dev_Explained_num = sapply(models, function(model) round(summary(model)$dev.expl * 100, 2))  
    )
    
    # Use numeric columns for best model selection
    best_model_idx <- which.max(model_metrics$Adj_R2_num * model_metrics$Dev_Explained_num)
    best_model <- model_metrics[best_model_idx, ]
    model_metrics$Best_Model <- ifelse(model_metrics$Model == best_model$Model, "Yes", "No")
    
    # Now format for display (after which.max is done)
    model_metrics$Adj_R2 <- sprintf("%.2f", model_metrics$Adj_R2_num)
    model_metrics$Dev_Explained <- sprintf("%.2f", model_metrics$Dev_Explained_num)
    
    # Drop the numeric helper columns before rendering
    model_metrics$Adj_R2_num <- NULL
    model_metrics$Dev_Explained_num <- NULL
    
    # Reorder columns
    model_metrics <- model_metrics[, c("Model", "GCV", "Adj_R2", "Dev_Explained", "Best_Model")]
    
    output$model_metrics <- DT::renderDataTable({
      DT::datatable(model_metrics,
                    options = list(
                      striped = TRUE,
                      hover = TRUE,
                      bordered = TRUE,
                      columns = list(
                        list(title = "Models"),
                        list(title = "GCV", className = 'dt-center'),
                        list(title = "Adj R²", className = 'dt-center'),
                        list(title = "Dev Explained", className = 'dt-center'),
                        list(title = "Best Model", className = 'dt-center')
                      )
                    ),
                    rownames = FALSE,
                    caption = tags$caption(
                      style = "caption-side: bottom; text-align: left; margin: -5px 0;",
                      tags$span(
                        style = "color: black; font-weight: bold; font-style: italic;",
                        "Footnote: Adj R² = Adjusted R Squared; Dev Explained = Deviance Explained; GCV = Generalized Cross-Validation score (lower is better)"
                      )
                    ),
                    callback = JS(
                      "table.on('init.dt', function () {",
                      " $('<tr><th colspan=\"7\" style=\"border-top: 0px solid black; padding-bottom: 0;\">Table 6: Model Performance Metrics of the GAM </th></tr>').insertBefore($('#model_metrics thead tr:first'));",
                      "});"
                    )
      )
    })
    
    
    output$best_model_parametric_summary <- renderPrint({
      # Check if the model exists
      if (is.null(models[[best_model_idx]])) {
        return(NULL)
      }
      
      # Return the summary of the best model
      return(summary(models[[best_model_idx]]))
    })
    
    
    output$best_model_smooth_terms <- DT::renderDataTable({
      # --- Smooth terms ---
      smooth_terms_table <- as.data.frame(round(summary(models[[best_model_idx]])$s.table, 2))
      smooth_terms_table <- cbind(Parameters = row.names(smooth_terms_table), smooth_terms_table)
      
      # --- Parametric coefficients ---
      param_table <- as.data.frame(round(summary(models[[best_model_idx]])$p.table, 2))
      param_table <- cbind(Parameters = row.names(param_table), param_table)
      
      # Keep only intercept row
      intercept_row <- param_table[param_table$Parameters == "(Intercept)", ]
      
      # --- Standardize column names ---
      colnames(intercept_row) <- c("Parameters", "Col2", "Col3", "Col4", "Col5")
      colnames(smooth_terms_table) <- c("Parameters", "Col2", "Col3", "Col4", "Col5")
      
      # Format numeric values
      intercept_row[] <- lapply(intercept_row, function(x) if(is.numeric(x)) format(x, nsmall = 2) else x)
      smooth_terms_table[] <- lapply(smooth_terms_table, function(x) if(is.numeric(x)) format(x, nsmall = 2) else x)
      
      # --- Add a row under intercept with column variable names ---
      header_row <- data.frame(
        Parameters = "Non‑parametric terms",
        Col2 = "edf",
        Col3 = "Ref.df",
        Col4 = "F",
        Col5 = "p-value",
        stringsAsFactors = FALSE
      )
      
      # Combine: intercept, header row, then smooth terms
      full_table <- rbind(intercept_row, header_row, smooth_terms_table)
      
      # --- Rename columns after binding ---
      colnames(full_table) <- c("Parameters", "Estimate", "Std. Error", "t-value", "p-value")
      
      # Render
      DT::datatable(full_table,
                    options = list(
                      striped = TRUE,
                      hover = TRUE,
                      bordered = TRUE,
                      # Center all columns except the first
                      columnDefs = list(
                        list(className = 'dt-center', targets = 1:4)
                      ),
                      rowCallback = JS(
                        "function(row, data) {",
                        "  if(data[0] === 'Non‑parametric terms') {",
                        "    $(row).addClass('section-header');",
                        "  }",
                        "}"
                      ),
                      # Add the footnote after the table
                      initComplete = JS(
                        #"function(settings, json) {",
                        #"  var footnote = '<div style=\"padding: 5px 20px; background: white; font-style: italic; font-size: 0.85em; color: black; text-align: left; margin-top: 5px;\"><strong>Footnote:</strong> edf = Effective degrees of freedom, Ref df = Reference degrees of freedom</div>';",
                        #"  $(this.api().table().container()).append(footnote);",
                        #"}"
                      )
                    ),
                    rownames = FALSE,
                    caption = htmltools::tags$caption(
                      class = "data-table-title",
                      #style = "caption-side: top; padding: 10px 20px; text-align: left; font-weight: bold;",
                      "Table 7: Parametric and Non‑Parametric Estimates of the GAM"
                    )
      )
    })
    
  })
  
  #============================================
  # REGIONAL GAM PLOTS
  #============================================
  plot_models_reactive <- reactive({
    switch(input$plot_region,
           "Upper East"    = list(model_UE_1, model_UE_2, model_UE_3, model_UE_4, model_UE_5, model_UE_6, model_UE_7, model_UE_8),
           "Upper West"    = list(model_UW_1, model_UW_2, model_UW_3, model_UW_4, model_UW_5, model_UW_6, model_UW_7, model_UW_8),
           "Northern"      = list(model_NO_1, model_NO_2, model_NO_3, model_NO_4, model_NO_5, model_NO_6, model_NO_7, model_NO_8),
           "Brong Ahafo"   = list(model_BA_1, model_BA_2, model_BA_3, model_BA_4, model_BA_5, model_BA_6, model_BA_7, model_BA_8),
           "Ashanti"       = list(model_AS_1, model_AS_2, model_AS_3, model_AS_4, model_AS_5, model_AS_6, model_AS_7, model_AS_8),
           "Eastern"       = list(model_EA_1, model_EA_2, model_EA_3, model_EA_4, model_EA_5, model_EA_6, model_EA_7, model_EA_8),
           "Volta"         = list(model_VO_1, model_VO_2, model_VO_3, model_VO_4, model_VO_5, model_VO_6, model_VO_7, model_VO_8),
           "Greater Accra" = list(model_GA_1, model_GA_2, model_GA_3, model_GA_4, model_GA_5, model_GA_6, model_GA_7, model_GA_8),
           "Central"       = list(model_CE_1, model_CE_2, model_CE_3, model_CE_4, model_CE_5, model_CE_6, model_CE_7, model_CE_8),
           "Western"       = list(model_WE_1, model_WE_2, model_WE_3, model_WE_4, model_WE_5, model_WE_6, model_WE_7, model_WE_8)
    )
  })
  
  # Get best model for plots
  plot_best_model <- reactive({
    models <- plot_models_reactive()
    best_model_idx <- which.max(sapply(models, function(model) summary(model)$r.sq * summary(model)$dev.expl))
    models[[best_model_idx]]
  })
  
  # Get available parameters from best model
  available_params <- reactive({
    best_model_fit <- plot_best_model()
    summary(best_model_fit)$s.table %>% row.names()
  })
  
  # Dynamic UI for plots
  output$dynamic_plots <- renderUI({
    used_params <- available_params()
    
    available_plots <- list()
    
    if ("s(time)" %in% used_params) {
      available_plots <- c(available_plots, list(plotlyOutput("model_plots_1", height = "350px")))
    }
    if ("s(months)" %in% used_params) {
      available_plots <- c(available_plots, list(plotlyOutput("model_plots_2", height = "350px")))
    }
    if ("s(rainfall)" %in% used_params) {
      available_plots <- c(available_plots, list(plotlyOutput("model_plots_3", height = "350px")))
    }
    if ("s(avgtemp)" %in% used_params) {
      available_plots <- c(available_plots, list(plotlyOutput("model_plots_4", height = "350px")))
    }
    if ("ti(time,months)" %in% used_params) {
      available_plots <- c(available_plots, list(plotlyOutput("model_plots_5", height = "350px")))
    }
    if ("ti(avgtemp,rainfall)" %in% used_params) {
      available_plots <- c(available_plots, list(plotlyOutput("model_plots_6", height = "350px")))
    }
    
    # If no plots available
    if (length(available_plots) == 0) {
      return(div(
        style = "text-align: center; padding: 50px;",
        h4("No smooth terms available in the selected model", style = "color: gray;")
      ))
    }
    
    # Arrange in 2-column grid
    rows <- lapply(seq(1, length(available_plots), by = 2), function(i) {
      if (i + 1 <= length(available_plots)) {
        fluidRow(
          column(6, available_plots[[i]]),
          column(6, available_plots[[i+1]])
        )
      } else {
        fluidRow(
          column(6, available_plots[[i]]),
          column(6, div())  # Empty column for alignment
        )
      }
    })
    
    do.call(tagList, rows)
  })
  
  
  # Individual GAM Plots
  output$model_plots_1 <- renderPlotly({
    best_model_fit <- plot_best_model()
    used_params <- available_params()
    
    req("s(time)" %in% used_params)  # Only render if term exists
    
    plots_1 <- gratia::draw(best_model_fit, overall_uncertainty = TRUE, select = "s(time)",
                            smooth_col = "black", ci_col = "cyan", 
                            smooth_lwd = 30, ci_lwd = 30, alpha = 0.5) +
      labs(x = "Time", y = "Partial effect", title = NULL) +
      theme_gam_plots()
    make_static_plotly(plots_1,
                       filename = paste0("s_time_", input$plot_region))
  })
  
  output$model_plots_2 <- renderPlotly({
    best_model_fit <- plot_best_model()
    used_params <- available_params()
    
    req("s(months)" %in% used_params)
    
    plots_2 <- gratia::draw(best_model_fit, overall_uncertainty = TRUE, select = "s(months)",
                            smooth_col = "black", ci_col = "cyan", 
                            smooth_lwd = 30, ci_lwd = 30, alpha = 0.5) +
      labs(x = "Months", y = "Partial effect", title = NULL) +
      theme_gam_plots()
    make_static_plotly(plots_2,
                       filename = paste0("s_months_", input$plot_region))
  })
  
  output$model_plots_3 <- renderPlotly({
    best_model_fit <- plot_best_model()
    used_params <- available_params()
    
    req("s(rainfall)" %in% used_params)
    
    plots_3 <- gratia::draw(best_model_fit, overall_uncertainty = TRUE, select = "s(rainfall)",
                            smooth_col = "black", ci_col = "cyan", 
                            smooth_lwd = 30, ci_lwd = 30, alpha = 0.5) +
      labs(x = "Rainfall (mm)", y = "Partial effect", title = NULL) +
      theme_gam_plots()
    make_static_plotly(plots_3,
                       filename = paste0("s_rainfall_", input$plot_region))
  })
  
  output$model_plots_4 <- renderPlotly({
    best_model_fit <- plot_best_model()
    used_params <- available_params()
    
    req("s(avgtemp)" %in% used_params)
    
    plots_4 <- gratia::draw(best_model_fit, overall_uncertainty = TRUE, select = "s(avgtemp)",
                            smooth_col = "black", ci_col = "cyan", 
                            smooth_lwd = 30, ci_lwd = 30, alpha = 0.5) +
      labs(x = "Average temperature (°C)", y = "Partial effect", title = NULL) +
      theme_gam_plots()
    make_static_plotly(plots_4,
                       filename = paste0("s_avgtemp_", input$plot_region))
  })
  
  
  output$model_plots_5 <- renderPlotly({
    best_model_fit <- plot_best_model()
    used_params <- available_params()
    
    req("ti(time,months)" %in% used_params)
    
    plots_5 <- gratia::draw(best_model_fit, overall_uncertainty = TRUE, select = "ti(time,months)",
                            smooth_col = "black", ci_col = "cyan", 
                            smooth_lwd = 30, ci_lwd = 30, alpha = 0.5) +
      labs(x = "Time\nBasis: Tensor product int", y = "Months", title = NULL) +
      theme_gam_plots()
    make_static_plotly(plots_5,
                       filename = paste0("ti_time_months_", input$plot_region))
  })
  
  output$model_plots_6 <- renderPlotly({
    best_model_fit <- plot_best_model()
    used_params <- available_params()
    
    req("ti(avgtemp,rainfall)" %in% used_params)
    
    plots_6 <- gratia::draw(best_model_fit, overall_uncertainty = TRUE, select = "ti(avgtemp,rainfall)",
                            smooth_col = "black", ci_col = "cyan", 
                            smooth_lwd = 30, ci_lwd = 30, alpha = 0.5) +
      labs(x = "Average temperature (°C)\nBasis: Tensor product int", y = "Rainfall (mm)", title = NULL) +
      theme_gam_plots()
    make_static_plotly(plots_6,
                       filename = paste0("ti_avgtemp_rainfall_", input$plot_region))
  })
  
  #============================================
  # GAM DIAGNOSTICS
  #============================================
  diag_models_reactive <- reactive({
    switch(input$diag_region,
           "Upper East"    = list(model_UE_1, model_UE_2, model_UE_3, model_UE_4, model_UE_5, model_UE_6, model_UE_7, model_UE_8), 
           "Upper West"    = list(model_UW_1, model_UW_2, model_UW_3, model_UW_4, model_UW_5, model_UW_6, model_UW_7, model_UW_8),
           "Northern"      = list(model_NO_1, model_NO_2, model_NO_3, model_NO_4, model_NO_5, model_NO_6, model_NO_7, model_NO_8),
           "Brong Ahafo"   = list(model_BA_1, model_BA_2, model_BA_3, model_BA_4, model_BA_5, model_BA_6, model_BA_7, model_BA_8),
           "Ashanti"       = list(model_AS_1, model_AS_2, model_AS_3, model_AS_4, model_AS_5, model_AS_6, model_AS_7, model_AS_8),  
           "Eastern"       = list(model_EA_1, model_EA_2, model_EA_3, model_EA_4, model_EA_5, model_EA_6, model_EA_7, model_EA_8),
           "Volta"         = list(model_VO_1, model_VO_2, model_VO_3, model_VO_4, model_VO_5, model_VO_6, model_VO_7, model_VO_8),
           "Greater Accra" = list(model_GA_1, model_GA_2, model_GA_3, model_GA_4, model_GA_5, model_GA_6, model_GA_7, model_GA_8),
           "Central"       = list(model_CE_1, model_CE_2, model_CE_3, model_CE_4, model_CE_5, model_CE_6, model_CE_7, model_CE_8),
           "Western"       = list(model_WE_1, model_WE_2, model_WE_3, model_WE_4, model_WE_5, model_WE_6, model_WE_7, model_WE_8)
    )
  })
  
  # Pick best model by r.sq * dev.expl
  diag_best_model <- reactive({
    models <- diag_models_reactive()
    best_model_idx <- which.max(sapply(models, function(model) summary(model)$r.sq * summary(model)$dev.expl))
    models[[best_model_idx]]
  })
  
  # Filter data for selected region 
  diag_region_data <- reactive({
    data %>% filter(region == input$diag_region)
  })
  
  # ---------- Observed vs Fitted Plot ----------
  output$observed_fitted <- renderPlotly({
    best_model <- diag_best_model()
    region_data <- diag_region_data()
    
    # Fitted values
    fitted_vals <- fitted(best_model)
    
    # Observed values
    observed_vals <- region_data$uncom
    date <- region_data$date
    
    # Build plotting data
    plot_data <- data.frame(
      Date = date,
      Observed = observed_vals/1000,
      Fitted = fitted_vals/1000
    )
    
    # Plot
    p <- ggplot(plot_data, aes(x = Date)) +
      geom_line(aes(y = Observed, color = "Observed"), linewidth = 1) +
      geom_line(aes(y = Fitted, color = "Fitted"), linetype = "dashed", linewidth = 1) +
      scale_x_date(
        date_labels = "%Y",      
        date_breaks = "1 years" 
      ) +
      scale_color_manual(values = c("Observed" = "black", "Fitted" = "#F8766D")) +
      labs(
        x = "Year",
        y = "Uncomplicated Malaria(x10\u00b3)") + 
      theme_residuals()
    
    make_static_plotly(p,
                       filename = paste0("observed_fitted_", input$diag_region))
  })
  
  
  # ---------- Q–Q Plot of Deviance Residuals ----------
  output$qq_plot <- renderPlotly({
    region <- input$diag_region
    
    # Filter the data based on region and date range
    filtered_data <- subset(data,
                            region == input$diag_region &
                              date >= input$dateRange[1] &
                              date <= input$dateRange[2])
    
    # Get the list of models for the selected region
    models <- switch(region,
                     "Upper East"    = list(model_UE_1, model_UE_2, model_UE_3, model_UE_4, model_UE_5, model_UE_6, model_UE_7, model_UE_8), 
                     "Upper West"    = list(model_UW_1, model_UW_2, model_UW_3, model_UW_4, model_UW_5, model_UW_6, model_UW_7, model_UW_8),
                     "Northern"      = list(model_NO_1, model_NO_2, model_NO_3, model_NO_4, model_NO_5, model_NO_6, model_NO_7, model_NO_8),
                     "Brong Ahafo"   = list(model_BA_1, model_BA_2, model_BA_3, model_BA_4, model_BA_5, model_BA_6, model_BA_7, model_BA_8),
                     "Ashanti"       = list(model_AS_1, model_AS_2, model_AS_3, model_AS_4, model_AS_5, model_AS_6, model_AS_7, model_AS_8),  
                     "Eastern"       = list(model_EA_1, model_EA_2, model_EA_3, model_EA_4, model_EA_5, model_EA_6, model_EA_7, model_EA_8),
                     "Volta"         = list(model_VO_1, model_VO_2, model_VO_3, model_VO_4, model_VO_5, model_VO_6, model_VO_7, model_VO_8),
                     "Greater Accra" = list(model_GA_1, model_GA_2, model_GA_3, model_GA_4, model_GA_5, model_GA_6, model_GA_7, model_GA_8),
                     "Central"       = list(model_CE_1, model_CE_2, model_CE_3, model_CE_4, model_CE_5, model_CE_6, model_CE_7, model_CE_8),
                     "Western"       = list(model_WE_1, model_WE_2, model_WE_3, model_WE_4, model_WE_5, model_WE_6, model_WE_7, model_WE_8)
                     #"National"      = list(model_NAT_1, model_NAT_2, model_NAT_3, model_NAT_4, model_NAT_5, model_NAT_6, model_NAT_7, model_NAT_8)
    )
    
    # Compute metrics to select best model
    model_metrics <- data.frame(
      Model = paste0("Model ", 1:length(models)),
      Adj_R2 = sapply(models, function(model) round(summary(model)$r.sq * 100, 2)),
      Dev_Explained = sapply(models, function(model) round(summary(model)$dev.expl * 100, 2))
    )
    
    best_model_idx <- which.max(model_metrics$Adj_R2 * model_metrics$Dev_Explained)
    best_model_fit <- models[[best_model_idx]]
    
    # Plot using mgcv’s qq_plot with method = "normal"
    p <- qq_plot(best_model_fit, method = "normal") +
      labs(title=NULL, x = "Theoretical quantiles", y = "Deviance residuals") +
      theme_residuals()
    
    make_static_plotly(p,
                       filename = paste0("qq_", input$diag_region))
  })
  
  # ---------- Response vs Fitted Plot ----------
  output$response_fitted_plot <- renderPlotly({
    region <- input$diag_region
    
    # Filter the data based on region and date range
    filtered_data <- subset(data,
                            region == input$diag_region &
                              date >= input$dateRange[1] &
                              date <= input$dateRange[2])
    
    # Get the list of models for the selected region
    models <- switch(region,
                     "Upper East"    = list(model_UE_1, model_UE_2, model_UE_3, model_UE_4, model_UE_5, model_UE_6, model_UE_7, model_UE_8), 
                     "Upper West"    = list(model_UW_1, model_UW_2, model_UW_3, model_UW_4, model_UW_5, model_UW_6, model_UW_7, model_UW_8),
                     "Northern"      = list(model_NO_1, model_NO_2, model_NO_3, model_NO_4, model_NO_5, model_NO_6, model_NO_7, model_NO_8),
                     "Brong Ahafo"   = list(model_BA_1, model_BA_2, model_BA_3, model_BA_4, model_BA_5, model_BA_6, model_BA_7, model_BA_8),
                     "Ashanti"       = list(model_AS_1, model_AS_2, model_AS_3, model_AS_4, model_AS_5, model_AS_6, model_AS_7, model_AS_8),  
                     "Eastern"       = list(model_EA_1, model_EA_2, model_EA_3, model_EA_4, model_EA_5, model_EA_6, model_EA_7, model_EA_8),
                     "Volta"         = list(model_VO_1, model_VO_2, model_VO_3, model_VO_4, model_VO_5, model_VO_6, model_VO_7, model_VO_8),
                     "Greater Accra" = list(model_GA_1, model_GA_2, model_GA_3, model_GA_4, model_GA_5, model_GA_6, model_GA_7, model_GA_8),
                     "Central"       = list(model_CE_1, model_CE_2, model_CE_3, model_CE_4, model_CE_5, model_CE_6, model_CE_7, model_CE_8),
                     "Western"       = list(model_WE_1, model_WE_2, model_WE_3, model_WE_4, model_WE_5, model_WE_6, model_WE_7, model_WE_8)
    )
    
    # Compute metrics to select best model
    model_metrics <- data.frame(
      Model = paste0("Model ", 1:length(models)),
      Adj_R2 = sapply(models, function(model) round(summary(model)$r.sq * 100, 2)),
      Dev_Explained = sapply(models, function(model) round(summary(model)$dev.expl * 100, 2))
    )
    
    best_model_idx <- which.max(model_metrics$Adj_R2 * model_metrics$Dev_Explained)
    best_model_fit <- models[[best_model_idx]]
    
    # Observed response values
    observed_vals <- filtered_data$uncom
    
    # Fitted values from the model
    fitted_vals <- fitted(best_model_fit)
    
    # Build plotting data
    plot_data <- data.frame(
      Fitted = fitted_vals/1000,
      Observed = observed_vals/1000
    )
    
    # Plot response vs fitted
    p <- ggplot(data = plot_data, aes(x = Fitted, y = Observed)) +
      geom_point(size=2.5, color="#F8766D") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "steelblue") +
      labs(x = "Fitted values(x10\u00b3)", y = "Uncomplicated Malaria(x10\u00b3)") +
      theme_residuals()
      
    make_static_plotly(p,
                       filename = paste0("response_fitted_", input$diag_region))
  })
    
  
  #============================================
  # APPENDIX
  #============================================

  # ------- Combined Time Series Plots --------
  combined_filtered_data <- reactive({
    data %>%
      dplyr::filter(
        date >= as.Date(input$combined_dateRange[1]),
        date <= as.Date(input$combined_dateRange[2])
      ) %>%
      arrange(region, date) %>%
      mutate(date = as.Date(date))
  })

  output$combined_plot <- renderPlotly({

    plot_data <- combined_filtered_data()

    # Guard against empty data
    req(nrow(plot_data) > 0)

    # Define the first plot (no legend)
    p1 <- ggplot(plot_data, aes(x = date, y = uncom/1000, color = region)) +
      geom_line() +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(x = "", y = "Malaria cases(x10\u00b3)", color = "Region") +
      theme_app_series()

    # Define the second plot (no legend)
    p2 <- ggplot(plot_data, aes(x = date, y = rainfall, color = region)) +
      geom_line() +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(x = "", y = "Rainfall (mm)", color = "Region") +
      theme_app_series()

    # Define the third plot (legend at bottom)
    p3 <- ggplot(plot_data, aes(x = date, y = avgtemp, color = region)) +
      geom_line() +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous(
        limits = c(22.5, 35),
        breaks = seq(22.5, 35, by = 2.5)
      ) +
      labs(x = "Year", y = "Average Temperature (°C)", color = "Region") +
      theme_classic(base_size = 12) +
      theme(
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text.y  = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.text.x  = element_text(face = "bold", size = 12),
        panel.background = element_rect(fill = "#f0f0f0"),
        legend.position = "none"
      ) +
      guides(color = guide_legend(nrow = 1, title = NULL))

    # Convert to plotly
    p1_plotly <- ggplotly(p1)
    p2_plotly <- ggplotly(p2)
    p3_plotly <- ggplotly(p3)

    # Suppress legend on ALL traces in p1 and p2
    for (i in seq_along(p1_plotly$x$data)) {
      p1_plotly$x$data[[i]]$showlegend <- FALSE
    }
    for (i in seq_along(p2_plotly$x$data)) {
      p2_plotly$x$data[[i]]$showlegend <- FALSE
    }
    # Keep legend only on p3 traces
    for (i in seq_along(p3_plotly$x$data)) {
      p3_plotly$x$data[[i]]$showlegend <- TRUE
    }

    # Build the combined subplot
    combined <- subplot(
      p1_plotly, p2_plotly, p3_plotly,
      nrows  = 3,
      shareX = TRUE,
      titleY = TRUE
    )

    # Layout and static config
    combined %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation   = "h",
          x             = 0.5,
          xanchor       = "center",
          y             = -0.12,
          yanchor       = "top",
          font          = list(size = 14),
          tracegroupgap = 0
        ),
        yaxis  = list(fixedrange = TRUE, title = list(text = "<b>Malaria cases(x10\u00b3)</b>", font = list(size = 14))),
        yaxis2 = list(fixedrange = TRUE, title = list(text = "<b>Rainfall (mm)</b>",            font = list(size = 14))),
        yaxis3 = list(fixedrange = TRUE, title = list(text = "<b>Average Temperature (°C)</b>", font = list(size = 14))),
        xaxis  = list(fixedrange = TRUE),
        xaxis2 = list(fixedrange = TRUE),
        xaxis3 = list(fixedrange = TRUE)
      ) %>%
      style(hoverinfo = "none") %>%
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c(
          "zoom2d", "pan2d", "select2d", "lasso2d",
          "zoomIn2d", "zoomOut2d", "autoScale2d",
          "hoverClosestCartesian", "hoverCompareCartesian",
          "sendDataToCloud", "toggleSpikelines",
          "toImage"
        ),
        displaylogo = FALSE
      )
  })
  
  output$download_combined_plot <- downloadHandler(
    filename = function() paste0("combined_plot_", Sys.Date(), ".png"),
    content = function(file) {
      plot_data <- combined_filtered_data()
      
      p1 <- ggplot(plot_data, aes(x = date, y = uncom/1000, color = region)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        labs(x = "", y = "Malaria cases(x10\u00b3)", color = "Region") +
        theme_app_series()
      
      p2 <- ggplot(plot_data, aes(x = date, y = rainfall, color = region)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        labs(x = "", y = "Rainfall (mm)", color = "Region") +
        theme_app_series()
      
      p3 <- ggplot(plot_data, aes(x = date, y = avgtemp, color = region)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        scale_y_continuous(limits = c(22.5, 35), breaks = seq(22.5, 35, by = 2.5)) +
        labs(x = "Year", y = "Average Temperature (°C)", color = "Region") +
        theme_classic(base_size = 12) +
        theme(
          axis.title.y = element_text(face = "bold", size = 12),
          axis.text.y  = element_text(face = "bold", size = 12),
          axis.title.x = element_text(face = "bold", size = 12),
          axis.text.x  = element_text(face = "bold", size = 12),
          panel.background = element_rect(fill = "#f0f0f0"),
          legend.position = "bottom"
        ) +
        guides(color = guide_legend(nrow = 1, title = NULL))
      
      combined_grid <- gridExtra::arrangeGrob(p1, p2, p3, ncol = 1, nrow = 3)
      ggsave(file, plot = combined_grid, width = 10, height = 10, dpi = 300, device = "png")
    }
  )

  
  # ------- Combined Heatmaps --------
    output$combined_heatmap <- renderPlot({
      df_avg <- data %>%
      dplyr::filter(
        date >= as.Date(input$heatmap_dateRange[1]),
        date <= as.Date(input$heatmap_dateRange[2])
      ) %>%
      mutate(month = factor(month, levels = month.abb)) %>%
      group_by(month, region) %>%
      summarise(
        uncom    = mean(uncom,    na.rm = TRUE),
        avgtemp  = mean(avgtemp,  na.rm = TRUE),
        rainfall = mean(rainfall, na.rm = TRUE),
        .groups  = "drop"
      )
    
    # Heatmap 1 — Malaria Cases
    heatmap_1 <- ggplot(df_avg, aes(x = region, y = month, fill = uncom/1000)) +
      geom_tile(color = "white") +
      scale_fill_gradient(name = "Malaria cases(x10\u00b3)", low = "#f7fcfc", high = "#4d0c4b") +
      labs(x = "", y = "Month", title = "") +
      theme_heatmap1()
    
    # Heatmap 2 — Average Temperature
    heatmap_2 <- ggplot(df_avg, aes(x = region, y = month, fill = avgtemp)) +
      geom_tile(color = "white") +
      scale_fill_distiller(name = "Average Temperature (°C)", palette = "Spectral") +
      labs(x = "", y = "Month", title = "") +
      theme_heatmap2()
    
    # Heatmap 3 — Rainfall
    heatmap_3 <- ggplot(df_avg, aes(x = region, y = month, fill = rainfall)) +
      geom_tile(color = "white") +
      scale_fill_gradient(name = "Rainfall (mm)", low = "#f7fbff", high = "#08316b") +
      labs(x = "", y = "Month", title = "") +
      theme_heatmap3()
      
    # Arrange and save
    combined_grid <- grid.arrange(heatmap_1, heatmap_2, heatmap_3, ncol = 1, nrow = 3)
    
  })
  
  #Download handler for combined heatmaps
  output$download_heatmap <- downloadHandler(
    filename = function() paste0("combined_heatmap_", Sys.Date(), ".png"),
    content = function(file) {
      df_avg <- data %>%
        dplyr::filter(
          date >= as.Date(input$heatmap_dateRange[1]),
          date <= as.Date(input$heatmap_dateRange[2])
        ) %>%
        mutate(month = factor(month, levels = month.abb)) %>%
        group_by(month, region) %>%
        summarise(
          uncom    = mean(uncom,    na.rm = TRUE),
          avgtemp  = mean(avgtemp,  na.rm = TRUE),
          rainfall = mean(rainfall, na.rm = TRUE),
          .groups  = "drop"
        )
      
      heatmap_1 <- ggplot(df_avg, aes(x = region, y = month, fill = uncom/1000)) +
        geom_tile(color = "white") +
        scale_fill_gradient(name = "Malaria cases(x10\u00b3)", low = "#f7fcfc", high = "#4d0c4b") +
        labs(x = "", y = "Month", title = "") +
        theme_heatmap1()
      
      heatmap_2 <- ggplot(df_avg, aes(x = region, y = month, fill = avgtemp)) +
        geom_tile(color = "white") +
        scale_fill_distiller(name = "Average Temperature (°C)", palette = "Spectral") +
        labs(x = "", y = "Month", title = "") +
        theme_heatmap2()
      
      heatmap_3 <- ggplot(df_avg, aes(x = region, y = month, fill = rainfall)) +
        geom_tile(color = "white") +
        scale_fill_gradient(name = "Rainfall (mm)", low = "#f7fbff", high = "#08316b") +
        labs(x = "", y = "Month", title = "") +
        theme_heatmap3()
      
      combined_grid <- gridExtra::arrangeGrob(heatmap_1, heatmap_2, heatmap_3, ncol = 1, nrow = 3)
    }
  )
  
  #---------PATTERNS OF AGGREGATED HISTOGRAM PLOTS---------
  
  # Function to standardize values
  stdFun <- function(dataVec, a=0, b=1){
    stdVec = (dataVec - a)/(b - a)
    return(stdVec)
  }
  
  # Reactive function to process data based on selected region
  process_data <- reactive({
    # Guard against NA dates during slider transition
    req(
      !is.na(input$seasonal_dateRange[1]),
      !is.na(input$seasonal_dateRange[2]),
      input$seasonal_dateRange[1] < input$seasonal_dateRange[2]
    )
    
    # Filter data based on selected region
    mal_data <- subset(data, region == input$region)
    
    # Create plot data frame
    plot_data <- data.frame(
      year = mal_data$year,
      month = mal_data$months,
      uncomp_malaria = mal_data$uncom,
      rainfall = mal_data$rainfall,
      temperature = mal_data$avgtemp
    )
    
    start_year  <- as.integer(format(input$seasonal_dateRange[1], "%Y"))
    start_month <- 1  # always start from January of the selected start year
    
    end_year    <- as.integer(format(input$seasonal_dateRange[2], "%Y"))
    end_month   <- 12  # always end at December of the selected end year
    
    # Filter rows to only those within the selected date range
    plot_data <- plot_data[
      plot_data$year >= start_year & plot_data$year <= end_year,
    ]
    
    # Validate: need at least 1 full year
    if (nrow(plot_data) < 12) {
      validate("Please select a date range spanning at least one full year.")
    }
    
    # Validate: must be exact multiple of 12 for matrix
    if (nrow(plot_data) %% 12 != 0) {
      validate(paste0("Selected range (", nrow(plot_data), " months) must cover complete years only."))
    }
    
    # Convert to time series
    cases_s    <- ts(plot_data[,3], st=c(start_year, start_month), end=c(end_year, end_month), fr=12)
    rainfall_s <- ts(plot_data[,4], st=c(start_year, start_month), end=c(end_year, end_month), fr=12)
    temp_s     <- ts(plot_data[,5], st=c(start_year, start_month), end=c(end_year, end_month), fr=12)
    
    # Create matrices
    cases_monthly <- matrix(cases_s, ncol=12, byrow=TRUE)
    rainfall_monthly <- matrix(rainfall_s, ncol=12, byrow=TRUE)
    temp_monthly <- matrix(temp_s, ncol=12, byrow=TRUE)
    
    # Calculate monthly values
    cases_month <- apply(cases_monthly, 2, sum)
    average_monthly_rainfall <- apply(rainfall_monthly, 2, mean)
    average_monthly_temperature <- apply(temp_monthly, 2, mean)
    
    # Standardize based on region
    if(input$region == "Brong Ahafo") {
      cases_month <- stdFun(cases_month, 0e5, 105e4)    
      average_monthly_rainfall <- stdFun(average_monthly_rainfall, 0, 180)
      average_monthly_temperature <- stdFun(average_monthly_temperature, 25, 30)
    } else if(input$region == "Ashanti") {
      cases_month <- stdFun(cases_month, 0e5, 73e4)
      average_monthly_rainfall <- stdFun(average_monthly_rainfall, 0, 280)
      average_monthly_temperature <- stdFun(average_monthly_temperature, 25, 30)
    } else if(input$region == "Eastern") {
      cases_month <- stdFun(cases_month, 0e5, 80e4)
      average_monthly_rainfall <- stdFun(average_monthly_rainfall, 0, 205)
      average_monthly_temperature <- stdFun(average_monthly_temperature, 25, 30)
    } else if(input$region == "Upper East") {
      cases_month <- stdFun(cases_month, 0e5, 86e4)
      average_monthly_rainfall <- stdFun(average_monthly_rainfall, 0, 240)
      average_monthly_temperature <- stdFun(average_monthly_temperature, 20, 35)
    } else if(input$region == "Upper West") {
      cases_month <- stdFun(cases_month, 0e5, 43e4)
      average_monthly_rainfall <- stdFun(average_monthly_rainfall, 0, 240)
      average_monthly_temperature <- stdFun(average_monthly_temperature, 25, 35)
    } else if(input$region == "Volta") {
      cases_month <- stdFun(cases_month, 0e5, 50e4)
      average_monthly_rainfall <- stdFun(average_monthly_rainfall, 0, 205)
      average_monthly_temperature <- stdFun(average_monthly_temperature, 25, 30)
    } else if(input$region == "Greater Accra") {
      cases_month <- stdFun(cases_month, 0e5, 30e4)
      average_monthly_rainfall <- stdFun(average_monthly_rainfall, 0, 180)
      average_monthly_temperature <- stdFun(average_monthly_temperature, 25, 30)
    } else if(input$region == "Central") {  
      cases_month <- stdFun(cases_month, 0e5, 60e4)
      average_monthly_rainfall <- stdFun(average_monthly_rainfall, 0, 230)
      average_monthly_temperature <- stdFun(average_monthly_temperature, 24, 29)
    } else if(input$region == "Western") {  
      cases_month <- stdFun(cases_month, 0e5, 75e4)
      average_monthly_rainfall <- stdFun(average_monthly_rainfall, 0, 335)
      average_monthly_temperature <- stdFun(average_monthly_temperature, 25, 30)
    } else { # Northern
      cases_month <- stdFun(cases_month, 0e5, 57e4)
      average_monthly_rainfall <- stdFun(average_monthly_rainfall, 0, 200)
      average_monthly_temperature <- stdFun(average_monthly_temperature, 25, 35)
    }
    
    list(
      cases_month = cases_month,
      average_monthly_rainfall = average_monthly_rainfall,
      average_monthly_temperature = average_monthly_temperature
    )
  })
  
  # Function to create the plot
  create_plot <- function() {
    processed_data <- process_data()
    
    monthLabs <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    axTiks <- seq(0, 1, length.out=12)
    
    # Create polygon coordinates for bar chart
    xpoly <- c()
    ypoly <- c()
    for(i in 1:length(axTiks)){
      xx <- c(rep(axTiks[i] -.04, 2), rep(axTiks[i] +.04, 2), NA)
      yy <- c(0, rep(processed_data$cases_month[i], 2), 0, NA)
      xpoly <- c(xpoly, xx)
      ypoly <- c(ypoly, yy)
    }
    
    # Set up plot parameters
    par(mar=c(4,3.5,2,8), las = 1, mfrow=c(1,1))
    
    # Create base plot
    plot(0, type="n", xlim=c(-.05,1.05), ylim=c(0,1), xlab = "", ylab="",
         xaxt = "n", yaxt = "n", bty="n")
    
    # Add bars
    polygon(cbind(xpoly, ypoly), col="#BDC9E1")
    
    # Add malaria cases axis
    yCases <- seq(0, 1, length.out = 6)
    max_cases <- switch(input$region,
                        "Upper East" = 860,
                        "Upper West" = 430,
                        "Northern" = 565,
                        "Brong Ahafo" = 1050,
                        "Ashanti" = 730,
                        "Eastern" = 800,
                        "Volta" = 500,
                        "Greater Accra" = 300,
                        "Central" = 600,    
                        "Western" = 750)    
    labCas <- seq(0, max_cases, length.out = 6)
    axis(2, yCases, labCas, line=-1.3, lwd = 2, hadj = .75,
         padj = 0.05, cex.axis=1.5, font.axis=2, col="black")
    
    # Add temperature line and axis
    points(axTiks, processed_data$average_monthly_temperature, 
           type="l", col="#045A8D", lwd=4, pch=19)
    points(axTiks, processed_data$average_monthly_temperature, 
           type="b", col="#045A8D", lwd=2, pch=19)
    
    temp_range <- switch(input$region,
                         "Upper East" = c(20, 35),
                         "Upper West" = c(25, 35),
                         "Northern" = c(25, 35),
                         "Brong Ahafo" = c(25, 30),
                         "Ashanti" = c(25, 30),
                         "Eastern" = c(25, 30),
                         "Volta" = c(25, 30),
                         "Greater Accra" = c(25, 30),
                         "Central" = c(24, 29),    
                         "Western" = c(25, 30))    
    
    labTemp <- seq(temp_range[1], temp_range[2], length.out = 6)
    axis(4, yCases, labTemp, line=-0.1, col = "#045A8D", lwd = 2, 
         hadj = .4, padj = .05, cex.axis=1.5, font.axis=2)
    
    # Add rainfall line and axis
    points(axTiks, processed_data$average_monthly_rainfall, 
           type="b", col="#8F0E00", lwd=2, pch=19)
    points(axTiks, processed_data$average_monthly_rainfall, 
           type="l", col="#8F0E00", lwd=4, pch=19)
    
    max_rain <- switch(input$region,
                       "Upper East" = 240,
                       "Upper West" = 240,
                       "Northern" = 200,
                       "Brong Ahafo" = 180,
                       "Ashanti" = 280,
                       "Eastern" = 205,
                       "Volta" = 205,
                       "Greater Accra" = 180,
                       "Central" = 230,    
                       "Western" = 335)    
    
    labRain <- seq(0, max_rain, length.out = 6)
    axis(4, yCases, labRain, line=2.9, col = "#8F0E00", lwd = 2, 
         hadj = .4, padj = .05, cex.axis=1.5, font.axis=2)
    
    # Add labels and titles
    mtext(expression(bold("Malaria cases (x10³)")), 
          2, las=0, line=1.4, font.main=2, cex=1.5, col="black")
    mtext(expression(bold("Temperature (°C)")), 
          4, las=0, line=-1.00, col="#045A8D", cex=1.5)
    mtext(expression(bold(paste("Precipitation ", (mm)))), 
          4, las=0, line=2.0, col="#8F0E00", cex=1.5)
    mtext(input$region, 
          3, line=-2, cex=1.5, adj = 0.1, font=2)  
    
    # Add x-axis labels
    axis(1, axTiks, monthLabs, las=1, tick = F, pos=0, 
         padj = -0.6, font.axis=2, cex.axis=1.5)
    mtext(expression(bold("Time (months)")), 1, line=1.0, cex=1.5)
    
    # Add baseline
    line_y_position <- stdFun(0, 0, 75)
    lines(c(-0.07, 2.0), c(line_y_position, line_y_position), 
          col = "#BDC9E1", lwd = 2)
  }
  
  # Render the plot
  output$malaria_plot <- renderPlot({
    create_plot()
  })
  
  output$download_plot <- downloadHandler(
    filename = function() paste0("seasonal_pattern_", input$region, ".png"),
    content = function(file) {
      png(file, width = 2000, height = 1000, res = 150)
      create_plot()
      dev.off()
    }
  )
  
}







