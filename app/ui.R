
# ==============================================
# DASHBOARD USER INTERFACE
# ==============================================

ui <- dashboardPage(
  skin = "purple",   
  
  # ============================================
  # DASHBOARD HEADER
  # ============================================
  dashboardHeader(
    title = tags$span(
      "GAM-MAP",
      style = "color: white; font-size: 3.5rem; margin-top: 20px; opacity: 0.95;"
      
    ),
    titleWidth = 250   
  ),
  
  # ============================================
  # DASHBOARD SIDEBAR
  # ============================================
  dashboardSidebar(
    width = 250, 
    sidebarMenu(
      id = "tabs",
      menuItem("Welcome", tabName = "welcome", icon = icon("house")),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Help", tabName = "help", icon = icon("question-circle")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-bar"),
               menuSubItem("Descriptives", tabName = "descriptives", icon = icon("table")),
               menuSubItem("Time Series", tabName = "timeseries", icon = icon("chart-line")),
               menuSubItem("Seasonality", tabName = "seasonality", icon = icon("calendar")),
               menuSubItem("Model Estimates", tabName = "estimates", icon = icon("calculator")),
               menuSubItem("Model Diagnostics", tabName = "diagnostics", icon = icon("stethoscope")),
               menuSubItem("Model Plots", tabName = "plots", icon = icon("chart-area"))
      ),
      menuItem("Appendix", tabName = "appendix", icon = icon("book")),
      menuItem("Source Code", tabName = "code_tab", icon = icon("github"))
    )
  ),
  
  # ============================================
  # DASHBOARD BODY
  # ============================================
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML(dashboardCSS)),
      tags$link(rel = "stylesheet",
                href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css")
    ),
    
    # Loading Screen
    div(
      id = "loading-overlay",
      class = "loading-overlay",
      div(class = "loading-spinner"),
      h2("Loading Application...", style = "margin-top: 30px; font-size: 50px; font-weight: 300;")
    ),
    
    tabItems(
      # ============================================
      # WELCOME TAB
      # ============================================
      tabItem(
        tabName = "welcome",
        
        # Hero Section
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = FALSE,
            div(
              style = "padding: 30px; 
               display: flex; 
               align-items: center; 
               justify-content: space-between; 
               background: linear-gradient(135deg, var(--content-bg) 0%, var(--primary) 100%) !important;
               border-radius: 10px;", 
              
              # LEFT: Logo row
              div(
                class = "logo-row",
                style = "flex: 0 0 auto; margin-right: 30px;",
                
                tags$a(
                  href = "https://masha.uct.ac.za", target = "_blank",
                  tags$span(
                    id = "logo_masha",
                    tags$img(src = "./images/logo_masha.png", alt = "MASHA")
                  )
                ),
                bsTooltip(
                  id        = "logo_masha",
                  title     = "Click to visit MASHA website",
                  placement = "bottom"
                ),
                
                tags$a(
                  href = "https://www.uct.ac.za", target = "_blank",
                  tags$span(
                    id = "logo_uct",
                    tags$img(src = "./images/logo_uct.png", alt = "UCT")
                  )
                ),
                bsTooltip(
                  id        = "logo_uct",
                  title     = "Click to visit University of Cape Town website",
                  placement = "bottom" 
                ),
                
                tags$a(
                  href = "https://science.uct.ac.za/departments/statistical-sciences", target = "_blank",
                  tags$span(
                    id = "logo_stats",
                    tags$img(src = "./images/Statistical Science.jpg", alt = "Stats Science")
                  )
                ),
                bsTooltip(
                  id        = "logo_stats",
                  title     = "Click to visit Department of Statistical Sciences, UCT",
                  placement = "bottom" 
                ),
                
                tags$a(
                  href = "https://ghs.gov.gh/", target = "_blank",
                  tags$span(
                    id = "logo_ghs",
                    tags$img(src = "./images/GHS_logo.jpg", alt = "Ghana Health Service")
                  )
                ),
                bsTooltip(
                  id        = "logo_ghs",
                  title     = "Click to visit Ghana Health Service website",
                  placement = "bottom" 
                )
              ),
              
              # div(
              #   class = "logo-row",
              #   style = "flex: 0 0 auto; margin-right: 30px;",
              #   tags$a(
              #     href = "https://masha.uct.ac.za", target = "_blank",
              #     tags$img(src = "./images/logo_masha.png", alt = "MASHA")
              #   ),
              #   bsTooltip("masha_logo", "Click to visit", placement = "bottom", trigger = "hover"),
              #   
              #   tags$a(
              #     href = "https://www.uct.ac.za", target = "_blank",
              #     tags$img(src = "./images/logo_uct.png", alt = "UCT")
              #   ),
              #   tags$a(
              #     href = "https://science.uct.ac.za/departments/statistical-sciences", target = "_blank",
              #     tags$img(src = "./images/Statistical Science.jpg", alt = "Stats Science")
              #   ),
              #   tags$a(
              #     href = "https://ghs.gov.gh/", target = "_blank",
              #     tags$img(src = "./images/GHS_logo.jpg", alt = "Ghana Health Service")
              #   )
              # ),
              
              # RIGHT: Title and subtitle
              div(
                style = "flex: 1; text-align: left;",
                h1(
                  icon("bug", class = "fa-2x"),
                  icon("disease", class = "fa-2x", style = "margin-left: 10px;"),
                  "GAM-Based Malaria Analytics Platform (GAM-MAP)",
                  style = "color: white; font-size: 2.5rem; margin: 0;"
                ),
                p(
                  "Temporal and Climatic Analysis of Uncomplicated Malaria in Ghana",
                  style = "color: white; font-size: 2.10rem; margin-top: 15px; opacity: 0.95;"
                )
              )
            )
          )
        ),
        
        # Content Row
        fluidRow(
          # Quick Overview
          column(
            width = 4,
            box(
              width = NULL,
              title = tags$span(icon("info-circle"), "Quick Overview"),
              status = "primary",
              solidHeader = TRUE,
              includeMarkdown("www/markdown/welcome_page_0.md")
            )
          ),
          
          # Main Content
          column(
            width = 8,
            box(
              width = NULL,
              title = tags$span(icon("chart-line"), "The GAM-Based Malaria Analytics Platform"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              includeMarkdown("www/markdown/welcome_page_1.Rmd")
            ),
            box(
              width = NULL,
              title = tags$span(icon("cogs"), "Analysis Process"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              includeMarkdown("www/markdown/welcome_page_2.Rmd")
            ),
            box(
              width = NULL,
              title = tags$span(icon("play-circle"), "Getting Started"),
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              div(
                style = "padding: 15px;",
                h4(icon("rocket"), "How to Use This Application"),
                tags$ol(
                  tags$li(
                    strong("Application Loads:"),
                    " Wait 1-5 seconds for data and models to load"
                  ),
                  tags$li(
                    strong("Select a Region:"),
                    " Navigate to the Analysis section and choose a region of interest"
                  ),
                  tags$li(
                    strong("Explore Descriptives:"),
                    " Review summary statistics and descriptive tables"
                  ),
                  tags$li(
                    strong("Examine Temporal Patterns:"),
                    " Use the Time Series and Seasonality tabs to explore trends and seasonal dynamics"
                  ),
                  tags$li(
                    strong("Evaluate Model Results:"),
                    " Review GAM estimates, diagnostics, and smooth term plots"
                  )
                )
              )
            ),
            box(
              width = NULL,
              title = tags$span(icon("graduation-cap"), "Learning about GAM"),
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              includeMarkdown("www/markdown/welcome_page_3.Rmd")
            )
          )
        )
      ),
      
      # ============================================
      # ABOUT TAB - Updated with Project Team Section
      # ============================================
      tabItem(
        tabName = "about",
        
        fluidRow(
          box(
            width = 12,
            background = "teal",
            div(
              style = "padding: 20px; text-align: center;",
              h2(icon("info-circle", class = "fa-2x"), "About the GAM-MAP",
                 style = "color: white; margin: 0;")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              width = NULL,
              title = tags$span(icon("info-circle"), "About Overview"),
              status = "primary",
              solidHeader = TRUE,
              includeMarkdown("www/markdown/about_page_0.Rmd")
            )
          ),
          column(
            width = 8,
            box(
              width = NULL,
              title = tags$span(icon("bullseye"), "Objectives"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              includeMarkdown("www/markdown/about_page_1.Rmd")
            ),
            box(
              width = NULL,
              title = tags$span(icon("map"), "Study Area: Ghana's Ecological Zones"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              div(
                style = "text-align: center; padding: 20px;",
                img(src = './images/Ghana_ecological_zones.png',
                    style = 'max-width: 80%; height: auto; border-radius: 5px;'),
                p(style = "margin-top: 15px; font-style: italic;",
                  strong("Fig 1."), "Map of Ghana showing administrative regions")
              )
            ),
            box(
              width = NULL,
              title = tags$span(icon("rocket"), "Launching Process of the GAM-MAP"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              includeMarkdown("www/markdown/about_page_4.Rmd")
            ),
            box(
              width = NULL,
              title = tags$span(icon("database"), "GAM-MAP Data Sources"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              includeMarkdown("www/markdown/about_page_3.Rmd")
            ),
            
            # ---- NEW: Project Team Box ----
            box(
              width = NULL,
              title = tags$span(icon("users"), "Project Team"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              includeMarkdown("www/markdown/about_page_5.Rmd")
            )
          )
        )
      ),
      
      
      # # ============================================
      # # ABOUT TAB
      # # ============================================
      # tabItem(
      #   tabName = "about",
      #   
      #   fluidRow(
      #     box(
      #       width = 12,
      #       background = "teal",
      #       div(
      #         style = "padding: 20px; text-align: center;",
      #         h2(icon("info-circle", class = "fa-2x"), "About the GAM-MAP",
      #            style = "color: white; margin: 0;")
      #       )
      #     )
      #   ),
      #   
      #   fluidRow(
      #     column(
      #       width = 4,
      #       box(
      #         width = NULL,
      #         title = tags$span(icon("info-circle"), "About Overview"),
      #         status = "primary",
      #         solidHeader = TRUE,
      #         includeMarkdown("www/markdown/about_page_0.Rmd")
      #       )
      #     ),
      #     column(
      #       width = 8,
      #       box(
      #         width = NULL,
      #         title = tags$span(icon("bullseye"), "Objectives"),
      #         status = "info",
      #         solidHeader = TRUE,
      #         collapsible = TRUE,
      #         includeMarkdown("www/markdown/about_page_1.Rmd")
      #       ),
      #       box(
      #         width = NULL,
      #         title = tags$span(icon("map"), "Study Area: Ghana's Ecological Zones"),
      #         status = "info",
      #         solidHeader = TRUE,
      #         collapsible = TRUE,
      #         collapsed = TRUE,
      #         div(
      #           style = "text-align: center; padding: 20px;",
      #           img(src = './images/Ghana_ecological_zones.png',
      #               style = 'max-width: 80%; height: auto; border-radius: 5px;'),
      #           p(style = "margin-top: 15px; font-style: italic;",
      #             strong("Fig 1."), "Map of Ghana showing administrative regions")
      #         )
      #       ),
      #       box(
      #         width = NULL,
      #         title = tags$span(icon("rocket"), "Launching Process of the GAM-MAP"),
      #         status = "info",
      #         solidHeader = TRUE,
      #         collapsible = TRUE,
      #         collapsed = TRUE,
      #         includeMarkdown("www/markdown/about_page_4.Rmd")
      #       ),
      #       box(
      #         width = NULL,
      #         title = tags$span(icon("database"), "GAM-MAP Data Sources"),
      #         status = "info",
      #         solidHeader = TRUE,
      #         collapsible = TRUE,
      #         collapsed = TRUE,
      #         includeMarkdown("www/markdown/about_page_3.Rmd")
      #       )
      #     )
      #   )
      # ),
      
      # ============================================
      # HELP TAB
      # ============================================
      tabItem(
        tabName = "help",
        
        fluidRow(
          box(
            width = 12,
            background = "orange",
            div(
              style = "padding: 20px; text-align: center;",
              h2(icon("question-circle", class = "fa-2x"), "Help & Documentation",
                 style = "color: white; margin: 0;")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            box(
              width = NULL,
              title = tags$span(icon("info-circle"), "Help Overview"),
              status = "primary",
              solidHeader = TRUE,
              includeMarkdown("www/markdown/help_page_0.Rmd")
            )
          ),
          column(
            width = 8,
            box(
              width = NULL,
              title = tags$span(icon("chart-column"), "Getting Started"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              includeMarkdown("www/markdown/summary_help_1.Rmd")
            ),
            box(
              width = NULL,
              title = tags$span(icon("map"), "Navigation Guide"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              includeMarkdown("www/markdown/summary_help_2.Rmd")
            ),
            box(
              width = NULL,
              title = tags$span(icon("chart-line"), "Using the Analysis Tab"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              includeMarkdown("www/markdown/summary_help_3.Rmd")
            ),
            box(
              width = NULL,
              title = tags$span(icon("bullseye"), "Tips for Best Results"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              includeMarkdown("www/markdown/summary_help_4.Rmd")
            ),
            box(
              width = NULL,
              title = tags$span(icon("download"), "Downloading Results"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              includeMarkdown("www/markdown/summary_help_5.Rmd")
            ),
            box(
              width = NULL,
              title = tags$span(icon("envelope"), "Contact & Support"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              includeMarkdown("www/markdown/summary_help_6.Rmd")
            ),
            box(
              width = NULL,
              title = tags$span(icon("graduation-cap"), "Learning Resource"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              includeMarkdown("www/markdown/summary_help_7.Rmd")
            )
          )
        )
      ),
      
      # ============================================
      # DESCRIPTIVES TAB
      # ============================================
      tabItem(
        tabName = "descriptives",
        
        fluidRow(
          column(
            width = 3,
            box(
              width = NULL,
              title = tags$span(icon("filter"), "Filters"),
              status = "primary",
              solidHeader = TRUE,
              selectInput(
                "desc_region",
                "Select Region:",
                choices = c("Upper East", "Upper West", "Northern",
                            "Brong Ahafo", "Ashanti", "Eastern", "Volta",
                            "Greater Accra", "Central", "Western"),
                selected = "Upper East"
              ),
              selectInput(
                "year",
                "Year (Annual Stats):",
                choices  = sort(unique(data$year)),
                selected = min(data$year)
              ),
              sliderInput(
                "year_range",
                "Year Range (Overall Stats):",
                min   = min(data$year, na.rm = TRUE),
                max   = max(data$year, na.rm = TRUE),
                value = c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE)),
                step  = 1,
                sep   = ""
              )
            )
          ),
          
          column(
            width = 9,
            box(
              width = NULL,
              title = tags$span(icon("table"), "Annual Statistics"),
              status = "primary",
              solidHeader = TRUE,
              DTOutput("statsTableAnnual")
            ),
            box(
              width = NULL,
              title = tags$span(icon("table"), "Overall Statistics"),
              status = "info",
              solidHeader = TRUE,
              DTOutput("statsTableOverall")
            )
          )
        )
      ),
      
      # ============================================
      # TIME SERIES TAB
      # ============================================
      tabItem(
        tabName = "timeseries",
        
        fluidRow(
          column(
            width = 3,
            box(
              width = NULL,
              title = tags$span(icon("filter"), "Filters"),
              status = "primary",
              solidHeader = TRUE,
              selectInput(
                "ts_region",
                "Select Region:",
                choices = c("Upper East", "Upper West", "Northern",
                            "Brong Ahafo", "Ashanti", "Eastern", "Volta",
                            "Greater Accra", "Central", "Western"),
                selected = "Upper East"
              ),
              dateRangeInput(
                "dateRange",
                "Date Range:",
                start = min(data$date, na.rm = TRUE),
                end   = max(data$date, na.rm = TRUE),
                min   = min(data$date, na.rm = TRUE),
                max   = max(data$date, na.rm = TRUE)
              )
            )
          ),
          
          column(
            width = 9,
            box(
              width = NULL,
              title = tags$span(icon("line-chart"), "Time Series Decomposition"),
              status = "primary",
              solidHeader = TRUE,
              fluidRow(
                column(6, plotlyOutput("observed_plot", height = "300px")),
                column(6, plotlyOutput("trend_plot", height = "300px"))
              ),
              fluidRow(
                column(6, plotlyOutput("seasonal_plot", height = "300px")),
                column(6, plotlyOutput("remainder_plot", height = "300px"))
              )
            )
          )
        )
      ),
      
      # ============================================
      # SEASONALITY TAB
      # ============================================
      tabItem(
        tabName = "seasonality",
        
        fluidRow(
          column(
            width = 3,
            box(
              width = NULL,
              title = tags$span(icon("filter"), "Filters"),
              status = "primary",
              solidHeader = TRUE,
              selectInput(
                "season_region",
                "Select Region:",
                choices = c("Upper East", "Upper West", "Northern",
                            "Brong Ahafo", "Ashanti", "Eastern", "Volta",
                            "Greater Accra", "Central", "Western"),
                selected = "Upper East"
              )
            )
          ),
          
          column(
            width = 9,
            tabBox(
              width = NULL,
              title = tags$span(icon("calendar"), "Seasonal Patterns"),
              tabPanel(
                "Seasonality",
                fluidRow(
                  column(6, plotlyOutput("boxplot1", height = "300px")),
                  column(6, plotlyOutput("boxplot2", height = "300px"))
                ),
                fluidRow(
                  column(6, plotlyOutput("boxplot3", height = "300px")),
                  column(6, plotlyOutput("boxplot4", height = "300px"))
                ),
                fluidRow(
                  column(6, plotlyOutput("boxplot5", height = "300px")),
                  column(6, DTOutput("results_table"))
                )
              )
            )
          )
        )
      ),
      
      # ============================================
      # MODEL ESTIMATES TAB
      # ============================================
      tabItem(
        tabName = "estimates",
        
        fluidRow(
          column(
            width = 3,
            box(
              width = NULL,
              title = tags$span(icon("filter"), "Filters"),
              status = "primary",
              solidHeader = TRUE,
              selectInput(
                "model_region",
                "Select Region:",
                choices = c("Upper East", "Upper West", "Northern",
                            "Brong Ahafo", "Ashanti", "Eastern", "Volta",
                            "Greater Accra", "Central", "Western"),
                selected = "Upper East"
              )
            )
          ),
          
          column(
            width = 9,
            box(
              width = NULL,
              title = tags$span(icon("calculator"), "GAM Model Metrics"),
              status = "primary",
              solidHeader = TRUE,
              DTOutput("model_metrics")
            ),
            
            box(
              width = NULL,
              title = tags$span(icon("wave-square"), "Smooth Terms"),
              status = "info",
              solidHeader = TRUE,
              DTOutput("best_model_smooth_terms"),
              tags$hr(),
              tags$p(
                "Footnote: edf (Effective Degrees of Freedom) measures the 'wiggliness' or flexibility of a smooth function; Ref.df (Reference Degrees of Freedom) is the degrees of freedom used for hypothesis testing of the smooth term.",
                style = "color: black; font-weight: bold; font-style: italic; font-size: 12px; text-align: justify; padding: 5px 10px;"
              )
            )
          )
        )
      ),
      
      # ============================================
      # MODEL DIAGNOSTICS TAB
      # ============================================
      tabItem(
        tabName = "diagnostics",
        
        fluidRow(
          column(
            width = 3,
            box(
              width = NULL,
              title = tags$span(icon("filter"), "Filters"),
              status = "primary",
              solidHeader = TRUE,
              selectInput(
                "diag_region",
                "Select Region:",
                choices = c("Upper East", "Upper West", "Northern",
                            "Brong Ahafo", "Ashanti", "Eastern", "Volta",
                            "Greater Accra", "Central", "Western"),
                selected = "Upper East"
              )
            )
          ),
          
          column(
            width = 9,
            box(
              width = NULL,
              title = tags$span(icon("stethoscope"), "GAM Model Diagnostics"),
              status = "primary",
              solidHeader = TRUE,
              fluidRow(
                column(12, plotlyOutput("observed_fitted", height = "400px"))
              ),
              fluidRow(
                column(12, plotlyOutput("qq_plot", height = "400px"))
              ),
              fluidRow(
                column(12, plotlyOutput("response_fitted_plot", height = "400px"))
              )
            )
          )
        )
      ),
      
      # ============================================
      # MODEL PLOTS TAB
      # ============================================
      tabItem(
        tabName = "plots",
        
        fluidRow(
          column(
            width = 3,
            box(
              width = NULL,
              title = tags$span(icon("filter"), "Filters"),
              status = "primary",
              solidHeader = TRUE,
              selectInput(
                "plot_region",
                "Select Region:",
                choices = c("Upper East", "Upper West", "Northern",
                            "Brong Ahafo", "Ashanti", "Eastern", "Volta",
                            "Greater Accra", "Central", "Western"),
                selected = "Upper East"
              )
            )
          ),
          
          column(
            width = 9,
            box(
              width = NULL,
              title = tags$span(icon("chart-area"), "Model Smooth Terms Plots"),
              status = "primary",
              solidHeader = TRUE,
              uiOutput("dynamic_plots"),
              tags$hr(),
              tags$p(
                "Footnote: Partial effects represent the isolated contribution of each predictor to the outcome (uncomplicated malaria cases), holding all other variables constant. Effects are centered at zero and expressed on the scale of the linear predictor.",
                style = "font-size: 12px; color: black; font-weight: bold; font-style: italic; text-align: justify; padding: 5px 10px 5px 10px;"
              )
            )
          )
        )
      ),
      
      # ============================================
      # APPENDIX TAB
      # ============================================
      tabItem(
        tabName = "appendix",
        
        fluidRow(
          box(
            width = 12,
            background = "purple",
            div(
              style = "padding: 20px; text-align: center;",
              h2(icon("book", class = "fa-2x"), "Appendix & Additional Resources",
                 style = "color: white; margin: 0;")
            )
          )
        ),
        
        fluidRow(
          # Filter sidebar
          column(
            width = 3,
            # Line Plots filter 
            conditionalPanel(
              condition = "input.appendix_tabs == 'Line Plots'",
              box(
                width = NULL,
                title = tags$span(icon("filter"), "Filters"),
                status = "primary",
                solidHeader = TRUE,
                dateRangeInput(
                  "combined_dateRange",
                  "Date Range:",
                  start = min(data$date, na.rm = TRUE),
                  end   = max(data$date, na.rm = TRUE),
                  min   = min(data$date, na.rm = TRUE),
                  max   = max(data$date, na.rm = TRUE)
                ),
                hr(),
                downloadButton(
                  "download_combined_plot",
                  label = "Download",
                  style = "width: 100%; font-weight: bold;"
                )
              )
            ),
            
            # Heatmaps filter 
            conditionalPanel(
              condition = "input.appendix_tabs == 'Heatmaps'",
              box(
                width = NULL,
                title = tags$span(icon("filter"), "Filters"),
                status = "primary",
                solidHeader = TRUE,
                dateRangeInput(
                  "heatmap_dateRange",
                  "Date Range:",
                  start = min(data$date, na.rm = TRUE),
                  end   = max(data$date, na.rm = TRUE),
                  min   = min(data$date, na.rm = TRUE),
                  max   = max(data$date, na.rm = TRUE)
                ),
                hr(),                                          
                downloadButton(                                
                  "download_heatmap",
                  label = "Download",
                  style = "width: 100%; font-weight: bold;"
                )
              )
            ),
            
            # Seasonal Pattern filter 
            conditionalPanel(
              condition = "input.appendix_tabs == 'Seasonal Pattern'",
              box(
                width = NULL,
                title = tags$span(icon("filter"), "Filters"),
                status = "primary",
                solidHeader = TRUE,
                selectInput(
                  "region",
                  "Region:",
                  choices = c(
                    "Upper East", "Upper West", "Northern",
                    "Brong Ahafo", "Ashanti", "Eastern", "Volta",
                    "Greater Accra", "Central", "Western"
                  ),
                  selected = "Upper East"
                ),
                dateRangeInput(
                  "seasonal_dateRange",
                  "Date Range (full years only):",
                  start = min(data$date, na.rm = TRUE),
                  end   = max(data$date, na.rm = TRUE),
                  min   = min(data$date, na.rm = TRUE),
                  max   = max(data$date, na.rm = TRUE),
                  format = "yyyy"      
                ),
                hr(),
                downloadButton(
                  "download_plot",
                  label = "Download",
                  style = "width: 100%; font-weight: bold;"
                )
              )
            ) 
          ),
          
          # Main content area
          column(
            width = 9,
            tabBox(
              width = 12,
              id = "appendix_tabs",  
              title = "Additional Content",
              tabPanel(
                "Line Plots",
                plotlyOutput("combined_plot", height = "800px")
              ),
              tabPanel(
                "Seasonal Pattern",
                plotOutput("malaria_plot", height = "600px")
              ),
              tabPanel(
                "Heatmaps",
                plotOutput("combined_heatmap", height = "800px"),
              ),
              tabPanel(
                "GAM Framework", 
                includeMarkdown("www/markdown/gam model building.md"),
                tags$div(
                  style = "text-align: center; font-size: 16px;",    
                  tags$img( src = "./images/GAM_algorithm.png", 
                            height = "auto",  
                            width = "100%",
                            alt = "GAM framework diagram"
                            )
                  )
                )
            )
          )
        )
      ),
      
      # ============================================
      # CODE TAB ITEM 
      # ============================================
      tabItem(
        tabName = "code_tab",
        
        fluidRow(
          box(
            width = 12,
            div(
              id = "code-panel",
              
              # Page Header
              div(
                class = "output-card",
                style = "background: linear-gradient(135deg, #24292e 0%, #000000 100%);
                   color: white; padding: 50px; margin-bottom: 30px; text-align: center;",
                
                h2(
                  style = "margin: 0 0 20px 0;",
                  icon("github", class = "fa-3x"),
                  " View Source Code"
                ),
                
                p(
                  style = "font-size: 1.25rem; opacity: 0.9;",
                  "Access the complete R code repository on GitHub"
                ),
                
                tags$a(
                  href   = "https://github.com/EdwardAkurugu/GAM-MAP",
                  target = "_blank",
                  class  = "btn btn-primary",
                  style  = "font-size: 1.25rem; padding: 15px 40px; margin-top: 20px;",
                  icon("github", class = "fa-lg"),
                  " Visit GitHub Repository"
                )
              )
            )
          )
        )
      )
    )
  )
)
