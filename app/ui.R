# ================================================================================================
# GAM(M)-MAP SHINY USER INTERFACE
# ================================================================================================

# =====================================================================
# STEP PROGRESS STRIP
# =====================================================================
step_progress <- function(current_step, total = 7) {
  steps <- c("Descriptives","Time Series","Seasonality",
             "Estimates","Diagnostics","GAM Plots","Summary")
  items <- lapply(seq_len(total), function(i) {
    cls <- if (i < current_step) "analysis-step done"
    else if (i == current_step) "analysis-step active"
    else "analysis-step"
    tags$div(
      class = cls,
      style = if (i < current_step) "opacity:0.95;" else "",
      tags$span(class = "step-num", i),
      steps[i]
    )
  })
  div(class = "analysis-stepper", style = "margin-bottom:18px;", tagList(items))
}


# =============================================================================================
# USER INTERFACE DEFINITION
# =============================================================================================
ui <- dashboardPage(
  skin = "purple",
  
  # ============================================================
  # DASHBOARD HEADER
  # ============================================================
  dashboardHeader(
    title = tags$span("GAM(M)-MAP",
                      style = "color:white;font-size:3.5rem;margin-top:20px;opacity:0.95;"),
    titleWidth = 250,
    tags$li(class="dropdown", style="margin-top:16px;margin-right:4px;",
            tags$span(style="color:rgba(255,255,255,0.80);font-size:20px;font-weight:700;",
                      textOutput("data_provenance", inline=TRUE))),
    tags$li(class="dropdown", style="margin-top:8px;margin-right:5px;",
            tags$a(href="https://github.com/EdwardAkurugu/GAM-MAP", target="_blank",
                   class="header-action-btn", icon("github"), " Source Code")),
    tags$li(class="dropdown", style="margin-top:8px;margin-right:10px;",
            actionLink("header_help", label=tagList(icon("question-circle")," Help"),
                       class="header-action-btn"))
  ),
  
  # ============================================================
  # DASHBOARD SIDEBAR
  # ============================================================
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem(tags$span(tags$span(class="sidebar-step-badge","★"),"Welcome"),
               tabName="welcome", icon=NULL),
      menuItem(tags$span(tags$span(class="sidebar-step-badge","i"),"About"),
               tabName="about", icon=NULL),
      menuItem(tags$span(tags$span(class="sidebar-step-badge","?"),"Help"),
               tabName="help", icon=NULL),
      
      # ---------------- Analysis (collapsible) ----------------
      menuItem(tags$span(tags$span(class="sidebar-step-badge",icon("chart-bar")),"Basic Analysis"),
               tabName = NULL, icon = NULL, startExpanded = FALSE,
               menuSubItem(tags$span(tags$span(class="sidebar-step-badge","1"),"Descriptives"),
                           tabName="descriptives", icon=NULL),
               menuSubItem(tags$span(tags$span(class="sidebar-step-badge","2"),"Time Series"),
                           tabName="timeseries", icon=NULL),
               menuSubItem(tags$span(tags$span(class="sidebar-step-badge","3"),"Seasonality"),
                           tabName="seasonality", icon=NULL)
      ),
      
      # ---------------- GAM Analysis (collapsible) ----------------
      menuItem(tags$span(tags$span(class="sidebar-step-badge",icon("wave-square")),"GAM Analysis"),
               tabName = NULL, icon = NULL, startExpanded = FALSE,
               menuSubItem(tags$span(tags$span(class="sidebar-step-badge","4"),"GAM Estimates"),
                           tabName="estimates", icon=NULL),
               menuSubItem(tags$span(tags$span(class="sidebar-step-badge","5"),"GAM Diagnostics"),
                           tabName="diagnostics", icon=NULL),
               menuSubItem(tags$span(tags$span(class="sidebar-step-badge","6"),"GAM Plots"),
                           tabName="plots", icon=NULL),
               menuSubItem(tags$span(tags$span(class="sidebar-step-badge","7"),"GAM Summary"),
                           tabName="regional_summary", icon=NULL)
      ),
      
      # ---------------- Model Validity (standalone, no children) ----------------
      menuItem(tags$span(tags$span(class="sidebar-step-badge", icon("shield-halved")),"Model Validity"),
               tabName="model_status", icon=NULL),
      
      # ---------------- GAMM Analysis (collapsible) ----------------
      menuItem(tags$span(tags$span(class="sidebar-step-badge",icon("wave-square")),"GAMM Analysis"),
               tabName = NULL, icon = NULL, startExpanded = FALSE,
               menuSubItem(tags$span(tags$span(class="sidebar-step-badge","4G"),"GAMM Estimates"),
                           tabName="gamm_estimates", icon=NULL),
               menuSubItem(tags$span(tags$span(class="sidebar-step-badge","5G"),"GAMM Diagnostics"),
                           tabName="gamm_diagnostics", icon=NULL),
               menuSubItem(tags$span(tags$span(class="sidebar-step-badge","6G"),"GAMM Plots"),
                           tabName="gamm_plots", icon=NULL),
               menuSubItem(tags$span(tags$span(class="sidebar-step-badge","7G"),"GAMM Summary"),
                           tabName="gamm_regional_summary", icon=NULL)
      ),
      
      # ---------------- More (collapsible) ----------------
      menuItem(tags$span(tags$span(class="sidebar-step-badge","+"),"More"),
               tabName = NULL, icon = NULL, startExpanded = FALSE,
               menuSubItem(tags$span(tags$span(class="sidebar-step-badge","A"),"Appendix"),
                           tabName="appendix", icon=NULL),
               menuSubItem(tags$span(tags$span(class="sidebar-step-badge",HTML("&#60;/&#62;")),"Source Code"),
                           tabName="code_tab", icon=NULL)
      )
    )
  ),
  
  # ============================================================
  # DASHBOARD BODY
  # ============================================================
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML(dashboardCSS)),
      tags$link(rel="stylesheet",
                href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
      # Section-tab-bar switching
      tags$script(HTML("
        $(document).on('click', '.stab', function(){
          var target = $(this).data('target');
          var group  = $(this).data('group');
          $('.stab[data-group=\"' + group + '\"]').removeClass('active');
          $('.stab-panel[data-group=\"' + group + '\"]').removeClass('active');
          $(this).addClass('active');
          $('.stab-panel[data-panel=\"' + target + '\"][data-group=\"' + group + '\"]').addClass('active');
          $(window).trigger('resize');
          setTimeout(function(){
           $(window).trigger('resize');
          }, 50);
          setTimeout(function(){
            $($.fn.dataTable.tables(true)).DataTable().columns.adjust().draw();
          }, 400);
        });
      ")),
      # Sync JS tabs to Shiny inputs
      tags$script(HTML("
        $(document).on('shiny:connected', function(){
          Shiny.setInputValue('desc_tab',      'd_annual',     {priority:'event'});
          Shiny.setInputValue('season_tab',    's_plots',      {priority:'event'});
          Shiny.setInputValue('est_tab',       'e_metrics',    {priority:'event'});
          Shiny.setInputValue('diag_tab',      'diag1',        {priority:'event'});
          Shiny.setInputValue('help_tab',      'h1',           {priority:'event'});
          Shiny.setInputValue('rsub_tab',      'rs_summary',   {priority:'event'});
          Shiny.setInputValue('map_inner_tab', 'mi_map',       {priority:'event'});
          Shiny.setInputValue('cmp_inner_tab', 'cp_ts',        {priority:'event'});
          Shiny.setInputValue('gest_tab',      'ge_metrics',   {priority:'event'});
          Shiny.setInputValue('gdiag_tab',     'gdiag1',       {priority:'event'});
          Shiny.setInputValue('gsub_tab',       'grs_summary', {priority:'event'});
          Shiny.setInputValue('gmap_inner_tab', 'gmi_map',     {priority:'event'});
          Shiny.setInputValue('gcmp_inner_tab', 'gci_ts',      {priority:'event'});
          Shiny.setInputValue('mv_tab',         'mv_summary',  {priority:'event'});

        });
        $(document).on('click', '.stab', function(){
          var target = $(this).data('target');
          var group  = $(this).data('group');
          Shiny.setInputValue(group + '_tab', target, {priority:'event'});
        });
      ")),
      # Step strip click navigation
      tags$script(HTML("
        $(document).on('click', '.analysis-step', function(){
          var num = $(this).find('.step-num').text().trim();
          Shiny.setInputValue('step_click', num, {priority:'event'});
        });
      ")),
      tags$style(HTML("
        .analysis-step { font-size:16px!important; font-weight:600!important; margin-right:12px; cursor:pointer; }
        .analysis-step .step-num { font-size:16px!important; font-weight:700!important; }
        .yoy-badge { display:inline-flex;align-items:center;gap:6px;padding:5px 12px;border-radius:20px;
                     font-size:13px;font-weight:600;margin-bottom:10px; }
        .yoy-up          { background:#FCEBEB;color:#A32D2D; }
        .yoy-down        { background:#EAF3DE;color:#3B6D11; }
        .yoy-stable      { background:#F1EFE8;color:#5F5E5A; }
        .threshold-alert { padding:10px 16px;border-radius:8px;font-size:14px;
                           font-weight:600;margin-bottom:14px;display:flex;align-items:center;gap:8px; }
        .alert-high      { background:#FCEBEB;color:#A32D2D;border-left:4px solid #A32D2D; }
        .alert-low       { background:#EAF3DE;color:#3B6D11;border-left:4px solid #3B6D11; }
        .alert-normal    { background:#E6F1FB;color:#185FA5;border-left:4px solid #185FA5; }
      "))
    ),
    
    # Loading overlay
    div(id="loading-overlay", class="loading-overlay",
        includeHTML("www/markdown/loading_page.Rhtml"),
        h2(tags$em("Please Wait..."))),
    
    tabItems(
      
      # ======================================================
      # WELCOME
      # ======================================================
      
      tabItem(tabName="welcome",
              fluidRow(box(width=12,
                           div(class="hero-band",
                               div(class="logo-row",
                                   tags$a(href="https://masha.uct.ac.za", target="_blank",
                                          tags$span(id="logo_masha", tags$img(src="./images/logo_masha.png", alt="MASHA"))),
                                   bsTooltip("logo_masha","Click to visit MASHA website",placement="bottom"),
                                   tags$a(href="https://www.uct.ac.za", target="_blank",
                                          tags$span(id="logo_uct", tags$img(src="./images/logo_uct.png", alt="UCT"))),
                                   bsTooltip("logo_uct","Click to visit University of Cape Town website",placement="bottom"),
                                   tags$a(href="https://science.uct.ac.za/departments/statistical-sciences", target="_blank",
                                          tags$span(id="logo_stats", tags$img(src="./images/Statistical Science.jpg", alt="Stats Science"))),
                                   bsTooltip("logo_stats","Click to visit Department of Statistical Sciences, UCT",placement="bottom"),
                                   tags$a(href="https://ghs.gov.gh/", target="_blank",
                                          tags$span(id="logo_ghs", tags$img(src="./images/GHS_logo.jpg", alt="Ghana Health Service"))),
                                   bsTooltip("logo_ghs","Click to visit Ghana Health Service website",placement="bottom")),
                               div(style="flex:1;text-align:left;",
                                   h1(icon("bug",class="fa-2x"), icon("disease",class="fa-2x",style="margin-left:10px;"),
                                      "GAM(M)-Based Malaria Analytics Platform [GAM(M)-MAP]",
                                      style="color:white;font-size:2.20rem;margin:0;"),
                                   p("Temporal and Climatic Analysis of Uncomplicated Malaria in Ghana",
                                     style="color:white;font-size:2.27rem;margin-top:15px;opacity:0.95;"))))),
              tags$br(),
              fluidRow(
                box(width=4,
                    div(class="content-card card-accent-primary",
                        div(class="content-card-header", icon("info-circle"),"Quick Overview"),
                        div(class="content-card-body", div(style="text-align:justify;",
                                                           includeMarkdown("www/markdown/welcome_page_0.md"))))),
                box(width=8,
                    div(style="margin-top:20px;",
                        div(class="section-tab-bar",
                            tags$a(class="stab active", `data-target`="w_platform", `data-group`="welcome",
                                   icon("chart-line"),"The Platform"),
                            tags$a(class="stab", `data-target`="w_process", `data-group`="welcome",
                                   icon("cogs"),"Analysis Process"),
                            tags$a(class="stab", `data-target`="w_start", `data-group`="welcome",
                                   icon("play-circle"),"Getting Started"),
                            tags$a(class="stab", `data-target`="w_gam", `data-group`="welcome",
                                   icon("graduation-cap"),"Learning GAM")),
                        div(class="stab-panel active", `data-panel`="w_platform", `data-group`="welcome",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("chart-line"),"The GAM(M)-Based Malaria Analytics Platform"),
                                div(class="content-card-body", div(style="text-align:justify;",
                                                                   includeMarkdown("www/markdown/welcome_page_1.Rmd"))))),
                        div(class="stab-panel", `data-panel`="w_process", `data-group`="welcome",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("cogs"),"Analysis Process"),
                                div(class="content-card-body", div(style="text-align:justify;",
                                                                   includeMarkdown("www/markdown/welcome_page_2.Rmd"))))),
                        div(class="stab-panel", `data-panel`="w_start", `data-group`="welcome",
                            div(class="content-card card-accent-success",
                                div(class="content-card-header",icon("play-circle"),"Getting Started"),
                                div(class="content-card-body",
                                    h4(icon("rocket"),"How to Use This Application"),
                                    tags$ol(
                                      tags$li(strong("Application Loads:")," Wait 1-5 seconds for data and models to load"),
                                      tags$li(strong("Select a Region:")," Navigate to the Analysis section and choose a region"),
                                      tags$li(strong("Explore Descriptives:")," Review summary statistics, descriptive tables and correlation"),
                                      tags$li(strong("Examine Temporal Patterns:")," Use Time Series and Seasonality tabs"),
                                      tags$li(strong("Evaluate Models:")," Review GAM/GAMM estimates (with forecast), diagnostics, and smooth term plots"),
                                      tags$li(strong("Regional Summary:")," Review synthesised findings, map view, and region comparisons"))))),
                        div(class="stab-panel", `data-panel`="w_gam", `data-group`="welcome",
                            div(class="content-card card-accent-warning",
                                div(class="content-card-header",icon("graduation-cap"),"Learning about GAM"),
                                div(class="content-card-body", div(style="text-align:justify;",
                                                                   includeMarkdown("www/markdown/welcome_page_3.Rmd"))))),
                        div(class="next-step-bar",
                            div(tags$strong("Ready to explore?"),
                                p("Navigate to Step 1 — Descriptives — to begin your analysis.")),
                            actionButton("go_descriptives","Start Analysis →", class="nav-next-btn",
                                         onclick="Shiny.setInputValue('tabs','descriptives',{priority:'event'})")),
                        div(class="logo-gammap", style="text-align:right;margin-top:0px;",
                            includeHTML("www/markdown/gam(m)_map_logo.htm")))))),
      
      # ======================================================
      # ABOUT
      # ======================================================
      
      tabItem(tabName="about",
              fluidRow(box(width=12,
                           div(class="page-header-band", icon("info-circle",class="fa-4x",style="margin-left:0px;"),
                               div(h2("About the GAM(M)-MAP"))))),
              fluidRow(
                box(width=3,
                    div(class="content-card card-accent-primary",
                        div(class="content-card-header",icon("info-circle"),"Overview"),
                        div(class="content-card-body", div(style="text-align:justify;",
                                                           includeMarkdown("www/markdown/about_page_0.Rmd"))))),
                box(width=9,
                    div(style="margin-top:20px;",
                        div(class="section-tab-bar",
                            tags$a(class="stab active",`data-target`="ab_obj",  `data-group`="about",icon("bullseye"),  "Objectives"),
                            tags$a(class="stab",       `data-target`="ab_map",  `data-group`="about",icon("map"),       "Study Area"),
                            tags$a(class="stab",       `data-target`="ab_launch",`data-group`="about",icon("rocket"),   "Launching"),
                            tags$a(class="stab",       `data-target`="ab_data", `data-group`="about",icon("database"),  "Data Sources"),
                            tags$a(class="stab",       `data-target`="ab_team", `data-group`="about",icon("users"),     "Project Team")),
                        div(class="stab-panel active",`data-panel`="ab_obj",`data-group`="about",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("bullseye"),"Objectives"),
                                div(class="content-card-body",div(style="text-align:justify;",includeMarkdown("www/markdown/about_page_1.Rmd"))))),
                        div(class="stab-panel",`data-panel`="ab_map",`data-group`="about",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("map"),"Study Area: Ghana's Ecological Zones"),
                                div(class="content-card-body",
                                    div(style="text-align:center;padding:20px;",
                                        img(src='./images/Ghana_ecological_zones.png',
                                            style='max-width:80%;height:auto;border-radius:5px;'),
                                        p(style="margin-top:15px;font-style:italic;",
                                          "Map of Ghana showing administrative regions"))))),
                        div(class="stab-panel",`data-panel`="ab_launch",`data-group`="about",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("rocket"),"Launching Process"),
                                div(class="content-card-body",div(style="text-align:justify;",includeMarkdown("www/markdown/about_page_2.Rmd"))))),
                        div(class="stab-panel",`data-panel`="ab_data",`data-group`="about",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("database"),"Data Sources"),
                                div(class="content-card-body",div(style="text-align:justify;",includeMarkdown("www/markdown/about_page_3.Rmd"))))),
                        div(class="stab-panel",`data-panel`="ab_team",`data-group`="about",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("users"),"Project Team"),
                                div(class="content-card-body",includeMarkdown("www/markdown/about_page_4.Rmd")))))))),
      
      # ======================================================
      # HELP
      # ======================================================
      
      tabItem(tabName="help",
              fluidRow(box(width=12,
                           div(class="page-header-band",icon("question-circle",class="fa-4x",style="margin-left:0px;"),
                               div(h2("Help & Documentation"))))),
              fluidRow(
                box(width=3,
                    div(class="content-card card-accent-primary",
                        div(class="content-card-header",icon("info-circle"),"Help Overview"),
                        div(class="content-card-body",div(style="text-align:justify;",
                                                          includeMarkdown("www/markdown/help_page_0.Rmd"))))),
                box(width=9,
                    div(style="margin-top:20px;",
                        div(class="section-tab-bar",
                            tags$a(class="stab active",`data-target`="h1",`data-group`="help", icon("chart-column"), "Getting Started"),
                            tags$a(class="stab",       `data-target`="h2",`data-group`="help", icon("map"),           "Navigation"),
                            tags$a(class="stab",       `data-target`="h3",`data-group`="help", icon("chart-line"),    "Analysis Tab"),
                            tags$a(class="stab",       `data-target`="h4",`data-group`="help", icon("bullseye"),      "Best Results"),
                            tags$a(class="stab",       `data-target`="h5",`data-group`="help", icon("download"),      "Downloads"),
                            tags$a(class="stab",       `data-target`="h6",`data-group`="help", icon("envelope"),      "Contact"),
                            tags$a(class="stab",       `data-target`="h7",`data-group`="help", icon("graduation-cap"),"Resources")),
                        div(class="stab-panel active", `data-panel` ="h1",`data-group`="help",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("chart-column"),"Getting Started"),
                                div(class="content-card-body",div(style="text-align:justify;",includeMarkdown("www/markdown/help_page_1.Rmd"))))),
                        div(class="stab-panel",`data-panel`="h2",`data-group`="help",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("map"),"Navigation Guide"),
                                div(class="content-card-body",div(style="text-align:justify;",includeMarkdown("www/markdown/help_page_2.Rmd"))))),
                        div(class="stab-panel",`data-panel`="h3",`data-group`="help",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("chart-line"),"Using the Analysis Tab"),
                                div(class="content-card-body",div(style="text-align:justify;",includeMarkdown("www/markdown/help_page_3.Rmd"))))),
                        div(class="stab-panel",`data-panel`="h4",`data-group`="help",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("bullseye"),"Tips for Best Results"),
                                div(class="content-card-body",div(style="text-align:justify;",includeMarkdown("www/markdown/help_page_4.Rmd"))))),
                        div(class="stab-panel",`data-panel`="h5",`data-group`="help",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("download"),"Downloading Results"),
                                div(class="content-card-body",div(style="text-align:justify;",includeMarkdown("www/markdown/help_page_5.Rmd"))))),
                        div(class="stab-panel",`data-panel`="h6",`data-group`="help",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("envelope"),"Contact & Support"),
                                div(class="content-card-body",div(style="text-align:justify;",includeMarkdown("www/markdown/help_page_6.Rmd"))))),
                        div(class="stab-panel",`data-panel`="h7",`data-group`="help",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("graduation-cap"),"Learning Resources"),
                                div(class="content-card-body",div(style="text-align:justify;",includeMarkdown("www/markdown/help_page_7.Rmd"))))))))),
      
      # ======================================================
      # DESCRIPTIVES
      # ======================================================
      
      tabItem(tabName = "descriptives",
              fluidRow(
                box(width = 12,
                    div(class = "page-header-band",
                        icon("table", class = "fa-4x", style = "margin-left:0px;"),
                        div(h2("Step 1 — Descriptive Statistics"))))
              ),
              step_progress(1),
              fluidRow(
                # --- Left filter panel ---
                box(width = 3,
                    div(class = "filter-panel",
                        div(class = "filter-panel-title", icon("filter"), "Filters"),
                        
                        # Annual filters
                        conditionalPanel("input.desc_tab == 'd_annual'",
                                         selectInput("annual_region", "Region",
                                                     choices = c("Upper East","Upper West","Northern","Brong Ahafo",
                                                                 "Ashanti","Eastern","Volta","Greater Accra","Central","Western"),
                                                     selected = "Upper East"),
                                         selectInput("annual_year", "Year",
                                                     choices = sort(unique(data$year)), selected = min(data$year)),
                                         actionButton("reset_desc", "Reset Filters", icon = icon("redo"),
                                                      class = "btn btn-warning", style = "margin-top:10px;"),
                                         tags$br(),
                                         tags$br(),
                                         uiOutput("yoy_badge"),
                                         uiOutput("threshold_alert"),
                                         div(class = "instruction-card",
                                             strong("Step 1 of 7."), " Review annual summary statistics.",
                                             " The badge shows year-over-year change; the alert flags unusual burden levels.")
                        ),
                        
                        # Overall filters
                        conditionalPanel("input.desc_tab == 'd_overall'",
                                         selectInput("overall_region", "Region",
                                                     choices = c("Upper East","Upper West","Northern","Brong Ahafo",
                                                                 "Ashanti","Eastern","Volta","Greater Accra","Central","Western"),
                                                     selected = "Upper East"),
                                         sliderInput("overall_year_range", "Year Range",
                                                     min = min(data$year, na.rm = TRUE), max = max(data$year, na.rm = TRUE),
                                                     value = c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE)),
                                                     step = 1, sep = ""),
                                         actionButton("reset_overall", "Reset Filters", icon = icon("redo"),
                                                      class = "btn btn-warning", style = "margin-top:10px;"),
                                         tags$br(),
                                         tags$br(),
                                         div(class = "instruction-card",
                                             strong("Step 1 of 7."), " Review overall statistics across multiple years.",
                                             " Use the slider to narrow the period of interest.")
                        ),
                        
                        # Correlation filters
                        conditionalPanel("input.desc_tab == 'd_corr'",
                                         selectInput("corr_region", "Region",
                                                     choices = c("Upper East","Upper West","Northern","Brong Ahafo",
                                                                 "Ashanti","Eastern","Volta","Greater Accra","Central","Western"),
                                                     selected = "Upper East"),
                                         sliderInput("corr_year_range", "Year Range",
                                                     min = min(data$year, na.rm = TRUE), max = max(data$year, na.rm = TRUE),
                                                     value = c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE)),
                                                     step = 1, sep = ""),
                                         actionButton("reset_corr", "Reset Filters", icon = icon("redo"),
                                                      class = "btn btn-warning", style = "margin-top:10px;"),
                                         tags$br(),
                                         tags$br(),
                                         div(class = "instruction-card",
                                             strong("Correlation."), " Scatter plots show the association between climate variables",
                                             " and malaria cases. The matrix shows pairwise relationships.",
                                             " Pearson and Spearman coefficients are reported in the table.")
                        ),
                        
                        # Map View filters
                        conditionalPanel("input.desc_tab == 'd_map'",
                                         selectInput("map_year", "Year",
                                                     choices  = sort(unique(data$year), decreasing = TRUE),
                                                     selected = max(data$year, na.rm = TRUE)),
                                         selectInput("map_month", "Month",
                                                     choices  = c("All"),
                                                     selected = "All"),
                                         actionButton("reset_map", "Reset Filters", icon = icon("redo"),
                                                      class = "btn btn-warning", style = "margin-top:10px;"),
                                         tags$br(),
                                         tags$br(),
                                         div(class = "instruction-card",
                                             strong("Map View."),
                                             " Circle size and colour both encode malaria case burden.",
                                             " Click any circle for a region summary.",
                                             " Filter by year to explore the disease risk.")
                        ),
                        
                        # Compare Regions filters
                        conditionalPanel("input.desc_tab == 'd_compare'",
                                         selectInput("compare_region1", "Region 1",
                                                     choices  = c("Upper East","Upper West","Northern","Brong Ahafo",
                                                                  "Ashanti","Eastern","Volta","Greater Accra","Central","Western"),
                                                     selected = "Upper East"),
                                         selectInput("compare_region2", "Region 2",
                                                     choices  = c("Upper East","Upper West","Northern","Brong Ahafo",
                                                                  "Ashanti","Eastern","Volta","Greater Accra","Central","Western"),
                                                     selected = "Upper West"),
                                         sliderInput("compare_year_range", "Year Range",
                                                     min   = min(data$year, na.rm = TRUE),
                                                     max   = max(data$year, na.rm = TRUE),
                                                     value = c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE)),
                                                     step  = 1, sep = ""),
                                         actionButton("reset_compare", "Reset Filters", icon = icon("redo"),
                                                      class = "btn btn-warning", style = "margin-top:10px;"),
                                         tags$br(),
                                         tags$br(),
                                         div(class = "instruction-card",
                                             strong("Compare Regions."),
                                             " Select two regions to compare malaria incidence side-by-side.",
                                             " The time series overlay shows relative trends;",
                                             " the bar chart shows annual totals.")
                        ) 
                    )
                ),
                
                # --- Right content panels ---
                box(width = 9,
                    div(style = "margin-top:20px;",
                        div(class = "section-tab-bar",
                            tags$a(class = "stab active", `data-target` = "d_annual", `data-group` = "desc",
                                   icon("table"), "Annual Statistics"),
                            tags$a(class = "stab", `data-target` = "d_overall", `data-group` = "desc",
                                   icon("table"), "Overall Statistics"),
                            tags$a(class = "stab", `data-target` = "d_corr", `data-group` = "desc",
                                   icon("circle-dot"), "Correlation"),
                            tags$a(class = "stab", `data-target` = "d_map", `data-group` = "desc",
                                   icon("map"), "Map View"),
                            tags$a(class = "stab", `data-target` = "d_compare", `data-group` = "desc",
                                   icon("code-compare"), "Compare Regions")),
                        # Annual panel
                        div(class = "stab-panel active", `data-panel` = "d_annual", `data-group` = "desc",
                            div(class = "content-card card-accent-primary",
                                div(class = "content-card-header", icon("table"), "Annual Descriptive Statistics"),
                                div(class = "content-card-body",
                                    withSpinner(DTOutput("statsTableAnnual"), type = 8)))),
                        
                        # Overall panel
                        div(class = "stab-panel", `data-panel` = "d_overall", `data-group` = "desc",
                            div(class = "content-card card-accent-info",
                                div(class = "content-card-header", icon("table"), "Overall Descriptive Statistics"),
                                div(class = "content-card-body",
                                    withSpinner(DTOutput("statsTableOverall"), type = 8)))),
                        
                        # Correlation panel
                        div(class = "stab-panel", `data-panel` = "d_corr", `data-group` = "desc",
                            div(class = "content-card card-accent-info",
                                div(class = "content-card-header", icon("circle-dot"), "Correlation Analysis"),
                                div(class = "content-card-body",
                                    fluidRow(
                                      column(6, withSpinner(plotlyOutput("scatter_rain_malaria"), type = 8)),
                                      column(6, withSpinner(plotlyOutput("scatter_temp_malaria"), type = 8))
                                    ),
                                    fluidRow(
                                      column(6, withSpinner(plotlyOutput("corr_matrix_plot"), type = 8)),
                                      column(6, withSpinner(DTOutput("corr_table"), type = 8)),
                                    ),
                                    tags$p("Footnote: uncom = Uncomplicated malaria; mintem = Minimum temperature;
                                           mintem = Mininum temperature; avgtemp = Average temperature; maxtem = Maximum temperature;",
                                           style="color:black;font-weight:bold;font-style:italic;font-size:16px;text-align:justify;padding:5px 10px;")
                                )
                            )
                        ),
                        
                        # Map View panel
                        div(class = "stab-panel", `data-panel` = "d_map", `data-group` = "desc",
                            div(class = "content-card card-accent-primary",
                                div(class = "content-card-header", icon("map"), "Ghana — Malaria Incidence Map"),
                                div(class = "content-card-body",
                                    div(class = "section-tab-bar",
                                        tags$a(class = "stab active", `data-target` = "mi_map",  `data-group` = "map_inner",
                                               icon("map"), "Map"),
                                        tags$a(class = "stab",       `data-target` = "mi_table", `data-group` = "map_inner",
                                               icon("table"), "Summary Table")),
                                    div(class = "stab-panel active", `data-panel` = "mi_map", `data-group` = "map_inner",
                                        withSpinner(leafletOutput("ghana_map", height = "500px"), type = 8)),
                                    div(class = "stab-panel", `data-panel` = "mi_table", `data-group` = "map_inner",
                                        withSpinner(DTOutput("map_summary_table"), type = 8))))),
                        
                        # Compare Regions panel
                        div(class = "stab-panel", `data-panel` = "d_compare", `data-group` = "desc",
                            div(class = "content-card card-accent-info",
                                div(class = "content-card-header", icon("code-compare"), "Year-over-Year Region Comparison"),
                                div(class = "content-card-body",
                                    div(class = "section-tab-bar",
                                        tags$a(class = "stab active", `data-target` = "cp_ts",    `data-group` = "cmp_inner",
                                               icon("chart-line"), "Time Series"),
                                        tags$a(class = "stab",       `data-target` = "cp_bar",   `data-group` = "cmp_inner",
                                               icon("chart-bar"), "Annual Bar Chart"),
                                        tags$a(class = "stab",       `data-target` = "cp_table", `data-group` = "cmp_inner",
                                               icon("table"), "Summary Table")),
                                    div(class = "stab-panel active", `data-panel` = "cp_ts", `data-group` = "cmp_inner",
                                        withSpinner(plotlyOutput("compare_ts_plot", height = "400px"), type = 8)),
                                    div(class = "stab-panel", `data-panel` = "cp_bar", `data-group` = "cmp_inner",
                                        withSpinner(plotlyOutput("compare_bar_plot", height = "400px"), type = 8)),
                                    div(class = "stab-panel", `data-panel` = "cp_table", `data-group` = "cmp_inner",
                                        withSpinner(DTOutput("compare_summary_table"), type = 8))))),  
                        
                        # Next-step bar
                        div(class="next-step-bar",
                            div(tags$strong("Next: Time Series Analysis"),
                                p("Explore trends and decomposition of malaria cases over time.")),
                            actionButton("go_ts","Time Series →",class="nav-next-btn",
                                         onclick="Shiny.setInputValue('tabs','timeseries',{priority:'event'})"))
                    )
                )
              )
      ),
      
      # ======================================================
      # TIME SERIES
      # ======================================================
      
      tabItem(tabName="timeseries",
              fluidRow(box(width=12,
                           div(class="page-header-band",icon("chart-line",class="fa-4x",style="margin-left:0px;"),
                               div(h2("Step 2 — Time Series Decomposition"))))),
              step_progress(2),
              fluidRow(
                box(width=3,
                    div(class="filter-panel",
                        div(class="filter-panel-title",icon("filter"),"Filters"),
                        selectInput("ts_region","Region",
                                    choices=c("Upper East","Upper West","Northern","Brong Ahafo",
                                              "Ashanti","Eastern","Volta","Greater Accra","Central","Western"),
                                    selected="Upper East"),
                        dateRangeInput("dateRange","Date Range",
                                       start=min(data$date,na.rm=TRUE), end=max(data$date,na.rm=TRUE),
                                       min=min(data$date,na.rm=TRUE),   max=max(data$date,na.rm=TRUE)),
                        
                        conditionalPanel("output.short_range == true",
                                         p(style="font-size:13px;color:#a32d2d;margin-top:4px;",
                                           em("Note: Range must be at least two full years"))
                        ),
                        
                        actionButton("reset_ts","Reset Filters",icon=icon("redo"),
                                     class="btn btn-warning",style="margin-top:10px;")),
                    div(class="instruction-card",
                        strong("Step 2 of 7.")," STL decomposition separates long-term trend, ",
                        "seasonal cycles, and irregular noise. Minimum 2 years of data required.")),
                box(width=9,
                    div(class="content-card card-accent-primary",
                        div(class="content-card-header",icon("chart-line"),"Time Series Decomposition"),
                        div(class="content-card-body",
                            fluidRow(
                              column(6, withSpinner(plotlyOutput("observed_plot", height="300px"), type = 8)),
                              column(6, withSpinner(plotlyOutput("trend_plot",    height="300px"), type = 8))
                            ),
                            fluidRow(
                              column(6, withSpinner(plotlyOutput("seasonal_plot", height="300px"), type = 8)),
                              column(6, withSpinner(plotlyOutput("remainder_plot",height="300px"), type = 8))
                            ))),
                    div(class="next-step-bar",
                        div(tags$strong("Next: Seasonality"),
                            p("Examine monthly patterns and test for seasonal effects.")),
                        actionButton("go_season","Seasonality →",class="nav-next-btn",
                                     onclick="Shiny.setInputValue('tabs','seasonality',{priority:'event'})"))
                )
              )),
      
      # ======================================================
      # SEASONALITY
      # ======================================================
      
      tabItem(tabName="seasonality",
              fluidRow(box(width=12,
                           div(class="page-header-band",icon("calendar",class="fa-4x",style="margin-left:0px;"),
                               div(h2("Step 3 — Seasonal Patterns"))))),
              step_progress(3),
              fluidRow(
                box(width=3,
                    div(class="filter-panel",
                        div(class="filter-panel-title",icon("filter"),"Filters"),
                        conditionalPanel("input.season_tab == 's_plots'",
                                         selectInput("season_region_plots", "Region", 
                                                     choices=c("Upper East","Upper West","Northern","Brong Ahafo",
                                                               "Ashanti","Eastern","Volta","Greater Accra","Central","Western"),
                                                     selected="Upper East"),
                                         dateRangeInput("season_dateRange_plots", "Date Range",
                                                        start = min(data$date), end = max(data$date),
                                                        min = min(data$date), max = max(data$date)),
                                         actionButton("reset_season_plots", "Reset Filters", icon = icon("redo"),
                                                      class="btn btn-warning",style="margin-top:10px;")
                        ),
                        
                        conditionalPanel("input.season_tab == 's_test'",
                                         selectInput("season_region_tests", "Region", 
                                                     choices=c("Upper East","Upper West","Northern","Brong Ahafo",
                                                               "Ashanti","Eastern","Volta","Greater Accra","Central","Western"),
                                                     selected="Upper East"),
                                         dateRangeInput("season_dateRange_tests", "Date Range",
                                                        start = min(data$date), end = max(data$date),
                                                        min = min(data$date), max = max(data$date)),
                                         conditionalPanel("output.short_range_tests == true",
                                                          p(style="font-size:13px;color:#a32d2d;margin-top:4px;",
                                                            em("Note: Range must be at least one full years"))
                                         ),
                                         actionButton("reset_season_tests", "Reset Filters", icon = icon("redo"),
                                                      class="btn btn-warning",style="margin-top:10px;")
                        )),
                    
                    conditionalPanel("input.season_tab == 's_plots'",
                                     div(class="instruction-card",
                                         strong("Step 3 of 7.")," Box plots show the distribution of each variable by month.")),
                    conditionalPanel("input.season_tab == 's_test'",
                                     div(class="instruction-card",
                                         strong("Step 3 of 7.")," Statistical tests confirm whether seasonality is significant."))),
                box(width=9,
                    div(style="margin-top:20px;",
                        div(class="section-tab-bar",
                            tags$a(class="stab active",`data-target`="s_plots",`data-group`="season",
                                   icon("chart-bar"),"Seasonal Box Plots"),
                            tags$a(class="stab",       `data-target`="s_test", `data-group`="season",
                                   icon("flask"),"Seasonality Tests")),
                        div(class="stab-panel active",`data-panel`="s_plots",`data-group`="season",
                            div(class="content-card card-accent-primary",
                                div(class="content-card-header",icon("calendar"),"Seasonal Box Plots"),
                                div(class="content-card-body",
                                    fluidRow(
                                      column(6, withSpinner(plotlyOutput("boxplot1",height="300px"), type = 8)),
                                      column(6, withSpinner(plotlyOutput("boxplot2",height="300px"), type = 8))
                                    ),
                                    fluidRow(
                                      column(6, withSpinner(plotlyOutput("boxplot3",height="300px"), type = 8)),
                                      column(6, withSpinner(plotlyOutput("boxplot4",height="300px"), type = 8))
                                    ),
                                    fluidRow(
                                      column(6, withSpinner(plotlyOutput("boxplot5",height="300px"), type = 8))
                                    )))),
                        div(class="stab-panel",`data-panel`="s_test",`data-group`="season",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("flask"),"Test of Seasonality"),
                                div(class="content-card-body",
                                    withSpinner(DTOutput("results_table"), type = 8)))),
                        div(class="next-step-bar",
                            div(tags$strong("Next: Model Estimates"),
                                p("Review GAM model metrics and smooth term estimates.")),
                            actionButton("go_est","Model Estimates →",class="nav-next-btn",
                                         onclick="Shiny.setInputValue('tabs','estimates',{priority:'event'})"))
                    )
                )
              )),
      
      # ======================================================
      # GAM ESTIMATES
      # ======================================================
      
      tabItem(tabName="estimates",
              fluidRow(box(width=12,
                           div(class="page-header-band",icon("calculator",class="fa-4x",style="margin-left:0px;"),
                               div(h2("Step 4 — GAM Estimates"))))),
              step_progress(4),
              fluidRow(
                # --- Left filter panel ---
                box(
                  width = 3,
                  div(class = "filter-panel",
                      div(class = "filter-panel-title", icon("filter"), "Filters"),
                      
                      # Filters for Model Metrics
                      conditionalPanel("input.est_tab == 'e_metrics'",
                                       selectInput("model_region", "Region (GAM Metrics):",
                                                   choices  = gam_regions,
                                                   selected = "Northern"),
                                       actionButton("reset_est_metrics", "Reset Filters", icon = icon("redo"),
                                                    class = "btn btn-warning", style = "margin-top:10px;"),
                                       tags$br(), tags$br(),
                                       div(class = "instruction-card",
                                           strong("Step 4 of 7."), " Compare 8 candidate negative-binomial GAMs (fit via REML). The best model is ",
                                           "selected by highest adjusted R² and deviance explained.",
                                           " Use this panel to evaluate overall performance metrics.")
                      ),
                      
                      # Filters for Smooth Terms
                      conditionalPanel("input.est_tab == 'e_smooth'",
                                       selectInput("smooth_region", "Region (GAM Smooth Terms):",
                                                   choices  = gam_regions,
                                                   selected = "Northern"),
                                       actionButton("reset_est_smooth", "Reset Filters", icon = icon("redo"),
                                                    class = "btn btn-warning", style = "margin-top:10px;"),
                                       tags$br(), tags$br(),
                                       div(class = "instruction-card",
                                           strong("Step 4 of 7."), " Review parametric and smooth term estimates ",
                                           "for the best GAM model in the selected region.",
                                           " Use this panel to interpret climatic and temporal effects.")
                      ),
                      
                      # Filters for Forecast Plot
                      conditionalPanel("input.est_tab == 'e_forecast_plot'",
                                       selectInput("forecast_region_plot", "Region (GAM Forecast Plot):",
                                                   choices  = gam_regions,
                                                   selected = "Northern"),
                                       actionButton("reset_forecast_plot", "Reset Filters", icon = icon("redo"),
                                                    class = "btn btn-warning", style = "margin-top:10px;"),
                                       tags$br(), tags$br(),
                                       div(class = "instruction-card",
                                           strong("Step 4 of 7."), " Forecast plot shows predicted malaria incidence per 1000 population ",
                                           "with 95% confidence intervals for the selected region.")
                      ),
                      
                      # Filters for Forecast Table
                      conditionalPanel("input.est_tab == 'e_forecast_table'",
                                       selectInput("forecast_region_table", "Region (GAM Forecast Table):",
                                                   choices  = gam_regions,
                                                   selected = "Northern"),
                                       actionButton("reset_forecast_table", "Reset Filters", icon = icon("redo"),
                                                    class = "btn btn-warning", style = "margin-top:10px;"),
                                       tags$br(), tags$br(),
                                       div(class = "instruction-card",
                                           strong("Step 4 of 7."), " Forecast table shows numeric incidence per 1000 population predictions ",
                                           "with 95% confidence intervals for the selected region.")
                      )
                  )
                ),
                
                # --- Right content panels ---
                box(
                  width = 9,
                  div(
                    class = "section-tab-bar",
                    tags$a(class = "stab active", `data-target` = "e_metrics", `data-group` = "est",
                           icon("chart-bar"), "GAM Metrics"),
                    tags$a(class = "stab",        `data-target` = "e_smooth",  `data-group` = "est",
                           icon("wave-square"), "GAM Smooth Terms"),
                    tags$a(class = "stab",        `data-target` = "e_forecast_plot", `data-group` = "est",
                           icon("chart-line"), "GAM Forecast Plot"),
                    tags$a(class = "stab",        `data-target` = "e_forecast_table", `data-group` = "est",
                           icon("table"), "GAM Forecast Table")
                  ),
                  
                  # Model Metrics panel
                  div(class = "stab-panel active", `data-panel` = "e_metrics", `data-group` = "est",
                      div(class = "content-card card-accent-primary",
                          div(class = "content-card-header", icon("calculator"), "GAM Model Performance Metrics"),
                          div(class = "content-card-body",
                              withSpinner(DTOutput("model_metrics"), type = 8)))),
                  
                  # Smooth Terms panel
                  div(class = "stab-panel", `data-panel` = "e_smooth", `data-group` = "est",
                      div(class = "content-card card-accent-info",
                          div(class = "content-card-header", icon("wave-square"), "GAM Parametric & Smooth Term Estimates"),
                          div(class = "content-card-body",
                              withSpinner(DTOutput("best_model_smooth_terms"), type = 8),
                              tags$hr(),
                              tags$p("Footnote: edf (Effective Degrees of Freedom) measures the 'wiggliness' of a smooth function; Ref.df is used for hypothesis testing.",
                                     style = "color:black; font-weight:bold; font-style:italic; font-size:16px; text-align:justify; padding:5px 10px;")))),
                  
                  # Forecast Plot panel
                  div(class = "stab-panel", `data-panel` = "e_forecast_plot", `data-group` = "est",
                      div(class = "content-card card-accent-success",
                          div(class = "content-card-header", icon("chart-line"), "GAM Forecast Plot"),
                          div(class = "content-card-body",
                              withSpinner(plotlyOutput("forecast_plot", height = "400px"), type = 8)))),
                  
                  # Forecast Table panel
                  div(class = "stab-panel", `data-panel` = "e_forecast_table", `data-group` = "est",
                      div(class = "content-card card-accent-warning",
                          div(class = "content-card-header", icon("table"), "GAM Forecast Table"),
                          div(class = "content-card-body",
                              withSpinner(DTOutput("forecast_table"), type = 8)))),
                              
                  # Next step navigation
                  div(class = "next-step-bar",
                      div(tags$strong("Next: Model Diagnostics"),
                          p("Assess model fit with observed vs fitted plots and residual checks.")),
                      actionButton("go_diag", "Diagnostics →", class = "nav-next-btn",
                                   onclick = "Shiny.setInputValue('tabs', 'diagnostics', {priority: 'event'})"))
                )
              )
      ),
      
      # ======================================================
      # GAM DIAGNOSTICS
      # ======================================================
      
      tabItem(tabName="diagnostics",
              fluidRow(box(width=12,
                           div(class="page-header-band",icon("stethoscope",class="fa-4x",style="margin-left:0px;"),
                               div(h2("Step 5 — GAM Diagnostics"))))),
              step_progress(5),
              fluidRow(
                box(width=3,
                    div(class="filter-panel",
                        div(class="filter-panel-title",icon("filter"),"Filters"),
                        conditionalPanel("input.diag_tab == 'diag1'",
                                         selectInput("diag_region1","Region",
                                                     choices  = gam_regions,
                                                     selected = "Northern"),
                                         actionButton("reset_diag1","Reset Filters",icon=icon("redo"),
                                                      class="btn btn-warning",style="margin-top:10px;")),
                        conditionalPanel("input.diag_tab == 'diag2'",
                                         selectInput("diag_region2","Region",
                                                     choices  = gam_regions,
                                                     selected = "Northern"),
                                         actionButton("reset_diag2","Reset Filters",icon=icon("redo"),
                                                      class="btn btn-warning",style="margin-top:10px;")),
                        conditionalPanel("input.diag_tab == 'diag3'",
                                         selectInput("diag_region3","Region",
                                                     choices  = gam_regions,
                                                     selected = "Northern"),
                                         actionButton("reset_diag3","Reset Filters",icon=icon("redo"),
                                                      class="btn btn-warning",style="margin-top:10px;")),
                        conditionalPanel("input.diag_tab == 'diag4'",
                                         selectInput("diag_region4","Region",
                                                     choices  = gam_regions,
                                                     selected = "Northern"),
                                         sliderInput("max_lag","Select lag range:", min=1, max=60, value=12, width="95%"),
                                         actionButton("reset_diag4","Reset Filters",icon=icon("redo"),
                                                      class="btn btn-warning",style="margin-top:10px;")),
                        tags$br(),
                        conditionalPanel("input.diag_tab == 'diag1'",
                                         div(class="instruction-card",strong("Step 5 of 7.")," Observed vs fitted — close tracking indicates good model fit.")),
                        conditionalPanel("input.diag_tab == 'diag2'",
                                         div(class="instruction-card",strong("Step 5 of 7.")," Q-Q plot — points near the line confirm the deviance residuals
                                         follow the fitted negative-binomial reference distribution (obtained by simulation).")),
                        conditionalPanel("input.diag_tab == 'diag3'",
                                         div(class="instruction-card",strong("Step 5 of 7.")," Response vs fitted — systematic scatter indicates misspecification.")),
                        conditionalPanel("input.diag_tab == 'diag4'",
                                         div(class="instruction-card",strong("Step 5 of 7.")," ACF/PACF of deviance residuals and the Ljung-Box test show whether ",
                                             "meaningful residual autocorrelation remains in this negative-binomial GAM (unlike the GAMM, ",
                                             "there is no explicit corARMA correction here).")))),
                box(width=9,
                    div(style="margin-top:20px;",
                        div(class="section-tab-bar",
                            tags$a(class="stab active",`data-target`="diag1",`data-group`="diag",
                                   icon("chart-line"), "Observed vs Fitted"),
                            tags$a(class="stab",       `data-target`="diag2",`data-group`="diag",
                                   icon("chart-area"), "Q-Q Plot"),
                            tags$a(class="stab",       `data-target`="diag3",`data-group`="diag",
                                   icon("circle-dot"), "Response vs Fitted"),
                            tags$a(class="stab",       `data-target`="diag4",`data-group`="diag",
                                   icon("wave-square"), "Residual Autocorrelation")),
                        div(class="stab-panel active",`data-panel`="diag1",`data-group`="diag",
                            div(class="content-card card-accent-primary",
                                div(class="content-card-header",icon("chart-line"),"Observed vs Fitted"),
                                div(class="content-card-body",
                                    withSpinner(plotlyOutput("observed_fitted",height="400px"), type = 8)))),
                        div(class="stab-panel",`data-panel`="diag2",`data-group`="diag",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("chart-area"),"Q-Q Plot of Deviance Residuals"),
                                div(class="content-card-body",
                                    withSpinner(plotlyOutput("qq_plot",height="400px"), type = 8)))),
                        div(class="stab-panel",`data-panel`="diag3",`data-group`="diag",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header",icon("circle-dot"),"Response vs Fitted Values"),
                                div(class="content-card-body",
                                    withSpinner(plotlyOutput("response_fitted_plot",height="400px"), type = 8)))),
                        div(class="stab-panel", `data-panel`="diag4", `data-group`="diag",
                            div(class="content-card card-accent-info",
                                div(class="content-card-header", icon("wave-square"), "Residual ACF / PACF & Ljung-Box"),
                                div(class="content-card-body",
                                    withSpinner(uiOutput("gam_autocorr_banner"), type = 8),
                                    fluidRow(
                                      column(6, withSpinner(plotlyOutput("gam_acf_plot",  height="320px"), type = 8)),
                                      column(6, withSpinner(plotlyOutput("gam_pacf_plot", height="320px"), type = 8))
                                    ),
                                    tags$hr(),
                                    withSpinner(DTOutput("gam_ljung_box_table"), type = 8)))),
                        div(class="next-step-bar",
                            div(tags$strong("Next: Model Plots"),
                                p("Visualise smooth term partial effects for each predictor.")),
                            actionButton("go_plots","Model Plots →",class="nav-next-btn",
                                         onclick="Shiny.setInputValue('tabs','plots',{priority:'event'})"))
                    )
                )
              )),
      
      
      # ======================================================
      # GAM PLOTS
      # ======================================================
      
      tabItem(tabName="plots",
              fluidRow(box(width=12,
                           div(class="page-header-band",icon("chart-area",class="fa-4x",style="margin-left:0px;"),
                               div(h2("Step 6 — GAM Smooth Term Plots"))))),
              step_progress(6),
              fluidRow(
                box(width=3,
                    div(class="filter-panel",
                        div(class="filter-panel-title",icon("filter"),"Filters"),
                        selectInput("plot_region","Region",
                                    choices  = gam_regions,
                                    selected = "Northern"),
                        actionButton("reset_plots","Reset Filters",icon=icon("redo"),
                                     class="btn btn-warning",style="margin-top:10px;")),
                    div(class="instruction-card",
                        strong("Step 6 of 7.")," Partial effects show each predictor's isolated contribution ",
                        "holding all others at their mean. Shaded area = 95% CI.")),
                box(width=9,
                    div(class="content-card card-accent-primary",
                        div(class="content-card-header",icon("chart-area"),"Smooth Term Partial Effect Plots"),
                        div(class="content-card-body",
                            withSpinner(uiOutput("dynamic_plots"), type = 8),
                            tags$hr(),
                            tags$p("Footnote: Partial effects represent each predictor's isolated contribution. Effects are centred at zero on the linear predictor scale.",
                                   style="font-size:16px;color:black;font-weight:bold;font-style:italic;text-align:justify;padding:5px 10px;"))),
                    div(class="next-step-bar",
                        div(tags$strong("Final Step: Regional Summary"),
                            p("Synthesise all findings into a concise regional profile.")),
                        actionButton("go_summary","Regional Summary →",class="nav-next-btn",
                                     onclick="Shiny.setInputValue('tabs','regional_summary',{priority:'event'})"))
                )
              )),
      
      # ======================================================
      # GAM REGIONAL SUMMARY
      # ======================================================
      
      tabItem(tabName="regional_summary",
              fluidRow(box(width=12,
                           div(class="page-header-band",icon("map-location-dot",class="fa-4x",style="margin-left:0px;"),
                               div(h2("Step 7 — GAM Regional Summary (Incidence-Adjusted)"))))),
              step_progress(7),
              fluidRow(
                box(width = 3,
                    div(class = "filter-panel",
                        div(class = "filter-panel-title", icon("filter"), "Filters"),
                        selectInput("summary_region", "Region",
                                    choices  = gam_regions,
                                    selected = "Northern"),
                        actionButton("reset_summary", "Reset Filter", icon = icon("redo"),
                                     class = "btn btn-warning", style = "margin-top:10px;"),
                        tags$br(), tags$br(),
                        div(class = "instruction-card",
                            strong("Regional Summary:"),
                            " Synthesises findings from Descriptives, Seasonality and GAM models.",
                            " Includes threshold alert, key performance indicator cards,",
                            " trend direction, and narrative.")
                    )
                ),
                box(width=9,
                    tagList(
                      uiOutput("gam_autocorr_banner_summary"),
                      withSpinner(uiOutput("regional_summary_ui"), type = 8)
                    ))
              )
      ),
      
      # ======================================================
      # MODEL VALIDITY  
      # ======================================================
      
      tabItem(tabName = "model_status",
        fluidRow(
          box(width = 12,
            div(class = "page-header-band",
              icon("shield-halved", class = "fa-4x", style = "margin-left:0px;"),
              div(h2("Model Validity — Which Model to Trust, Per Region"))))
          ),

        fluidRow(
          box(width = 12,
            div(class = "section-tab-bar",
              tags$a(class = "stab active", `data-target` = "mv_summary", `data-group`  = "mv",
                icon("clipboard-check"), "Validity Summary"),
              tags$a(class = "stab", `data-target` = "mv_concurvity", `data-group`  = "mv",
                icon("diagram-project"), "Concurvity")),

            # --------------------Validity summary panel ---------------------------
            div(class = "stab-panel active", `data-panel` = "mv_summary", `data-group` = "mv",
              div(class = "content-card card-accent-primary",
                div(class = "content-card-header",icon("clipboard-check"),
                  "Per-Region Model Validity Summary"),
                div(class = "content-card-body",
                  p(style = "font-size:14px;color:#555;text-align:justify;",
                    "For each of the 10 regions, the standalone negative-binomial GAM was evaluated by testing its deviance ",
                    "residuals for serial autocorrelation using the Ljung-Box test at lags of 12, 24, 36, 48, and 60 months. ",
                    "Where significant autocorrelation remained (p-value < 0.05 at any tested lag), a GAMM with a corARMA(p,q) ",
                    "correlation structure was fitted to the same smoother, and the Ljung-Box test was repeated on the normalised ",
                    "residuals. The table below shows, for every region, which model should be used for inference and forecasting, ",
                    "and why."),

                  withSpinner(DTOutput("model_status_overview"), type = 8)))),

            # -----------------Concurvity panel ------------------------
            div(class = "stab-panel", `data-panel` = "mv_concurvity", `data-group` = "mv",
              fluidRow(
                column(3,
                  div(class = "filter-panel",
                    div(class = "filter-panel-title", icon("filter"), "Filters"),
                    selectInput("concurvity_region", "Region",
                                choices = c("Upper East","Upper West","Northern","Brong Ahafo",
                                            "Ashanti","Eastern","Volta","Greater Accra","Central","Western"),
                                selected = "Upper East"),
                    selectInput("concurvity_type", "Pairwise statistic:",
                                choices = c("Worst-case" = "worst",
                                            "Observed"   = "observed",
                                            "Estimate"   = "estimate"),
                                selected = "estimate"),
                    actionButton("reset_diag5", "Reset Filters", icon = icon("redo"),
                                 class = "btn btn-warning"),
                    tags$br(), tags$br(),
                    div(
                      class = "instruction-card", strong("Concurvity"),
                      " Concurvity measures how well one smooth term could be approximated by a combination of the others — ",
                      "the GAM analogue of collinearity. Values near 1 mean a term's effect may be confounded with other terms ",
                      "and its estimate should be interpreted cautiously; values near 0 indicate the term is well-identified. ",
                      "See the Appendix for the seasonal-smooth necessity checks that follow from this diagnostic.")
                  )
                ),

                column(9,
                  div(
                    class = "content-card card-accent-info",
                    div(class = "content-card-header", icon("diagram-project"), "Concurvity Analysis"),
                    div(class = "content-card-body",
                      h5("Overall Concurvity by Term"),
                      withSpinner(DTOutput("concurvity_overall_table"), type = 8),
                      tags$hr(),
                      h5("Pairwise Concurvity Between Terms"),
                      withSpinner(DTOutput("concurvity_pairwise_table"), type = 8)))))
              ),

            div(class = "next-step-bar",
              div(tags$strong("Next: GAMM Estimates"),
                p("For regions where the summary table above recommends a GAMM, proceed to the GAMM analysis branch.")),

              actionButton("go_gamm_est", "GAMM Estimates →", class = "nav-next-btn",
                onclick = "Shiny.setInputValue('tabs','gamm_estimates',{priority:'event'})"))))
        ),
      
      # ======================================================
      # GAMM ESTIMATES
      # ======================================================
      
      tabItem(tabName="gamm_estimates",
              fluidRow(box(width=12,
                           div(class="page-header-band",icon("calculator",class="fa-4x",style="margin-left:0px;"),
                               div(h2("Step 4G — GAMM Estimates"))))),
              fluidRow(
                box(width = 3,
                    div(class = "filter-panel",
                        div(class = "filter-panel-title", icon("filter"), "Filters"),
                        
                        conditionalPanel("input.gest_tab == 'ge_metrics'",
                                         selectInput("gamm_model_region", "Region (GAMM Metrics):",
                                                     choices  = gamm_regions,
                                                     selected = "Upper East"),
                                         actionButton("reset_gest_metrics", "Reset Filters", icon = icon("redo"),
                                                      class = "btn btn-warning", style = "margin-top:10px;"),
                                         tags$br(), tags$br(),
                                         div(class = "instruction-card",
                                             strong("Step 4G."), " Compares corARMA(p,q) structures fitted on top of the ",
                                             "concurvity-resolved negative-binomial smoother. Best model = lowest AIC among ",
                                             "PQL-converged fits.")
                        ),
                        
                        conditionalPanel("input.gest_tab == 'ge_smooth'",
                                         selectInput("gamm_smooth_region", "Region (GAMM Smooth Terms):",
                                                     choices  = gamm_regions,
                                                     selected = "Upper East"),
                                         actionButton("reset_gest_smooth", "Reset Filters", icon = icon("redo"),
                                                      class = "btn btn-warning", style = "margin-top:10px;"),
                                         tags$br(), tags$br(),
                                         div(class = "instruction-card",
                                             strong("Step 4G."), " Parametric/smooth estimates plus the fitted AR/MA ",
                                             "correlation coefficients for the winning corARMA structure.")
                        ),
                        
                        conditionalPanel("input.gest_tab == 'ge_forecast_plot'",
                                         selectInput("gamm_forecast_region_plot", "Region (GAMM Forecast Plot):",
                                                     choices  = gamm_regions,
                                                     selected = "Upper East"),
                                         actionButton("reset_gforecast_plot", "Reset Filters", icon = icon("redo"),
                                                      class = "btn btn-warning", style = "margin-top:10px;"),
                                         tags$br(), tags$br(),
                                         div(class = "instruction-card",
                                             strong("Step 4G."), " Forecast plot shows predicted malaria incidence per 1000 population ",
                                             "with 95% confidence intervals for the selected region.")
                        ),
                        
                        conditionalPanel("input.gest_tab == 'ge_forecast_table'",
                                         selectInput("gamm_forecast_region_table", "Region (GAMM Forecast Table):",
                                                     choices  = gamm_regions,
                                                     selected = "Upper East"),
                                         actionButton("reset_gforecast_table", "Reset Filters", icon = icon("redo"),
                                                      class = "btn btn-warning", style = "margin-top:10px;"),
                                         div(class = "instruction-card",
                                             strong("Step 4G."), "Forecast table shows numeric incidence per 1000 population predictions ",
                                             "with 95% confidence intervals for the selected region.")
                        )
                    )
                ),
                box(width = 9,
                    div(class = "section-tab-bar",
                        tags$a(class = "stab active", `data-target` = "ge_metrics", `data-group` = "gest",
                               icon("chart-bar"), "GAMM Metrics"),
                        tags$a(class = "stab", `data-target` = "ge_smooth", `data-group` = "gest",
                               icon("wave-square"), "GAMM Smooth Terms"),
                        tags$a(class = "stab", `data-target` = "ge_forecast_plot", `data-group` = "gest",
                               icon("chart-line"), "GAMM Forecast Plot"),
                        tags$a(class = "stab", `data-target` = "ge_forecast_table", `data-group` = "gest",
                               icon("table"), "GAMM Forecast Table")
                    ),
                    div(class = "stab-panel active", `data-panel` = "ge_metrics", `data-group` = "gest",
                        div(class = "content-card card-accent-primary",
                            div(class = "content-card-header", icon("calculator"), "GAMM Model Performance Metrics"),
                            div(class = "content-card-body",
                                withSpinner(DTOutput("gamm_model_metrics"), type = 8)))),
                    div(class = "stab-panel", `data-panel` = "ge_smooth", `data-group` = "gest",
                        div(class = "content-card card-accent-info",
                            div(class = "content-card-header", icon("wave-square"), "GAM Parametric, Smooth Term, & Correlation Structure Estimates"),
                            div(class = "content-card-body",
                                withSpinner(DTOutput("gamm_best_model_smooth_terms"), type = 8),
                                uiOutput("gamm_corr_info")))),
                    div(class = "stab-panel", `data-panel` = "ge_forecast_plot", `data-group` = "gest",
                        div(class = "content-card card-accent-success",
                            div(class = "content-card-header", icon("chart-line"), "GAMM Forecast Plot"),
                            div(class = "content-card-body",
                                withSpinner(plotlyOutput("gamm_forecast_plot", height = "400px"), type = 8)))),
                    div(class = "stab-panel", `data-panel` = "ge_forecast_table", `data-group` = "gest",
                        div(class = "content-card card-accent-warning",
                            div(class = "content-card-header", icon("table"), "GAMM Forecast Table"),
                            div(class = "content-card-body",
                                withSpinner(DTOutput("gamm_forecast_table"), type = 8)))),
                    div(class = "next-step-bar",
                        div(tags$strong("Next: GAMM Diagnostics")),
                        actionButton("go_gdiag", "GAMM Diagnostics →", class = "nav-next-btn",
                                     onclick = "Shiny.setInputValue('tabs', 'gamm_diagnostics', {priority: 'event'})"))
                )
              )
      ),
      
      # ======================================================
      # GAMM DIAGNOSTICS
      # ======================================================
      
      tabItem(tabName = "gamm_diagnostics",
              fluidRow(
                box(width = 12,
                    div(class = "page-header-band",
                        icon("stethoscope", class = "fa-4x", style = "margin-left:0px;"),
                        div(h2("Step 5G — GAMM Diagnostics"))))
              ),
              fluidRow(
                box(width = 3,
                    div(class = "filter-panel",
                        div(class = "filter-panel-title", icon("filter"), "Filters"),
                        
                        conditionalPanel("input.gdiag_tab == 'gdiag1'",
                                         selectInput("gamm_diag_region1", "Region",
                                                     choices  = gamm_regions,
                                                     selected = "Upper East"),
                                         actionButton("reset_gdiag1", "Reset Filters", icon = icon("redo"),
                                                      class = "btn btn-warning", style = "margin-top:10px;"),
                                         tags$br(), tags$br(),
                                         div(class = "instruction-card",
                                             strong("Step 5G."), " Observed vs fitted for the winning GAMM.")
                        ),
                        
                        conditionalPanel("input.gdiag_tab == 'gdiag2'",
                                         selectInput("gamm_diag_region2", "Region",
                                                     choices  = gamm_regions,
                                                     selected = "Upper East"),
                                         actionButton("reset_gdiag2", "Reset Filters", icon = icon("redo"),
                                                      class = "btn btn-warning", style = "margin-top:10px;"),
                                         tags$br(), tags$br(),
                                         div(class = "instruction-card",
                                             strong("Step 5G."), " Q-Q plot of normalised residuals — checks distributional assumptions.")
                        ),
                        
                        conditionalPanel("input.gdiag_tab == 'gdiag3'",
                                         selectInput("gamm_diag_region3", "Region",
                                                     choices  = gamm_regions,
                                                     selected = "Upper East"),
                                         actionButton("reset_gdiag3", "Reset Filters", icon = icon("redo"),
                                                      class = "btn btn-warning", style = "margin-top:10px;"),
                                         tags$br(), tags$br(),
                                         div(class = "instruction-card",
                                             strong("Step 5G."), " Response vs fitted values — checks model fit and systematic bias.")
                        ),
                        
                        conditionalPanel("input.gdiag_tab == 'gdiag4'",
                                         selectInput("gamm_diag_region4", "Region",
                                                     choices  = gamm_regions,
                                                     selected = "Upper East"),
                                         sliderInput("gamm_max_lag", "Select lag range:", min = 1, max = 60, value = 12, width = "95%"),
                                         actionButton("reset_gdiag4", "Reset Filters", icon = icon("redo"),
                                                      class = "btn btn-warning", style = "margin-top:10px;"),
                                         tags$br(), tags$br(),
                                         div(class = "instruction-card",
                                             strong("Step 5G."), " ACF/PACF/Ljung-Box on normalised residuals — these already include corARMA correction.")
                        ),
                    )
                ),
                
                box(width = 9,
                    div(class = "section-tab-bar",
                        tags$a(class = "stab active", `data-target` = "gdiag1", `data-group` = "gdiag",
                               icon("chart-line"), "Observed vs Fitted"),
                        tags$a(class = "stab", `data-target` = "gdiag2", `data-group` = "gdiag",
                               icon("chart-area"), "Q-Q Plot"),
                        tags$a(class = "stab", `data-target` = "gdiag3", `data-group` = "gdiag",
                               icon("circle-dot"), "Response vs Fitted"),
                        tags$a(class = "stab", `data-target` = "gdiag4", `data-group` = "gdiag",
                               icon("wave-square"), "Residual Autocorrelation")
                    ),
                    
                    div(class = "stab-panel active", `data-panel` = "gdiag1", `data-group` = "gdiag",
                        div(class = "content-card card-accent-primary",
                            div(class = "content-card-header", icon("chart-line"), "GAMM Observed vs Fitted"),
                            div(class = "content-card-body",
                                withSpinner(plotlyOutput("gamm_observed_fitted", height = "400px"), type = 8)))),
                    
                    div(class = "stab-panel", `data-panel` = "gdiag2", `data-group` = "gdiag",
                        div(class = "content-card card-accent-info",
                            div(class = "content-card-header", icon("chart-area"), "GAMM Q-Q Plot"),
                            div(class = "content-card-body",
                                withSpinner(plotlyOutput("gamm_qq_plot", height = "400px"), type = 8)))),
                    
                    div(class = "stab-panel", `data-panel` = "gdiag3", `data-group` = "gdiag",
                        div(class = "content-card card-accent-info",
                            div(class = "content-card-header", icon("circle-dot"), "GAMM Response vs Fitted"),
                            div(class = "content-card-body",
                                withSpinner(plotlyOutput("gamm_response_fitted_plot", height = "400px"), type = 8)))),
                    
                    div(class = "stab-panel", `data-panel` = "gdiag4", `data-group` = "gdiag",
                        div(class = "content-card card-accent-info",
                            div(class = "content-card-header", icon("wave-square"), "GAMM Residual ACF / PACF & Ljung-Box"),
                            div(class = "content-card-body",
                                withSpinner(uiOutput("gamm_autocorr_banner"), type = 8),
                                fluidRow(
                                  column(6, withSpinner(plotlyOutput("gamm_diag_acf_plot", height = "320px"), type = 8)),
                                  column(6, withSpinner(plotlyOutput("gamm_diag_pacf_plot", height = "320px"), type = 8))
                                ),
                                tags$hr(),
                                withSpinner(DTOutput("gamm_diag_ljung_box_table"), type = 8)))),
                    
                    div(class = "next-step-bar",
                        div(tags$strong("Next: GAMM Plots")),
                        actionButton("go_gplots", "GAMM Plots →", class = "nav-next-btn",
                                     onclick = "Shiny.setInputValue('tabs','gamm_plots',{priority:'event'})"))
                )
              )
      ),
      
      # ======================================================
      # GAMM PLOTS
      # ======================================================
      
      tabItem(tabName="gamm_plots",
              fluidRow(box(width=12,
                           div(class="page-header-band",icon("chart-area",class="fa-4x",style="margin-left:0px;"),
                               div(h2("Step 6G — GAMM Smooth Term Plots"))))),
              fluidRow(
                box(width=3,
                    div(class="filter-panel",
                        div(class="filter-panel-title",icon("filter"),"Filters"),
                        selectInput("gamm_plot_region","Region",
                                    choices  = gamm_regions,
                                    selected = "Upper East"),
                        actionButton("reset_gplots","Reset Filters",icon=icon("redo"),
                                     class="btn btn-warning",style="margin-top:10px;")),
                    div(class="instruction-card",
                        strong("Step 6G."), " Partial effects from the GAMM's underlying gam component (",
                        tags$code("bg$gam"), "), fitted with corARMA-corrected PQL.")),
                box(width=9,
                    div(class="content-card card-accent-primary",
                        div(class="content-card-header",icon("chart-area"),"GAMM Smooth Term Partial Effect Plots"),
                        div(class="content-card-body",
                            withSpinner(uiOutput("gamm_dynamic_plots"), type = 8))))
              )
      ),
      
      # ======================================================
      # GAMM REGIONAL SUMMARY
      # ======================================================
      
      tabItem(tabName="gamm_regional_summary",
              fluidRow(
                box(width=12,
                    div(class="page-header-band",
                        icon("map-location-dot", class="fa-4x", style="margin-left:0px;"),
                        div(h2("Step 7G — GAMM Regional Summary (Incidence-Adjusted)"))))
              ),
              fluidRow(
                # --- Left filter panel (Summary only) ---
                box(width=3,
                    div(class="filter-panel",
                        div(class="filter-panel-title", icon("filter"), "Filters"),
                        
                        conditionalPanel(
                          "input.gsub_tab == 'grs_summary'",
                          selectInput("gamm_summary_region", "Region",
                                      choices  = gamm_regions,
                                      selected = "Upper East"),
                          actionButton("reset_gamm_summary", "Reset", icon = icon("redo"),
                                       class = "btn btn-warning", style = "margin-top:10px;"),
                          tags$br(), tags$br(),
                          div(class="instruction-card",
                              strong("GAMM Regional Summary."),
                              " Because the GAMM includes ", tags$code("offset(log_pop_offset)"),
                              ", it models ", tags$strong("malaria incidence"), " (cases per capita), not raw burden.",
                              " This panel therefore reports incidence-based trend and seasonality, which can",
                              " legitimately diverge from the raw-count GAM Regional Summary — e.g. rising case",
                              " counts with flat incidence usually just means the population grew.")
                        )
                    )
                ),
                
                # --- Right panel (Summary only) ---
                box(width=9,
                    div(style="margin-top:20px;",
                        div(class="section-tab-bar",
                            tags$a(class="stab active", `data-target`="grs_summary", `data-group`="gsub",
                                   icon("map-location-dot"), "GAMM Summary Report")),
                        
                        div(class="stab-panel active", `data-panel`="grs_summary", `data-group`="gsub",
                            tagList(
                              uiOutput("gamm_autocorr_banner_summary"),
                              withSpinner(uiOutput("gamm_regional_summary_ui"), type = 8)
                            ))
                    )
                )
              )
      ),
      
      # ======================================================
      # APPENDIX
      # ======================================================
      tabItem(
        tabName = "appendix",
        fluidRow(
          box(width = 12,
              div(class = "page-header-band",
                  icon("book", class = "fa-4x", style = "margin-left:0px;"),
                  div(h2("Appendix & Additional Resources"))))
        ),
        fluidRow(
          box(width = 12,
              div(class = "content-card card-accent-warning",
                  div(class = "content-card-header", icon("book"), "Additional Content"),
                  div(class = "content-card-body",
                      tabBox(
                        width = 12, id = "appendix_tabs",
                        
                        # ---------------- Line Plots ----------------
                        tabPanel("Line Plots",
                                 fluidRow(
                                   column(3,
                                          div(class = "filter-panel",
                                              div(class = "filter-panel-title", icon("filter"), "Filters"),
                                              dateRangeInput("combined_dateRange", "Date Range",
                                                             start = min(data$date, na.rm = TRUE), end = max(data$date, na.rm = TRUE),
                                                             min   = min(data$date, na.rm = TRUE), max = max(data$date, na.rm = TRUE)),
                                              tags$hr(),
                                              downloadButton("download_combined_plot", "Download", class = "btn btn-warning")),
                                          div(class = "instruction-card", strong("Line Plots."),
                                              " Combined malaria and climate series for all regions.")
                                   ),
                                   column(9, withSpinner(plotlyOutput("combined_plot", height = "800px"), type = 8))
                                 )
                        ),
                        
                        # ---------------- Seasonal Pattern ----------------
                        tabPanel("Seasonal Pattern",
                                 fluidRow(
                                   column(3,
                                          div(class = "filter-panel",
                                              div(class = "filter-panel-title", icon("filter"), "Filters"),
                                              selectInput("region", "Region",
                                                          choices = c("Upper East","Upper West","Northern","Brong Ahafo",
                                                                      "Ashanti","Eastern","Volta","Greater Accra","Central","Western"),
                                                          selected = "Upper East"),
                                              dateRangeInput("seasonal_dateRange", "Date Range (full years only)",
                                                             start = min(data$date, na.rm = TRUE), end = max(data$date, na.rm = TRUE),
                                                             min   = min(data$date, na.rm = TRUE), max = max(data$date, na.rm = TRUE),
                                                             format = "yyyy"),
                                              tags$hr(),
                                              downloadButton("download_plot", "Download", class = "btn btn-warning")),
                                          div(class = "instruction-card", strong("Seasonal Pattern."),
                                              " Aggregated monthly histogram with climate overlays.")
                                   ),
                                   column(9, withSpinner(plotOutput("malaria_plot", height = "600px"), type = 8))
                                 )
                        ),
                        
                        # ---------------- Heatmaps ----------------
                        tabPanel("Heatmaps",
                                 fluidRow(
                                   column(3,
                                          div(class = "filter-panel",
                                              div(class = "filter-panel-title", icon("filter"), "Filters"),
                                              dateRangeInput("heatmap_dateRange", "Date Range",
                                                             start = min(data$date, na.rm = TRUE), end = max(data$date, na.rm = TRUE),
                                                             min   = min(data$date, na.rm = TRUE), max = max(data$date, na.rm = TRUE)),
                                              tags$hr(),
                                              downloadButton("download_heatmap", "Download", class = "btn btn-warning")),
                                          div(class = "instruction-card", strong("Heatmaps."),
                                              " Monthly averages of malaria, temperature, and rainfall.")
                                   ),
                                   column(9, withSpinner(plotOutput("combined_heatmap", height = "800px"), type = 8))
                                 )
                        ),
                        
                        # ---------------- GAM Seasonal Smooth Effect ----------------
                        tabPanel("GAM Seasonal Smooth Effect",
                                 fluidRow(
                                   column(
                                     3,
                                     div(class = "filter-panel",
                                         div(class = "filter-panel-title",
                                             icon("filter"),
                                             "Filters"),
                                         
                                         selectInput("gam_smooth_effect_region", "Region",
                                                     choices = c("All Regions" = "all", gam_regions),
                                                     selected = "all"),
                                         actionButton("reset_gam_smooth_effect_region", "Reset Filters", icon = icon("redo"),
                                                      class = "btn btn-warning"),
                                         tags$br(),
                                         tags$br(),
                                         div(class = "instruction-card", strong("Region Filter."),
                                             " Select a single region to focus the seasonal-term table and ",
                                             "narrative on that region only, or leave on \u201cAll Regions\u201d to see every ",
                                             "region compared side by side. This evaluation follows directly from the ",
                                             "concurvity diagnostics on the Model Validity page.")
                                     )
                                   ),
                                   
                                   column(
                                     9,
                                     div(class = "content-card card-accent-info",
                                         div(class = "content-card-header", icon("clipboard-check"),
                                             "GAM Seasonal Smooth Effect"),
                                         div(class = "content-card-body",
                                             p(style = "font-size:14px;color:#555;text-align:justify;",
                                               strong("Context: "),
                                               "Temporal smooths (s(time), s(months), and ti(time, months)) together with the ",
                                               "climate smooths exhibited substantial concurvity because rainfall and temperature ",
                                               "follow strong seasonal patterns. Since concurvity does not necessarily imply that ",
                                               "a smooth term is redundant, the contribution of the seasonal smooth was evaluated ",
                                               "directly."),
                                             p(style = "font-size:14px;color:#555;text-align:justify;",
                                               strong("Evaluation method: "),
                                               "For each region whose final model remained a GAM, a reduced model excluding ",
                                               "s(months) was refitted while keeping all remaining model terms unchanged. The ",
                                               "reduced model was compared with the full model using AIC, deviance explained, ",
                                               "and residual autocorrelation (Ljung-Box test on deviance residuals). If removing ",
                                               "s(months) increased AIC, reduced deviance explained or resulted in significant ",
                                               "residual autocorrelation that was absent in the full model, s(months) was retained. ",
                                               "This indicates that the seasonal smooth explains variation not fully captured by ",
                                               "the climate smooths despite the observed concurvity, consistent with Wood (2008): ",
                                               "concurvity does not imply redundancy."),
                                             withSpinner(uiOutput("seasonal_smooth_banner"), type = 8),
                                             withSpinner(DTOutput("concurvity_smooth_table"), type = 8),
                                             tags$hr(),
                                             uiOutput("concurvity_smooth_effect_writeup")))
                                   )
                                 )
                        ),
                        
                        # ---------------- GAMM Seasonal Smooth Effect ----------------
                        tabPanel("GAMM Seasonal Smooth Effect",
                                 fluidRow(
                                   column(
                                     3,
                                     div(class = "filter-panel",
                                         div(class = "filter-panel-title",
                                             icon("filter"),
                                             "Filters"),
                                         
                                         selectInput("smooth_effect_region_gamm", "Region",
                                                     choices = c("All Regions" = "all", gamm_regions),
                                                     selected = "all"),
                                         actionButton("reset_smooth_effect_region_gamm", "Reset Filters", icon = icon("redo"),
                                                      class = "btn btn-warning"),
                                         tags$br(),
                                         tags$br(),
                                         div(class = "instruction-card", strong("Region Filter."),
                                             " Select a single region to focus the seasonal-term table and ",
                                             "narrative on that region only, or leave on \u201cAll Regions\u201d to see every ",
                                             "GAMM-fitted region compared side by side.")
                                     )
                                   ),
                                   
                                   column(
                                     9,
                                     div(class = "content-card card-accent-warning",
                                         div(class = "content-card-header", icon("clipboard-check"),
                                             "GAMM Seasonal Smooth Effect"),
                                         div(class = "content-card-body",
                                             p(style = "font-size:14px;color:#555;text-align:justify;",
                                               strong("Context: "),
                                               "For regions exhibiting residual serial correlation, the selected GAM was extended ",
                                               "to a GAMM by incorporating an appropriate corARMA(p,q) structure. Although ",
                                               "substantial concurvity among the temporal and climate smooths remained, the ",
                                               "necessity of the seasonal smooth was evaluated directly rather than inferred ",
                                               "from concurvity alone."),
                                             p(style = "font-size:14px;color:#555;text-align:justify;",
                                               strong("Evaluation method: "),
                                               "For each region's final GAMM, a reduced model excluding s(months) was refitted ",
                                               "while retaining the selected corARMA(p,q) structure and the negative binomial ",
                                               "dispersion parameter (θ). Because gamm() estimates the GAM component using ",
                                               "penalized quasi-likelihood (PQL), deviance explained is not directly comparable ",
                                               "between competing GAMMs. Therefore, model comparison was based on the AIC and ",
                                               "BIC of the lme component together with residual autocorrelation diagnostics ",
                                               "(Ljung-Box test on normalised residuals). If removing s(months) increased AIC ",
                                               "or BIC or resulted in significant residual autocorrelation that was absent in ",
                                               "the full model, s(months) was retained. This demonstrates that the seasonal ",
                                               "smooth contributes information beyond that explained by the climate smooths ",
                                               "and the residual correlation structure, consistent with Wood (2008): concurvity ",
                                               "does not imply redundancy."),
                                             
                                             withSpinner(uiOutput("seasonal_smooth_banner_gamm"), type = 8),
                                             withSpinner(DTOutput("concurvity_smooth_table_gamm"), type = 8),
                                             tags$hr(),
                                             uiOutput("concurvity_smooth_effect_writeup_gamm")))
                                   )
                                 )
                        ),
                        
                        # ---------------- GAM & GAMM Framework ----------------
                        tabPanel("GAM & GAMM Framework",
                                 div(class = "instruction-card", strong("GAM & GAMM Framework."),
                                     " Methodology and model-building algorithm."),
                                 div(style = "text-align:center;font-size:14px;",
                                     tags$img(src = "./images/gamm_algorithm.png",
                                               width = "100%", alt = "GAM framework diagram"))
                        )
                      )
                  )))
        )
      ),
      
      # ======================================================
      # SOURCE CODE
      # ======================================================
      tabItem(tabName="code_tab",
              fluidRow(box(width=12,
                           div(id="code-panel",
                               div(class="output-card",
                                   style="background:linear-gradient(135deg,#24292e 0%,#000000 100%);color:white;
                         padding:50px;margin-top:5px;margin-bottom:5px;text-align:center;",
                                   h2(style="margin:0 0 20px 0;", icon("github",class="fa-3x")," View Source Code"),
                                   p(style="font-size:1.50rem;opacity:0.9;","Access the complete R code repository on GitHub"),
                                   tags$a(href="https://github.com/EdwardAkurugu/GAMM-MAP", target="_blank",
                                          class="btn btn-primary",
                                          style="font-size:2.0rem;padding:15px 40px;margin-top:20px;",
                                          icon("github",class="fa-lg")," Visit GitHub Repository"),
                                   tags$br(), tags$br(),
                                   div(style="display:inline-flex;gap:20px;flex-wrap:wrap;justify-content:center;margin-top:10px;",
                                       div(style="background:linear-gradient(135deg,var(--primary) 0%,var(--secondary) 100%)!important;border-radius:8px;padding:16px 24px;",
                                           icon("map","fa-2x"), tags$br(), p(style="margin:8px 0 0;font-size:1.2rem;opacity:0.85;","10 Ghana Regions")),
                                       div(style="background:linear-gradient(135deg,var(--primary) 0%,var(--secondary) 100%)!important;border-radius:8px;padding:16px 24px;",
                                           icon("database","fa-2x"), tags$br(), p(style="margin:8px 0 0;font-size:1.2rem;opacity:0.85;","DHIMS2 & GMet Data")),
                                       div(style="background:linear-gradient(135deg,var(--primary) 0%,var(--secondary) 100%)!important;border-radius:8px;padding:16px 24px;",
                                           icon("chart-line","fa-2x"), tags$br(), p(style="margin:8px 0 0;font-size:1.2rem;opacity:0.85;","mgcv GAMs & GAMMs")),
                                       div(style="background:linear-gradient(135deg,var(--primary) 0%,var(--secondary) 100%)!important;border-radius:8px;padding:16px 24px;",
                                           icon("r-project","fa-2x"), tags$br(), p(style="margin:8px 0 0;font-size:1.2rem;opacity:0.85;","Built with R & Shiny")))
                               )))))
    )
  )
)

