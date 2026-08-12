

#==============================================================
# DASHBOARD LAUNCHER
#==============================================================
source("packages.R")
source("data.R")
source("static_plot_function.R")
source("plot_themes.R")
source("app/server.R")
source("app/dashboard_styles.R")
source("app/ui.R")
# source("app/ui_old.R")
shinyApp(ui = ui, server = server)

#===============================================================



# #Notes on Deploying the app in R Studio interface
# install.packages("rsconnect")
# #Get the token with the secret
# rsconnect::setAccountInfo(name='edwardakurugu',
#                           token='7C491C5ADB6743430FCAFAC515EFEB63',
#                           secret='z2gS65lNWxf60+h1WFfs0MfBnUM7frRoLIZ18JHs')
# 
# library(rsconnect)
# rsconnect::deployApp(
#   appDir = "C:/Users/AKREDW001/Desktop/GAM-MAP/app",
#   appName = "gam-map-dashboard",
#   account = "edwardakurugu"
# )
# 
# deployApp()
