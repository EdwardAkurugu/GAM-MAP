
source("packages.R")

# Source the data file
source("data.R")

#Source ploting themes
source("plot_themes.R")

# Source the server file
source("app/server.R")  

# Source Custom CSS 
source("dashboard_styles.R")

# Source the UI file
source("app/ui.R")

# Run the Shiny application
shinyApp(ui = ui, server = server)

# Source the Pattern Plots
#source("Patterns of Aggregated Cases.R")




# #Notes on Deploying the app in R Studio interface 
# install.packages("rsconnect")
# #Get the token with the secret
# rsconnect::setAccountInfo(name='edwardakurugu',
#                           token='BEFB347220E858B713F11800B53C9B3B',
#                           secret='KfRn8EAR0qmFmH7csTlfzi/PJyIFb07ll7xYbKIb')
# library(rsconnect)
# rsconnect:deployApp("C:/Users/AKREDW001/Desktop/Final GAM/my_app")
# deployApp()