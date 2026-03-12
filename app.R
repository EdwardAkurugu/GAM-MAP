
#Source packages
source("packages.R")

# Source the data file
source("data.R")

#Source static plot function
source("static plot function.R")

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


