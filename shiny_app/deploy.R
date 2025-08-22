library(rsconnect)

deployApp(
  appDir = "./shiny_app",                  # The folder containing the app
  appPrimaryDoc = "app_optimized_v4.R",  # The main app file inside that folder
  appName = "G-TROPIC_2024",
  appTitle = "Global Tropical Cyclone Events of 2024",
  forceUpdate = TRUE
)
