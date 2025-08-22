library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(viridis)
library(DT)

# ========================================
# LOAD PRE-PROCESSED DATA (ONCE AT STARTUP)
# ========================================

# Check if processed data exists
if(!dir.exists("processed_data_v4")) {
  stop("Processed data not found! Please run 'preprocess_data.R' first.")
}

# Load all pre-processed data at startup
cat("Loading pre-processed data...\n")
adm0_boundaries_wgs84 <- readRDS("processed_data_v4/adm0_boundaries_wgs84.rds")
summary_stats <- readRDS("processed_data_v4/summary_stats.rds")
cat("Data loaded successfully!\n")

# ========================================
# PRE-CALCULATE PALETTES
# ========================================


# Month palette using a robust function
month_pal <- colorFactor(
  palette = viridis::mako(12),
  domain = 1:12,
  na.color = "grey" # Safety for NA values
)

# ========================================
# SHINY UI
# ========================================

ui <- dashboardPage(
  dashboardHeader(title = "Global Hurricane/Tropical Cyclone Exposure 2024"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Global Map", tabName = "map", icon = icon("globe")),
      menuItem("Storm Tracks", tabName = "tracks", icon = icon("route"))
    ),
    hr(),
    h4("Filters"),
    selectInput("exposure_type",
                "Exposure Type:",
                choices = c("Tropical Cyclone" = "tc",
                            "Hurricane/Typhoon" = "hurr"),
                selected = "tc"),
    
    sliderInput("month_filter",
                "Month Filter:",
                min = 1,
                max = 12,
                value = c(1, 12),
                step = 1),
    
    checkboxInput("show_boundaries",
                  "Show Country Boundaries",
                  value = TRUE),
    hr(),
    tags$div(
      style = "padding: 10px; font-size: 11px; color: #666;",
      tags$p(paste("Data processed:", summary_stats$data_processed_date)),
      tags$p(paste("TC storms:", summary_stats$tc_storms)),
      tags$p(paste("Hurricane storms:", summary_stats$hurr_storms))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "map",
        fluidRow(
          column(12,
                 box(
                   title = "Global Exposure Map with Storm Tracks",
                   width = 12, status = "primary", solidHeader = TRUE,
                   leafletOutput("exposure_map", height = "600px")
                 )
          )
        ),
        fluidRow(
          column(6,
                 box(
                   title = "Top 10 Most Affected Regions",
                   width = 12, status = "info", solidHeader = TRUE,
                   DT::dataTableOutput("top_regions")
                 )
          ),
          column(6,
                 box(
                   title = "Exposure Summary",
                   width = 12, status = "success", solidHeader = TRUE,
                   valueBoxOutput("total_exposure"),
                   valueBoxOutput("affected_regions"),
                   valueBoxOutput("affected_population")
                 )
          )
        )
      ),
      tabItem(
        tabName = "tracks",
        fluidRow(
          column(12,
                 box(
                   title = "Storm Tracks Visualization",
                   width = 12, status = "primary", solidHeader = TRUE,
                   leafletOutput("tracks_map", height = "600px")
                 )
          )
        ),
        fluidRow(
          column(12,
                 box(
                   title = "Storm Information",
                   width = 12, status = "info", solidHeader = TRUE,
                   DT::dataTableOutput("storm_table")
                 )
          )
        )
      )
    )
  )
)

# ========================================
# SHINY SERVER
# ========================================

server <- function(input, output, session) {
  
  # Reactive exposure data
  exposure_data <- reactive({
    if (input$exposure_type == "tc") {
      readRDS("processed_data_v4/adm2_tc_exp_wgs84.rds") # <-- LOAD HERE
    } else {
      readRDS("processed_data_v4/adm2_hurr_exp_wgs84.rds") # <-- OR HERE
    }
  })
  
  tracks_data <- reactive({
    tracks <- if (input$exposure_type == "tc") {
      readRDS("processed_data_v4/tc_tracks_2024.rds")
    } else {
      readRDS("processed_data_v4/ht_tracks_2024.rds")
    }
    
    tracks %>%
      filter(month >= input$month_filter[1] & month <= input$month_filter[2]) %>%
      sf::st_drop_geometry() %>%
      mutate(
        latitude = as.numeric(latitude),   # <-- Ensures latitude is a number
        longitude = as.numeric(longitude) # <-- Ensures longitude is a number
      ) %>%
      na.omit() # <-- Removes any rows that failed conversion
  })
  
  # Main exposure map
  output$exposure_map <- renderLeaflet({
    data_wgs84 <- exposure_data()
    tracks <- tracks_data()
    
    # --- PASTE THE PALETTE CALCULATION HERE ---
    exposure_pal <- colorNumeric(
      palette = viridis::rocket(100, direction = -1),
      domain = data_wgs84$log_exposure, # Use the already loaded data
      na.color = "transparent"
    )
    
    # Initialize map
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addPolygons(
        data = data_wgs84,
        fillColor = ~exposure_pal(log_exposure),
        weight = 0.5, opacity = 1, color = "white", fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
        ),
        popup = ~paste0(
          "<strong>", shapeName, "</strong><br/>",
          "Country/Territory: ", ifelse(!is.na(country), country, shapeGroup), "<br/>",
          "Person-Day Exposure: ", format(round(total_person_day_exposure), big.mark = ","), "<br/>",
          "Population: ", format(round(total_population), big.mark = ","), "<br/>",
          "Contributing Storms: ", sapply(contributing_storms, function(s) if(length(s) > 0 && !is.na(s[1])) paste(s, collapse=", ") else "N/A")
        ),
        label = ~shapeName
      )
    
    # Add country boundaries if selected
    if (input$show_boundaries) {
      map <- map %>%
        addPolygons(
          data = adm0_boundaries_wgs84,
          fill = FALSE, weight = 1, color = "gray", opacity = 0.5
        )
    }
    
    # Add storm tracks
    if (nrow(tracks) > 0) {
      storm_track_list <- tracks %>%
        dplyr::group_by(unique_identifier) %>%
        dplyr::group_split()
      
      for (storm_track in storm_track_list) {
        if (nrow(storm_track) > 1) {
          map <- map %>% addPolylines(
            data = storm_track %>% arrange(date),
            lng = ~longitude, lat = ~latitude,
            weight = 2, opacity = 0.9,
            color = ~month_pal(month),
            popup = ~paste0("<strong>Storm: ", storm_id[1], "</strong>"),
            label = ~storm_id[1]
          )
        }
      }
    }
    
    # Add legends
    map <- map %>%
      addLegend(
        position = "bottomright", pal = exposure_pal, values = ~log_exposure,
        data = data_wgs84, title = "Person-days exposure", opacity = 0.7,
        labFormat = labelFormat(transform = function(x) round(10^x - 0.1))
      )
    
    if (nrow(tracks) > 0) {
      map <- map %>%
        addLegend(
          position = "bottomleft", pal = month_pal, values = 1:12,
          title = "Month", opacity = 0.7,
          labFormat = labelFormat(transform = function(x) month.abb[x])
        )
    }
    
    map
  })
  
  # Storm tracks focused map
  output$tracks_map <- renderLeaflet({
    tracks <- tracks_data()
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = 0, lat = 20, zoom = 2)
    
    if (nrow(tracks) > 0) {
      wind_pal <- colorNumeric("YlOrRd", domain = tracks$wind, na.color = "transparent")
      
      storm_track_list <- tracks %>%
        dplyr::group_by(unique_identifier) %>%
        dplyr::group_split()
      
      for (storm_track in storm_track_list) {
        if (nrow(storm_track) > 1) {
          map <- map %>%
            addPolylines(
              data = storm_track %>% arrange(date),
              lng = ~longitude, lat = ~latitude,
              weight = 3, opacity = 0.8,
              color = ~wind_pal(wind)
            )
        }
      }
      
      map <- map %>%
        addLegend("bottomright", pal = wind_pal, values = ~wind, data = tracks,
                  title = "Wind Speed (kt)", opacity = 0.8)
    }
    map
  })
  
  # Top regions table
  output$top_regions <- DT::renderDataTable({
    exposure_data() %>%
      st_drop_geometry() %>%
      arrange(desc(total_person_day_exposure)) %>%
      head(10) %>%
      transmute(
        Region = shapeName,
        `Country/Territory` = ifelse(!is.na(country), country, shapeGroup),
        `Person-Day Exposure` = format(round(total_person_day_exposure), big.mark = ","),
        Population = format(round(total_population), big.mark = ",")
      ) %>%
      DT::datatable(options = list(pageLength = 10, dom = 't'))
  })
  
  # Storm information table
  output$storm_table <- DT::renderDataTable({
    tracks_data() %>%
      group_by(`Storm Name` = storm_id, ID = unique_identifier) %>%
      summarise(
        `Max Wind (kt)` = max(wind, na.rm = TRUE),
        `Start Date` = min(date),
        `End Date` = max(date),
        .groups = "drop"
      ) %>%
      arrange(desc(`Max Wind (kt)`)) %>%
      DT::datatable(options = list(pageLength = 10))
  })
  
  # Summary value boxes
  output$total_exposure <- renderValueBox({
    total <- sum(exposure_data()$total_person_day_exposure, na.rm = TRUE)
    valueBox(format(round(total), big.mark = ","), "Total Person-Day Exposure", icon = icon("users"), color = "blue")
  })
  
  output$affected_regions <- renderValueBox({
    valueBox(nrow(exposure_data()), "Affected Regions", icon = icon("map-marked-alt"), color = "green")
  })
  
  output$affected_population <- renderValueBox({
    total <- sum(exposure_data()$total_population, na.rm = TRUE)
    valueBox(format(round(total), big.mark = ","), "Total Exposed Population", icon = icon("globe"), color = "yellow")
  })
}

# Run the application
shinyApp(ui = ui, server = server)