library(shiny)
library(shinydashboard)
library(shinybusy)
library(leaflet)
library(sf)
library(dplyr)
library(viridis)
library(DT)
library(tidyr) # Needed for unnest()

# ========================================
# LOAD PRE-PROCESSED DATA (ONCE AT STARTUP)
# ========================================

# Check if processed data exists
if(!dir.exists("processed_data_v4")) {
  stop("Processed data not found! Please run 'preprocess_data.R' first.")
}

# Load only the minimal data needed at startup
cat("Loading pre-processed data...\n")
adm0_boundaries_wgs84 <- readRDS("processed_data_v4/adm0_boundaries_wgs84.rds")
summary_stats <- readRDS("processed_data_v4/summary_stats.rds")
cat("Data loaded successfully!\n")

# ========================================
# PRE-CALCULATE PALETTES
# ========================================

month_pal <- colorFactor(
  palette = viridis::mako(12),
  domain = 1:12
)

# ========================================
# SHINY UI
# ========================================

ui <- dashboardPage(
  dashboardHeader(title = "G-TROPIC 2024"),
  
  dashboardSidebar(
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
    use_busy_spinner(
      spin = "fading-circle",
      position = "full-page"),
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    
    # Row 1: The Map
    fluidRow(
      column(12,
             box(
               title = "Global exposure map with storm tracks",
               width = 12, status = "primary", solidHeader = TRUE,
               leafletOutput("exposure_map", height = "600px")
             )
      )
    ),
    
    # Row 2: Exposure summary
    fluidRow(
      column(12,
             box(
               title = "Exposure summary",
               width = 12, status = "primary", solidHeader = TRUE,
               height = "200px",
               valueBoxOutput("total_exposure"),
               valueBoxOutput("affected_regions"),
               valueBoxOutput("affected_population")
             )
      )
    ),
    
    fluidRow(
      column(12, 
             # Box 1: Top regions table
             column(6,
                    box(
                      title = "Top 10 most affected regions",
                      width = 12, status = "primary", solidHeader = TRUE,
                      # Add a height for better alignment
                      height = "450px", 
                      DT::dataTableOutput("top_regions")
                    )
             ),
             
             # Box 2: Top Storms Table
             column(6,
                    box(
                      title = "Top 10 storms by exposure",
                      width = 12, status = "primary", solidHeader = TRUE,
                      height = "450px",
                      DT::dataTableOutput("storm_table")
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
  # Reactive exposure data (lazy loading)
  exposure_data <- reactive({
    if (input$exposure_type == "tc") {
      readRDS("processed_data_v4/adm2_tc_exp_wgs84.rds")
    } else {
      readRDS("processed_data_v4/adm2_hurr_exp_wgs84.rds")
    }
  })
  
  # Reactive tracks data (lazy loading)
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
        latitude = as.numeric(latitude),
        longitude = as.numeric(longitude)
      ) %>%
      na.omit()
  })
  
  # Main exposure map
  output$exposure_map <- renderLeaflet({
    req(exposure_data())
    data_wgs84 <- exposure_data()
    tracks <- tracks_data()
    
    exposure_pal <- colorNumeric(
      palette = viridis::rocket(100, direction = -1),
      domain = data_wgs84$log_exposure,
      na.color = "transparent"
    )
    
    map <- leaflet(data = data_wgs84) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addPolygons(
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
          "Contributing Storm(s): ", sapply(contributing_storms, function(s) if(length(s) > 0 && !is.na(s[1])) paste(s, collapse=", ") else "N/A"),  "<br/>",
          "Relative GRDI: ", ifelse(!is.na(quartile), quartile, quartile)
        ),
        label = ~shapeName
      )
    
    if (input$show_boundaries) {
      map <- map %>%
        addPolygons(
          data = adm0_boundaries_wgs84,
          fill = FALSE, weight = 1, color = "gray", opacity = 0.5
        )
    }
    
    if (nrow(tracks) > 0) {
      storm_track_list <- tracks %>%
        group_by(unique_identifier) %>%
        group_split()
      
      for (storm_track in storm_track_list) {
        if (nrow(storm_track) > 1) {
          map <- map %>% addPolylines(
            data = storm_track %>% arrange(date),
            lng = ~longitude, lat = ~latitude,
            weight = 2, opacity = 0.9, color = ~month_pal(month),
            
            popup = ~paste0("<strong>Storm: </strong>", storm_id[1]),
            label = ~storm_id[1]
          )
        }
      }
    }
    
    map <- map %>%
      addLegend(
        position = "bottomright", pal = exposure_pal, values = ~log_exposure,
        title = "Person-days exposure", opacity = 0.7,
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
  
  # Top exposed ADM2 table
  output$top_regions <- DT::renderDataTable({
    req(exposure_data())
    exposure_data() %>%
      st_drop_geometry() %>%
      arrange(desc(total_person_day_exposure)) %>%
      transmute(
        Region = shapeName,
        `Country/Territory` = ifelse(!is.na(country), country, shapeGroup),
        `Person-Day Exposure` = format(round(total_person_day_exposure), big.mark = ",")
      ) %>%
      DT::datatable(
        options = list(
          pageLength = -1,  # Show ALL rows (no pagination)
          scrollY = "300px",  # Fixed height with scroll
          scrollCollapse = TRUE,
          dom = 'ft',  # 'f' for filter/search, 't' for table only
          order = list(list(2, 'desc'))  # Sort by Person-Day Exposure column
        ), 
        rownames = FALSE
      )
  })
  
  # Top contributing storms table
  output$storm_table <- DT::renderDataTable({
    
    # Load the correct, pre-summarized storm data file
    storm_file <- if (input$exposure_type == "tc") {
      "processed_data_v4/tc_pday_by_storm.rds"
    } else {
      "processed_data_v4/hurr_pday_by_storm.rds"
    }
    
    # Read the data and format the table
    readRDS(storm_file) %>%
      arrange(desc(total_pday)) %>%
      transmute(
        `Storm Name` = storm_id,
        `Total person-day exposure` = format(round(total_pday), big.mark = ",")
      ) %>%
      DT::datatable(
        options = list(pageLength = -1, 
                       scrollY = "300px",
                       scrollCollapse = TRUE,
                       dom = "ft",
                       order = list(list(1, "desc"))
        ), 
        rownames = FALSE
      )
  })
  
  # Summary value boxes
  output$total_exposure <- renderValueBox({
    req(exposure_data())
    total <- sum(exposure_data()$total_person_day_exposure, na.rm = TRUE)
    valueBox(format(round(total), big.mark = ","), "Total person-day exposure", icon = icon("users"), color = "maroon")
  })
  
  output$affected_regions <- renderValueBox({
    req(exposure_data())
    valueBox(nrow(exposure_data()), "Affected Regions", icon = icon("map-marked-alt"), color = "orange")
  })
  
  output$affected_population <- renderValueBox({
    req(exposure_data())
    total <- sum(exposure_data()$total_population, na.rm = TRUE)
    valueBox(format(round(total), big.mark = ","), "Total Exposed Population", icon = icon("globe"), color = "purple")
  })
}

# Run the application
shinyApp(ui = ui, server = server)