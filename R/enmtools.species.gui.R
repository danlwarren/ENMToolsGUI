library(shiny)
library(bslib)
library(leaflet)
library(terra)
library(sf)
library(dplyr)

ui <- page_sidebar(
  title = "ENMTools Species Builder",
  sidebar = sidebar(
    width = 300,
    textInput("species_name", "Species Name"),

    # Presence points section
    fileInput("presence_file", "Upload Presence Points (CSV)",
              accept = c(".csv")),
    uiOutput("presence_columns"),

    # Background points section
    fileInput("background_file", "Upload Background Points (CSV)",
              accept = c(".csv")),
    uiOutput("background_columns"),

    fileInput("range_file", "Upload Range Raster",
              accept = c(".tif", ".asc")),
    actionButton("build_species", "Build Species Object",
                 class = "btn-primary"),

    # Add download button
    uiOutput("download_ui")
  ),

  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Species Map"),
      leafletOutput("map", height = "600px")
    ),
    card(
      card_header("Species Object Summary"),
      verbatimTextOutput("species_summary")
    )
  )
)

server <- function(input, output, session) {

  # Reactive values to store data
  species_data <- reactiveValues(
    presence = NULL,
    background = NULL,
    range = NULL,
    species_obj = NULL,
    presence_df = NULL,
    background_df = NULL
  )

  # Load presence data
  observeEvent(input$presence_file, {
    species_data$presence_df <- read.csv(input$presence_file$datapath)
    species_data$presence <- NULL  # Clear any existing presence points
  })

  # Load background data
  observeEvent(input$background_file, {
    species_data$background_df <- read.csv(input$background_file$datapath)
    species_data$background <- NULL  # Clear any existing background points
  })

  # Dynamic UI for presence points columns
  output$presence_columns <- renderUI({
    req(species_data$presence_df)
    cols <- names(species_data$presence_df)

    tagList(
      selectInput("presence_lon", "Longitude Column (Presence)",
                  choices = c("Select column" = "", cols),
                  selected = if("longitude" %in% cols) "longitude" else ""),
      selectInput("presence_lat", "Latitude Column (Presence)",
                  choices = c("Select column" = "", cols),
                  selected = if("latitude" %in% cols) "latitude" else "")
    )
  })

  # Dynamic UI for background points columns
  output$background_columns <- renderUI({
    req(species_data$background_df)
    cols <- names(species_data$background_df)

    tagList(
      selectInput("background_lon", "Longitude Column (Background)",
                  choices = c("Select column" = "", cols),
                  selected = if("longitude" %in% cols) "longitude" else ""),
      selectInput("background_lat", "Latitude Column (Background)",
                  choices = c("Select column" = "", cols),
                  selected = if("latitude" %in% cols) "latitude" else "")
    )
  })

  # Create presence points when columns are selected
  observe({
    req(species_data$presence_df)
    req(input$presence_lon != "", input$presence_lat != "")

    tryCatch({
      # Create SpatVector with selected columns
      species_data$presence <- vect(species_data$presence_df,
                                    geom = c(input$presence_lon, input$presence_lat),
                                    crs = "epsg:4326")
    }, error = function(e) {
      species_data$presence <- NULL
    })
  })

  # Create background points when columns are selected
  observe({
    req(species_data$background_df)
    req(input$background_lon != "", input$background_lat != "")

    tryCatch({
      # Create SpatVector with selected columns
      species_data$background <- vect(species_data$background_df,
                                      geom = c(input$background_lon, input$background_lat),
                                      crs = "epsg:4326")
    }, error = function(e) {
      species_data$background <- NULL
    })
  })

  # Handle range raster upload
  observeEvent(input$range_file, {
    req(input$range_file)
    tryCatch({
      species_data$range <- rast(input$range_file$datapath)
    }, error = function(e) {
      species_data$range <- NULL
    })
  })

  # Build species object when button is clicked
  observeEvent(input$build_species, {
    # Get species name (if not provided, use NA)
    species_name <- if(input$species_name != "") input$species_name else NA

    # Create species object with whatever data is available
    species_data$species_obj <- enmtools.species(
      range = if(!is.null(species_data$range)) species_data$range else NA,
      presence.points = if(!is.null(species_data$presence)) species_data$presence else NA,
      background.points = if(!is.null(species_data$background)) species_data$background else NA,
      species.name = species_name
    )
  })

  # Show download button only when species object exists
  output$download_ui <- renderUI({
    req(species_data$species_obj)
    downloadButton("download_species", "Download Species Object")
  })

  # Handle the download
  output$download_species <- downloadHandler(
    filename = function() {
      species_name <- if(input$species_name != "") input$species_name else "unnamed"
      paste0(species_name, "_species.rds")
    },
    content = function(file) {
      # Create a temporary file with .rda extension
      temp_file <- tempfile(fileext = ".rda")

      # Use save.enmtools.species to properly save the object
      x <- species_data$species_obj
      save.enmtools.species(x, file = temp_file)

      # Copy the temporary file to the target file
      file.copy(temp_file, file)
      unlink(temp_file)  # Clean up temporary file
    }
  )

  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })

  # Update map when data changes
  observe({
    map <- leafletProxy("map")

    map %>% clearShapes() %>% clearMarkers()

    # Add range raster if available
    if (!is.null(species_data$range)) {
      map %>% addRasterImage(species_data$range,
                             colors = colorNumeric("OrRd",
                                                   values(species_data$range),
                                                   na.color = "transparent"))
    }

    # Add presence points if available
    if (!is.null(species_data$presence)) {
      presence_coords <- terra::crds(species_data$presence)
      map %>% addCircleMarkers(
        lng = presence_coords[,1],
        lat = presence_coords[,2],
        radius = 5,
        color = "black",
        fillOpacity = 0.8
      )
    }

    # Add background points if available
    if (!is.null(species_data$background)) {
      background_coords <- terra::crds(species_data$background)
      map %>% addCircleMarkers(
        lng = background_coords[,1],
        lat = background_coords[,2],
        radius = 3,
        color = "red",
        fillOpacity = 0.5
      )
    }
  })

  # Display species object summary
  output$species_summary <- renderPrint({
    req(species_data$species_obj)
    summary(species_data$species_obj)
  })
}

shinyApp(ui, server)
