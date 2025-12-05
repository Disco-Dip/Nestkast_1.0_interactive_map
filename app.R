library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Load data
data <- read.csv("data/final_data.csv", sep = ";")
outline_df <- read.csv("data/1900outline.csv")

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  titlePanel("Breeding bird data 1922-1950"),
  
  tags$head(
    tags$style(HTML("
      html, body { margin: 0; padding: 0; height: 100%; overflow: hidden; }
      #map-wrapper { position: relative; width: 100%; height: calc(100vh - 80px); display: flex; justify-content: center; align-items: center; }
      #map-center { width: 100%; max-width: 1200px; height: 100%; }
      .floating-panel { position: absolute; top: 80px; left: 20px; z-index: 1000; background-color: rgba(255, 255, 255, 0.9); padding: 15px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.3); width: 280px; }
      #mapPlot { width: 100%; height: 100%; }
    "))
  ),
  
  div(class = "floating-panel",
      selectInput("selected_location", "Search for an area",
                  choices = c("None", sort(unique(data$modern_location))),
                  selected = "None"),
      selectInput("filter_species", "Filter by species",
                  choices = c("None", sort(unique(data$species))),
                  selected = "None"),
      selectInput("filter_year", "Filter by year",
                  choices = c("None", sort(unique(data$year))),
                  selected = "None"),
      
      # Buttons side by side
      div(style = "display: flex; gap: 10px; margin-bottom: 10px;",
          actionButton("clear_found", "Clear all", class = "btn-danger"),
          uiOutput("clear_selection_ui")  # Conditional button
      ),
      
      tags$hr(),
      div(class="summary-box", tableOutput("table"))
  ),
  
  plotlyOutput("mapPlot", height = "600px")
)

# ----------------------
# server
# ----------------------

server <- function(input, output, session) {
  nofilter <- "None"
  
  # Store box-selected points
  found_locs <- reactiveVal(character(0))
  
  zoom_state <- reactiveValues(x = NULL, y = NULL)
  
  relayout_data <- reactive({
    event_data("plotly_relayout")
  }) %>% debounce(250)
  
  observeEvent(relayout_data(), {
    evt <- relayout_data()
    if (!is.null(evt[["xaxis.range[0]"]])) {
      zoom_state$x <- c(evt[["xaxis.range[0]"]], evt[["xaxis.range[1]"]])
    }
    if (!is.null(evt[["yaxis.range[0]"]])) {
      zoom_state$y <- c(evt[["yaxis.range[0]"]], evt[["yaxis.range[1]"]])
    }
  })
  
  
  # ---------------------------
  # Reactive filtered data
  # ---------------------------
  filtered_data <- reactive({
    df <- data
    if (!is.null(input$selected_location) && input$selected_location != nofilter) {
      df <- df %>% filter(modern_location == input$selected_location)
    }
    if (!is.null(input$filter_species) && input$filter_species != nofilter) {
      df <- df %>% filter(species == input$filter_species)
    }
    if (!is.null(input$filter_year) && input$filter_year != nofilter) {
      df <- df %>% filter(year == as.numeric(input$filter_year))
    }
    df
  })
  
  # ---------------------------
  # Update filter choices dynamically (dependent filters)
  # ---------------------------
  observe({
    df <- data
    
    # Apply filters except for the one being updated
    loc_choices <- df
    if (!is.null(input$filter_species) && input$filter_species != nofilter) {
      loc_choices <- loc_choices %>% filter(species == input$filter_species)
    }
    if (!is.null(input$filter_year) && input$filter_year != nofilter) {
      loc_choices <- loc_choices %>% filter(year == as.numeric(input$filter_year))
    }
    updateSelectInput(session, "selected_location",
                      choices = c(nofilter, sort(unique(loc_choices$modern_location))),
                      selected = input$selected_location)
    
    species_choices <- df
    if (!is.null(input$selected_location) && input$selected_location != nofilter) {
      species_choices <- species_choices %>% filter(modern_location == input$selected_location)
    }
    if (!is.null(input$filter_year) && input$filter_year != nofilter) {
      species_choices <- species_choices %>% filter(year == as.numeric(input$filter_year))
    }
    updateSelectInput(session, "filter_species",
                      choices = c(nofilter, sort(unique(species_choices$species))),
                      selected = input$filter_species)
    
    year_choices <- df
    if (!is.null(input$selected_location) && input$selected_location != nofilter) {
      year_choices <- year_choices %>% filter(modern_location == input$selected_location)
    }
    if (!is.null(input$filter_species) && input$filter_species != nofilter) {
      year_choices <- year_choices %>% filter(species == input$filter_species)
    }
    updateSelectInput(session, "filter_year",
                      choices = c(nofilter, sort(unique(year_choices$year))),
                      selected = input$filter_year)
    
    # Clear box selection if current found points are no longer visible
    visible_locs <- unique(filtered_data()$modern_location)
    found_locs(intersect(found_locs(), visible_locs))
  })
  
  # ---------------------------------------------------------
  # Click on a point to toggle inside/outside status
  # ---------------------------------------------------------
  observeEvent(event_data("plotly_click", source = "mapPlot"), {
    click <- event_data("plotly_click", source = "mapPlot")
    if (is.null(click$key)) return()
    
    clicked_loc <- click$key
    current <- found_locs()
    
    # 1. If clicked point is already inside → remove it
    if (clicked_loc %in% current) {
      new_set <- setdiff(current, clicked_loc)
      found_locs(new_set)
      
      # If ALL remaining filtered points are out → reset to default
      visible <- unique(filtered_data()$modern_location)
      if (length(new_set) > 0 && all(!(visible %in% new_set))) {
        found_locs(character(0))
      }
      
      # 2. If clicked point is outside → add it to inside
    } else {
      found_locs(unique(c(current, clicked_loc)))
    }
    
    # 3. After toggling, if NO points remain inside → reset
    if (length(found_locs()) == 0) {
      found_locs(character(0))
    }
  })
  
  
  # ---------------------------
  # Points for plotting (with status)
  # ---------------------------
  loc_plot <- reactive({
    df <- filtered_data() %>%
      group_by(modern_location, latitude, longitude) %>%
      summarise(records = n(), .groups = "drop") %>%
      mutate(status = case_when(
        length(found_locs()) > 0 & modern_location %in% found_locs() ~ "inside",
        length(found_locs()) > 0 & !(modern_location %in% found_locs()) ~ "outside",
        TRUE ~ "default"
      ))
    df
  })
  
  
  # ---------------------------
  # Map rendering
  # ---------------------------
  output$mapPlot <- renderPlotly({
    df <- loc_plot()
    
    color_map <- c(
      "default" = "#B22600",
      "inside"  = "#B22600",
      "outside" = "#F0B0A3"
    )
    
    p <- ggplot() +
      geom_polygon(data = outline_df, aes(x=X, y=Y, group=L1),
                   fill="#f4ecd8", color="#5c4033", show.legend = FALSE) +
      geom_point(data = df,
                 aes(x=longitude, y=latitude, size=records, fill=status, key=modern_location, text=modern_location),
                 shape=16, color = NA, alpha=0.6,
                 show.legend = TRUE) +
      scale_fill_manual(values = color_map) +
      coord_fixed(ratio=1.7) +
      theme_classic() +
      theme(axis.title=element_blank(), axis.text=element_blank(),
            axis.ticks=element_blank(), axis.line=element_blank())
    
    ggplotly(p, tooltip="text", source="mapPlot") |>
      event_register("plotly_relayout") |>
      layout(dragmode="select", showlegend = FALSE,
             xaxis = isolate(if (!is.null(zoom_state$x)) list(range = zoom_state$x) else NULL),
             yaxis = isolate(if (!is.null(zoom_state$y)) list(range = zoom_state$y) else NULL)) |>
      config(scrollZoom=TRUE, displayModeBar=TRUE)
  })
  
  
  
  # ---------------------------
  # Box select points (additive)
  # ---------------------------
  observeEvent(event_data("plotly_selected", source="mapPlot"), {
    pts <- event_data("plotly_selected", source="mapPlot")$key
    if(length(pts) > 0){
      found_locs(unique(c(found_locs(), pts)))
    }
  })
  
  # Conditional Clear Selection button
  output$clear_selection_ui <- renderUI({
    if(length(found_locs()) > 0){
      actionButton("clear_selection", "Clear selection", class = "btn-warning")
    } else {
      NULL  # no button if nothing selected
    }
  })
  
  
  # ---------------------------
  # Clear all button
  # ---------------------------
  observeEvent(input$clear_found, {
    found_locs(character(0))
    updateSelectInput(session, "selected_location", selected = nofilter)
    updateSelectInput(session, "filter_species", selected = nofilter)
    updateSelectInput(session, "filter_year", selected = nofilter)
  })
  
  # ---------------------------
  # Clear selection button
  # ---------------------------
  observeEvent(input$clear_selection, {
    found_locs(character(0))
  })
  
  # ---------------------------
  # Reset box selection on filter change
  # ---------------------------
  observeEvent(list(input$selected_location, input$filter_species, input$filter_year), {
    found_locs(character(0))
  })
  
  # ---------------------------
  # Summary table
  # ---------------------------
  output$table <- renderTable({
    df <- filtered_data()
    sel_locs <- found_locs()
    
    # Only include box-selected points if they exist
    if(length(sel_locs) > 0){
      df <- df %>% filter(modern_location %in% sel_locs)
    }
    
    tibble(
      Metric = c("Areas", "Species", "Years", "Records"#,
                 #if (!is.null(input$filter_species) && input$filter_species != nofilter)
                #   paste0("Records for ", input$filter_species),
                # if (!is.null(input$filter_year) && input$filter_year != nofilter)
                #   paste0("Records for ", input$filter_year)
                ),
      Value  = c(n_distinct(df$modern_location),
                 n_distinct(df$species),
                 n_distinct(df$year),
                 nrow(df)#,
                 #if (!is.null(input$filter_species) && input$filter_species != nofilter)
                #   nrow(df %>% filter(species == input$filter_species)),
                # if (!is.null(input$filter_year) && input$filter_year != nofilter)
                #   nrow(df %>% filter(year == as.numeric(input$filter_year)))
                )
    )
  })
}

# ----------------------
# Run app
# ----------------------

shinyApp(ui, server)
