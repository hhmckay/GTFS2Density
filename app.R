### GTFS2Density Shiny App

# Load libraries
library(shiny)
library(tools)
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(sf)
library(mapview)
library(lwgeom)
library(tidycensus)
library(tigris)
library(lehdr)
library(tidytransit)
library(ggplot2)
library(leaflet)
library(waiter)
library(shinycssloaders)

# Waiting screen to display when analysis is running
waiting_screen <- tagList(
  spin_3(),
  h4("Performing spatial analysis. This may take a few minutes.", style = "color: black;")
  )

### Global parameters/functions

# Parameters
crs <- 3310
max_distance <- 1609.34

# Functions (from the tidytransit R package)
filter_feed_by_trips = function(gtfs_obj, trip_ids) {
  route_ids = gtfs_obj$trips[which(gtfs_obj$trips$trip_id %in% trip_ids),]
  route_ids <- unique(route_ids$route_id)
  
  # first batch via trip_ids
  gtfs_obj$stop_times <- gtfs_obj$stop_times[which(gtfs_obj$stop_times$trip_id %in% trip_ids),]
  gtfs_obj$routes <- gtfs_obj$routes[which(gtfs_obj$routes$route_id %in% route_ids),]
  gtfs_obj$trips <- gtfs_obj$trips[which(gtfs_obj$trips$trip_id %in% trip_ids),]
  
  # other
  trip_stop_ids = gtfs_obj$stop_times$stop_id
  service_ids = unique(gtfs_obj$trips$service_id)
  gtfs_obj$stops <- gtfs_obj$stops[which(gtfs_obj$stops$stop_id %in% trip_stop_ids),]
  
  gtfs_obj$.$dates_services <- filter(gtfs_obj$.$dates_services, service_id %in% service_ids)
  if(feed_contains(gtfs_obj, "calendar")) {
    gtfs_obj$calendar <- gtfs_obj$calendar[which(gtfs_obj$calendar$service_id %in% service_ids),]
  }
  if(feed_contains(gtfs_obj, "calendar_dates")) {
    gtfs_obj$calendar_dates <- gtfs_obj$calendar_dates[which(gtfs_obj$calendar_dates$service_id %in% service_ids),]
  }
  if(feed_contains(gtfs_obj, "shapes")) {
    gtfs_obj$shapes <- gtfs_obj$shapes[which(gtfs_obj$shapes$shape_id %in% gtfs_obj$trips$shape_id),]
  }
  if(feed_contains(gtfs_obj, "frequencies")) {
    gtfs_obj$frequencies <- gtfs_obj$frequencies[which(gtfs_obj$frequencies$trip_id %in% trip_ids),]
  }
  if(feed_contains(gtfs_obj, "transfers")) {
    gtfs_obj$transfers <- gtfs_obj$transfers[which(
      gtfs_obj$transfers$from_stop_id %in% trip_stop_ids & 
        gtfs_obj$transfers$to_stop_id %in% trip_stop_ids),]
  }
  
  gtfs_obj
}

feed_contains <- function(gtfs_obj, table_name) {
  exists(table_name, where = gtfs_obj) ||
    (exists(".", where = gtfs_obj) && exists(table_name, where = gtfs_obj$.))
}

### Access land use data (currently set to read from local server)

# Get ACS population data from tidycensus
# ca_pop <- st_transform(get_acs(
#   geography = "block group",
#   state = 06,
#   variables = "B01003_001",
#   year = 2021,
#   survey = "acs5",
#   geometry = T
# ), crs = crs)
# 
# # Get employment data from LEHDR
# ca_jobs <- grab_lodes(
#   state = "ca",
#   year = 2021,
#   lodes_type = "wac",
#   job_type = "JT00",
#   segment = "S000",
#   agg_geo = "bg"
# ) %>%
#   select(w_bg, C000)
# 
# # Combine population/employment data into a single dataframe
# ca_demographics <- merge(
#   ca_pop,
#   ca_jobs,
#   by.x = "GEOID",
#   by.y = "w_bg",
#   all.x = T
# )

### Shiny UI
ui <- fluidPage(
  titlePanel("GTFS 2 Density"),
  sidebarLayout(
    sidebarPanel(
      fileInput("gtfs_zip", "Upload a zipped GTFS dataset", accept = ".zip"),
      uiOutput("route_select"),
      uiOutput("trip_select"),
      uiOutput("title_text"),
      uiOutput("spatial_gran"),
      uiOutput("distance_gran"),
      useWaiter(),
      actionButton("plot", "Generate Plot"),
      downloadButton("plot_download", "Download Plot")
    ),
    mainPanel(
      plotOutput("map"),
      textOutput("error_message")
    )
  )
)

### Shiny server
server <- function(input, output, session) {
  
  # Set max file input size to 30 MB (to enable large GTFS feeds)
  options(shiny.maxRequestSize=30*1024^2)
  
  # Read land use data from server
  ca_demographics <- read_rds("data/ca_demographics.rds") %>%
    st_set_crs(crs)
  
  # Read GTFS data
  gtfs_data <- reactive({
    req(input$gtfs_zip)
    gtfs <- read_gtfs(input$gtfs_zip$datapath)
    
    # Check if necessary files are present
    if (!all(c("routes", "trips", "shapes") %in% names(gtfs))) {
      return(NULL)
    }
    return(gtfs)
  })
  
  # Select route in UI from GTFS feed
  output$route_select <- renderUI({
    gtfs <- gtfs_data()
    req(gtfs)
    
    # Extract routes
    routes <- gtfs$routes
    
    # Create route labels
    route_labels <- paste0(routes$route_short_name, " - ", routes$route_long_name)
    route_choices <- setNames(routes$route_id, route_labels)
    
    # Define ID/UI parmaters of route selection
    selectInput("route_id", "Select Route", choices = route_choices, selected = NULL)
  })
  
  # Select trip in UI from GTFS feed
  output$trip_select <- renderUI({
    gtfs <- gtfs_data()
    req(gtfs)
    req(input$route_id)
    
    # Extract trips
    route_trips <- gtfs$trips %>% filter(route_id == input$route_id)
    
    # Return null if no trips present
    if (nrow(route_trips) == 0) {
      return(NULL)
    }
    
    # Create trip labels
    trip_labels <- paste0(route_trips$trip_id, " - ", route_trips$trip_headsign)
    trip_choices <- setNames(route_trips$trip_id, trip_labels)
    
    # Define ID/UI parameters of trip selection
    selectInput("trip_id", "Select Trip", choices = trip_choices, selected = NULL)
  })
  
  # Error message output (untested)
  output$error_message <- renderText({
    if (is.null(gtfs_data())) {
      return("Invalid GTFS file or missing routes, trips, or shapes data.")
    }
    return(NULL)
  })
  
  # Chart title input
  output$title_text <- renderUI({
    gtfs <- gtfs_data()
    req(gtfs)
    req(input$route_id)
    req(input$trip_id)
    textInput("title_text", "Graph Title")
  })
  
  # Spatial granularity input
  output$spatial_gran <- renderUI({
    gtfs <- gtfs_data()
    req(gtfs)
    req(input$route_id)
    req(input$trip_id)
    numericInput("spatial_gran", "Land Use Granularity", value = 100, min = 1, max = 1000)
  })
  
  # Distance granularity input
  output$distance_gran <- renderUI({
    gtfs <- gtfs_data()
    req(gtfs)
    req(input$route_id)
    req(input$trip_id)
    numericInput("distance_gran", "Distance Granularity", value = 50, min = 5, max = 500)
  })
  
  # Get stop data for selected trip
  stop_shape_data <- reactive({
    req(input$route_id)
    gtfs <- gtfs_data()
    
    # Filter GTFS feed by trip ID
    gtfs_filtered <- filter_feed_by_trips(gtfs, input$trip_id)
    
    # Get stops for selected trip
    stops <- st_as_sf(gtfs_filtered$stops, coords = c("stop_lon", "stop_lat"), crs = 4326)
    stops <- st_transform(stops, crs = crs)
    return(stops)
  })
  
  # Get shape data for selected trip
  route_shape_data <- reactive({
    req(input$route_id)
    gtfs <- gtfs_data()
    
    # Filter GTFS feed by trip ID
    gtfs_filtered <- filter_feed_by_trips(gtfs, input$trip_id)

    # Get route shape
    route <- gtfs_as_sf(gtfs_filtered)
    route <- st_transform(st_as_sf(st_union(route$shapes)), crs = crs)
    
    # Function to generate points along a polyline
    generate_points_along_sf_polyline <- function(polyline, interval) {
      total_length <- as.numeric(st_length(polyline))
      distances <- seq(0, total_length, by = interval)
      points <- st_line_sample(polyline, sample = distances / total_length)
      points <- st_cast(points, "POINT")
      points_sf <- st_sf(geometry = points)
      points_sf$Distance <- distances
      return(points_sf)
    }
    
    # Generate points along trip shape, at defined spacing (distance granularity, in meters)
    route_points <- st_transform(generate_points_along_sf_polyline(route, input$distance_gran), crs = crs)
    return(route_points)
  })
  
  ### Perform spatial analysis and generate plot
  observeEvent(input$plot, {
    
    # Show loading screen while analysis is running
    waiter_show(html = waiting_screen, color = "white")
    
    req(route_shape_data())
    req(stop_shape_data())
    
    # Create a 1 mile buffer around selected trip shape
    route_points <- route_shape_data()
    route_buffer <- st_buffer(route_points, max_distance)
    route_buffer <- st_as_sf(st_union(route_buffer))
    
    # Filter land use geometries to buffer around specified trip shape
    geo_within_distance <- ca_demographics %>%
      filter(st_intersects(geometry, route_buffer, sparse = FALSE))

    # Generate random points representing pop/job density
     pop_dots <- as_dot_density(
       geo_within_distance,
       "estimate",
       values_per_dot = input$spatial_gran,
       group = NULL,
       erase_water = FALSE,
       area_threshold = NULL,
       water_year = 2020
     )
    
     job_dots <- as_dot_density(
       geo_within_distance,
       "C000",
       values_per_dot = input$spatial_gran,
       group = NULL,
       erase_water = FALSE,
       area_threshold = NULL,
       water_year = 2020
     )
     
     # Function to count the number of points within a polygon
     count_pop_in_buffer <- function(polygons, points) {
       counts <- numeric(nrow(polygons))
       for (i in 1:nrow(polygons)) {
         counts[i] <- sum(st_within(points, polygons[i, ], sparse = FALSE))
       }
       return(counts)
     }
    
     # Function to run spatial analysis
     run_analysis <- function(buff_dist, dist_miles) {
    
       # Create buffer around route points at specified distance
       points_buffer <- st_buffer(route_points, buff_dist)
       
       # Count points in buffer (calling count_pop_in_buffer function)
       buffer_counts_pop <- count_pop_in_buffer(points_buffer, pop_dots)
    
       # Combine buffer count results
       buffer_counts_pop <- data.frame(Distance = points_buffer$Distance, Count_temp = buffer_counts_pop) %>%
         mutate(Count = Count_temp * (-1 * input$spatial_gran))
    
       # Create a name for the measure being generated
       buffer_counts_pop$Measure <- paste0("People (",dist_miles,")")
       
       # Create a name for the density type being calculated
       buffer_counts_pop$Type <- "People"
    
       # Same as above, but for jobs
       buffer_counts_jobs <- count_pop_in_buffer(points_buffer, job_dots)
       buffer_counts_jobs <- data.frame(Distance = points_buffer$Distance, Count_temp = buffer_counts_jobs) %>%
         mutate(Count = Count_temp * input$spatial_gran)
       buffer_counts_jobs$Measure <- paste0("Jobs (",dist_miles,")")
       buffer_counts_jobs$Type <- "Jobs"
       
       # Combine population and employment data
       df <- rbind(buffer_counts_pop, buffer_counts_jobs)
       return(df)
     }
    
    # Run the above function for multiple buffer distances
    df_1_mile <- run_analysis(1609.34, "1 Mile") %>%
      mutate(unique_id = paste0(Type, "_", Distance)) %>%
      rename("Count_1_Mile" = Count) %>%
      select(unique_id, Count_1_Mile)

    df_3_4_mile <- run_analysis(1207.01, "3/4 Mile") %>%
      mutate(unique_id = paste0(Type, "_", Distance)) %>%
      rename("Count_3_4_Mile" = Count) %>%
      select(unique_id, Count_3_4_Mile)

    df_1_2_mile <- run_analysis(804.672, "1/2 Mile") %>%
      mutate(unique_id = paste0(Type, "_", Distance)) %>%
      rename("Count_1_2_Mile" = Count) %>%
      select(unique_id, Count_1_2_Mile)

    df_1_4_mile <- run_analysis(402.336, "1/4 Mile") %>%
      mutate(unique_id = paste0(Type, "_", Distance)) %>%
      rename("Count_1_4_Mile" = Count) %>%
      select(unique_id, Count_1_4_Mile)

    # Merge dataframes
    df <- merge(df_1_mile,
                df_3_4_mile,
                by = "unique_id",
                all = T)

    df <- merge(df,
                df_1_2_mile,
                by = "unique_id",
                all = T)

    df <- merge(df,
                df_1_4_mile,
                by = "unique_id",
                all = T)
    
    # Subtract #s so that counts aren't cumulative (for stacked area chart)
    df_1_mile <- df %>%
      mutate(Count = Count_1_Mile - Count_3_4_Mile) %>%
      mutate(Measure = paste0(sub("(.*)_.*", "\\1", unique_id), " (1 Mile)")) %>%
      mutate(Distance = sub(".*_(.*)", "\\1", unique_id)) %>%
      select(Count, Measure, Distance)

    df_3_4_mile <- df %>%
      mutate(Count = Count_3_4_Mile - Count_1_2_Mile) %>%
      mutate(Measure = paste0(sub("(.*)_.*", "\\1", unique_id), " (3/4 Mile)")) %>%
      mutate(Distance = sub(".*_(.*)", "\\1", unique_id)) %>%
      select(Count, Measure, Distance)

    df_1_2_mile <- df %>%
      mutate(Count = Count_1_2_Mile - Count_1_4_Mile) %>%
      mutate(Measure = paste0(sub("(.*)_.*", "\\1", unique_id), " (1/2 Mile)")) %>%
      mutate(Distance = sub(".*_(.*)", "\\1", unique_id)) %>%
      select(Count, Measure, Distance)

    df_1_4_mile <- df %>%
      mutate(Count = Count_1_4_Mile) %>%
      mutate(Measure = paste0(sub("(.*)_.*", "\\1", unique_id), " (1/4 Mile)")) %>%
      mutate(Distance = sub(".*_(.*)", "\\1", unique_id)) %>%
      select(Count, Measure, Distance)

    # Combine data
    df <- rbind(df_1_mile, df_3_4_mile, df_1_2_mile, df_1_4_mile)
    
    # Ensure that Distance column is numeric
    df$Distance <- as.numeric(df$Distance)

    # Get stop data
    stops <- stop_shape_data()

    # Calculate nearest shape point to each stop location
    nearest_index <- st_nearest_feature(stops, route_points)
    nearest_points <- route_points[nearest_index, ]

    # Merge attributes from shape points to stop points
    stations_with_attributes <- st_sf(id = stops$stop_name,
                                      Distance = nearest_points$Distance,
                                      geometry = stops$geometry) %>%
      st_drop_geometry()
    
     # Merge density dataframe with stops dataframe
     df <- merge(df,
                 stations_with_attributes,
                 by = "Distance",
                 all.x = T)
    
     # Replace NA values with blank strings
     df[is.na(df)] <- ""
    
     ### Generate graph
     
     # Function for axis formatting
     abs_comma <- function (x, ...) {
       format(abs(x), ..., big.mark = ",", scientific = FALSE, trim = TRUE)
     }
    
     # Define colors for chart
     group.colors <- c("People (1 Mile)" = "#7D9514",
                       "People (3/4 Mile)" = "#B0AC1A",
                       "People (1/2 Mile)" = "#CAA121",
                       "People (1/4 Mile)" = "#E48F29",
                       "Jobs (1 Mile)" = "#D4DBCE",
                       "Jobs (3/4 Mile)" = "#91AA8E",
                       "Jobs (1/2 Mile)" = "#4F785B",
                       "Jobs (1/4 Mile)" = "#114533")
    
     # Define factors for chart
     df$Measure <- factor(df$Measure, levels=c("People (1 Mile)", "People (3/4 Mile)", "People (1/2 Mile)", "People (1/4 Mile)",
                                               "Jobs (1 Mile)", "Jobs (3/4 Mile)", "Jobs (1/2 Mile)", "Jobs (1/4 Mile)"))
    
     # Create stacked area chart
     plot <- ggplot(df, aes(x=Distance, y=Count, fill=Measure)) +
       geom_area() +
       scale_y_continuous(labels = abs_comma) +
       scale_x_continuous(breaks = df$Distance, labels = df$id, minor_breaks = NULL) +
       scale_fill_manual(values=group.colors) +
       coord_flip() + 
       theme(axis.title.x=element_blank(),
             axis.title.y=element_blank(),
             axis.ticks.y=element_blank()) +
       theme_minimal() +
       labs(title = input$title_text,
            subtitle = "Population and Employment Density") +
       theme(
         plot.title = element_text(color = "grey23", size = 25, face = "bold"),
         plot.subtitle = element_text(color = "grey23", size = 12),
         plot.caption = element_text(color = "grey23", size = 15, face = "italic", hjust = 0),
         legend.title=element_text(color = "grey23", size=15, face = "bold"),
         axis.text.y = element_text(size = 6),
         axis.text.x = element_text(size = 8)
       )

     # Render plot
     output$map <- renderPlot({
       plot
     })
     
     # Close waiting screen once analysis is run
     waiter_hide()
     
     # Enable download
     output$plot_download = downloadHandler(
       filename = paste0(input$title_text,' Density Chart.png'),
       content = function(file) {
         device <- function(..., width, height) {
           grDevices::png(..., width = 8.5, height = 11,
                          res = 600, units = "in")
         }
         ggsave(file, plot = plot, device = device, bg = "white")
       })
  })
}

# Run app
shinyApp(ui = ui, server = server)

