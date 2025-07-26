# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny)
library(cluster)

# Read the dataset
ev_data <- read.csv("Electric_Vehicle_Population_Data.csv")

# Sample dataset for Github keeping key columns
sample_data <- ev_data %>%
  select(VIN..1.10., City, Electric.Range, Base.MSRP, Electric.Vehicle.Type) %>%
  distinct() %>%
  head(100)  # or use sample_n(ev_data, 100)
write.csv(sample_data, "sample_ev_data.csv", row.names = FALSE)

ev_data <- read.csv("sample_ev_data.csv")

# Basic cleaning and summarization by city
clean_ev <- ev_data %>%
  distinct(VIN..1.10., .keep_all = TRUE) %>%  # Remove duplicate vehicles
  filter(!is.na(City), !is.na(Electric.Range), !is.na(Base.MSRP)) %>%
  group_by(City) %>%
  summarise(
    num_vehicles = n(),
    avg_range = mean(Electric.Range, na.rm = TRUE),
    avg_msrp = mean(Base.MSRP, na.rm = TRUE),
    pct_battery = mean(Electric.Vehicle.Type == "Battery Electric Vehicle (BEV)")
  ) %>%
  drop_na()

# Clustering function
get_clusters <- function(data, k, vars) {
  selected_data <- data %>% select(all_of(vars)) %>% scale()
  kmeans_model <- kmeans(selected_data, centers = k)
  data$cluster <- as.factor(kmeans_model$cluster)
  return(data)
}

# UI
ui <- fluidPage(
  titlePanel("EV Clustering in Washington"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("k", "Number of Clusters (k):", min = 2, max = 10, value = 3),
      checkboxGroupInput("vars", "Variables to Cluster On:", 
                         choices = c("Number of Vehicles" = "num_vehicles", 
                                     "Average Electric Range" = "avg_range",
                                     "Average MSRP" = "avg_msrp", 
                                     "Battery EV % (vs Plug-in)" = "pct_battery"),
                         selected = c("num_vehicles", "avg_range"))
    ),
    
    mainPanel(
      plotOutput("clusterPlot"),
      tableOutput("clusterSummary"),
      textOutput("n_cities")
    )
  )
)

# Server
server <- function(input, output) {
  # Reactive clustering
  reactive_cluster_data <- reactive({
    req(input$vars)
    get_clusters(clean_ev, input$k, input$vars)
  })
  
  # Plot of city clusters
  output$clusterPlot <- renderPlot({
    cluster_df <- reactive_cluster_data()
    
    # Only plot if at least 2 variables selected
    if (length(input$vars) < 2) {
      plot.new()
      title("Please select at least 2 variables")
      return()
    }
    
    ggplot(cluster_df, aes_string(x = input$vars[1], y = input$vars[2], color = "cluster")) +
      geom_point(size = 3) +
      labs(
        title = "City Clusters Based on EV Statistics",
        x = input$vars[1],
        y = input$vars[2]
      ) +
      theme_minimal()
  })
  
  # Summary table showing city-level data and assigned cluster
  output$clusterSummary <- renderTable({
    reactive_cluster_data() %>%
      select(City, cluster, all_of(input$vars))
  })
  
  # Number of cities clustered
  output$n_cities <- renderText({
    paste("Number of cities clustered:", nrow(reactive_cluster_data()))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
