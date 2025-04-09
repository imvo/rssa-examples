# Load necessary libraries
library(shiny)
library(Rssa)

# Define UI for the dataset selection module
datasetSelectUI <- function(id) {
  ns <- NS(id)  # Namespace for module
  tagList(
    selectInput(ns("dataset"), "Choose a Dataset:", choices = data(package = "Rssa")$results[, "Item"])
  )
}

# Define server logic for the dataset selection module
datasetSelectServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Return the selected dataset
    return(reactive({
      input$dataset
    }))
  })
}