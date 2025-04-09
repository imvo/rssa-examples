library(shiny)
library(Rssa)
source('utils.R', local = TRUE)
source('modules/datasetSelect.R', local = TRUE)


# Define UI
ui <- fluidPage(
  titlePanel("W-correlation matrix"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text_input", "Enter values (separated by space or semicolon):", "", rows = 5),
      datasetSelectUI("dataset_select")  # Call the dataset selection UI module
    ),
    mainPanel(
      verbatimTextOutput("array_output"),
      tableOutput("dataset_preview"),
      plotOutput("plotWcor",
                 click = "plot_click",
                 dblclick = "plot_dblclick",
                 hover = "plot_hover",
                 brush = "plot_brush"
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  data("AustralianWine", package = "Rssa")
  data("USUnemployment", package = "Rssa")
  output$array_output <- renderText({
    data_array <- parseText(input$text_input)
    paste("Array:", toString(data_array))
  })

  # Call the dataset selection server module
  selected_dataset <- datasetSelectServer("dataset_select")
  #data(list=selected_dataset, package = "Rssa")

  # Render the dataset preview
  output$dataset_preview <- renderTable({
    dataset_name <- selected_dataset()
    if (!is.null(dataset_name)) {
      #data(dataset_name, package = "Rssa")
      data <- get(dataset_name, envir = asNamespace("Rssa"))
      head(data)  # Display first few rows of the dataset
    }
  })
  
  output$plotWcor <- renderPlot({
        dataset_name <- selected_dataset()
        if (dataset_name == "AustralianWine") {
          wine <- window(AustralianWine, end = time(AustralianWine)[174])
          fort <- wine[, "Fortified"]
          ss <- ssa(fort, L = 84, kind = "1d-ssa")
        } else if (dataset_name == "USUnemployment") {
          ser <- USUnemployment[, "MALE"]
          Time <- time(ser)
          L = 204
          ss <- ssa(ser, L = L, svd.method = "eigen")
        } else {
          print("bar")
        }
        plot(wcor(ss, groups = 1:30),
             scales = list(at = c(10, 20, 30)))
      })
}

# Run the application
shinyApp(ui = ui, server = server)





# # Load R packages
# library(shiny)
# library(shinythemes)
# 
# 
# # Define UI
# ui <- fluidPage(theme = shinytheme("cosmo"),
#                 navbarPage(
#                   theme = "cerulean",  # <--- To use a theme, uncomment this
#                   "SSA with R interactive examples",
#                   tabPanel("FORT: Reconstruction",
#                            sidebarPanel(
#                              tags$h3("Input parameters:"),
#                              textInput("txt1", "Seasonality a:", 2),
#                              textInput("txt2", "Seasonality b:", 11),
#                              actionButton("button", "Reset")
# 
#                            ), # sidebarPanel
#                            mainPanel(
#                              h1("Output"),
# 
#                              #h4("Output 1"),
#                              #checkboxGroupInput("groups", "Select Groups:", choices = as.character(1:20)),
#                              verbatimTextOutput("Description or hint"),
#                              plotOutput("plot1",
#                                         click = "plot_click",
#                                         dblclick = "plot_dblclick",
#                                         hover = "plot_hover",
#                                         brush = "plot_brush"
#                              )
# 
# 
#                            ) # mainPanel
# 
#                   ), # Navbar 1, tabPanel
#                   tabPanel("Navbar 2", "This panel is intentionally left blank"),
#                   tabPanel("About", "This panel is intentionally left blank")
# 
#                 ) # navbarPage
# ) # fluidPage
# 
# 
# # Define server function
# server <- function(input, output, session) {
#   library("Rssa")
# 
#   observeEvent(input$button, {
#     updateNumericInput(session, "txt1", value = 2)
#     updateNumericInput(session, "txt2", value = 11)
#   })
# 
#   output$plot1 <- renderPlot({
#     data("AustralianWine", package = "Rssa")
#     wine <- window(AustralianWine, end = time(AustralianWine)[174])
#     fort <- wine[, "Fortified"]
#     s.fort <- ssa(fort, L = 84, kind = "1d-ssa")
#     r.fort <- reconstruct(s.fort,
#                           groups = list(Trend = 1,
#                                         Seasonality = input$txt1:input$txt2))
# 
#     plot(r.fort, add.residuals = TRUE, add.original = TRUE,
#          plot.method = "xyplot",
#          superpose = TRUE, auto.key = list(columns = 2))
#   })
# 
#   output$txtout <- renderText({
#     paste( input$txt1, input$txt2, sep = " " )
#   })
# } # server
# 
# 
# # Create Shiny object
# shinyApp(ui = ui, server = server)