## --------------------------------------- SHINY APP -----------------------------------------

library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel(title = "SOM Training", windowTitle = "SOM Training"),
      br(),
      # fileInput(inputId = "input_data", label = "Input a patient file:", accept = ".csv", width = '300'),
      numericInput(inputId = "size_of_data", 
                   label = "size of data to be used", value = 1000),
      numericInput(inputId = "Epoch", 
                   label = "Epoch value", value = 1),
      numericInput(inputId = "K_folds", 
                   label = "K folds", value = 2),
      sliderInput(inputId = "grid_x",
                  label = "grid x value", 
                  value = 5, min = 1, max = 10),
      sliderInput(inputId = "grid_y",
                  label = "grid y value", 
                  value = 5, min = 1, max = 10),
      checkboxGroupInput(inputId = "input", label = "Inputs", selected = "demographics", choices = c("demographics", 
                                                                                                     "vitals", 
                                                                                                     "totals",
                                                                                                     "social",
                                                                                                     "context",
                                                                                                     "history",
                                                                                                     "meds",
                                                                                                     "cc",
                                                                                                     "totals"))
    ),
    mainPanel(
      br(),
      plotOutput("som_model"),
      br(),
      verbatimTextOutput("matrix")
    )
  )
)


server <- function(input, output) {
  output$matrix <- renderPrint(
    training_som(data, 
                 size_of_data = input$size_of_data, 
                 input = input$input, 
                 Epoch = input$Epoch, 
                 K_folds = input$K_folds, 
                 grid_x = input$grid_x, 
                 grid_y = input$grid_y))
  
  output$som_model <- renderPlot(
    training_som(data, 
                 size_of_data = input$size_of_data, 
                 input = input$input, 
                 Epoch = input$Epoch, 
                 K_folds = input$K_folds, 
                 grid_x = input$grid_x, 
                 grid_y = input$grid_y)
    
    
  )
}

shinyApp(ui = ui, server = server)
