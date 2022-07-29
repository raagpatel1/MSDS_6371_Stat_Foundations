#
library("ggplot2")
library("dplyr")

server <- function(input, output) {
  # assign values to `output` here
  v <- reactiveValues(data = NULL)
  
  td <- reactive({
    if (input$dataset == "trainDS")
      read.csv("train.csv", header = TRUE , ",")
    else
      stop("Unexpected dataset")
  })
  
  observeEvent(input$NAmes, {
    v$data <- td() %>% filter(grepl("NAmes", Neighborhood)) %>% 
      ggplot(aes(x = GrLivArea, y = SalePrice)) + geom_point() + 
      labs(title = "North Ames; Sales Price VS Square Footage", x = "Square Footage", y = "Sales Price")
  })
  observeEvent(input$Edwards, {
    v$data <- td() %>% filter(grepl("Edwards", Neighborhood)) %>% 
      ggplot(aes(x = GrLivArea, y = SalePrice)) + geom_point() + 
      labs(title = "Edwards; Sales Price VS Square Footage", x = "Square Footage", y = "Sales Price")
  })
  observeEvent(input$BrkSde, {
    v$data <- td() %>% filter(grepl("BrkSide", Neighborhood)) %>% 
      ggplot(aes(x = GrLivArea, y = SalePrice)) + geom_point() + 
      labs(title = "Berkside; Sales Price VS Square Footage", x = "Square Footage", y = "Sales Price")
  })
  
  
  output$plot <- renderPlot({
    if (is.null(v$data)) return()
    v$data
  })
  
}
