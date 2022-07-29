intro_panel <- tabPanel(
  "Introduction",
  
  titlePanel("Scatterplots of Sales Prices in 3 Neighborhoods"),
  
  br(), br(),
  
  p("Series of Scatterplots for North Ames, Edwards and Berkside Areas."),
  
)

# Page 2 - Visualization -------------------------------------------

sidebar_content <- sidebarPanel(
  radioButtons("dataset", "Dataset:",
               c("Train.csv" = "trainDS")
               
  ),
  
  actionButton("NAmes", "View North Ames Scatterplot"),
  actionButton("Edwards", "View Edwards Scatterplot"),
  actionButton("BrkSde", "View Berkside Scatterplot"),
)

main_content <- mainPanel(
  plotOutput("plot")
)





# User Interface -----------------------------------------------------
ui <- navbarPage(
  "Neighboorhood and SalePrice Analysis of House Data",
  intro_panel,
  sidebar_content,
  main_content
)