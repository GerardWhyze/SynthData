ui <- fluidPage(
  theme = bs_theme(),
  
  # App title
  titlePanel("Synthetic Data Generator"),
  
  # Main tabset panel
  tabsetPanel(
    id = "mainNav",
    
    # 1) Variables Setup Tab
    tabPanel(
      "Variables Setup",
      fluidPage(
        br(),
        strong(
          "Use 'Add Variable' to define numeric or categorical variables. Then press 'Generate Data' to produce the dataset. View results in 'Data Summary' tab."
        ),
        br(),
        actionButton(
          "addVar",
          "Add Variable",
          icon = icon("plus"),
          class = "btn-primary"
        ),
        br(),
        br(),
        uiOutput("varCards"),
        br(),
        actionButton(
          "generateData",
          "Generate Data",
          icon = icon("cogs"),
          class = "btn-success"
        )
      )
    ),
    
    # 2) Data Summary Tab
    tabPanel(
      "Data Summary",
      fluidPage(
        br(),
        downloadButton("downloadData", "Export Data to CSV"),
        br(),
        br(),
        DTOutput("dataPreview")
      )
    ),
    
    # 3) Visualisations Tab
    tabPanel("Visualisations", fluidPage(
      br(), uiOutput("varSelectUI"), plotOutput("distPlot")
    ))
  )
)
