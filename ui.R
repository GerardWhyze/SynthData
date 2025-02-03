ui <- fluidPage(
  theme = bs_theme(),
  
  # App title (replaces navbar title)
  titlePanel("Synthetic Data Generator"),
  
  # Use tabsetPanel for horizontal tabs, preserving the same 'mainNav' ID
  tabsetPanel(
    id = "mainNav",
    
    # 1) Variables Setup
    tabPanel(
      "Variables Setup",
      fluidPage(
        br(),
        strong("Use 'Add Variable' to define numeric or categorical variables. 
               Then press 'Generate Data' to produce the dataset. 
               View results in 'Data Summary' tab."),
        br(),
        actionButton("addVar", "Add Variable", icon = icon("plus"), class = "btn-primary"),
        br(), br(),
        uiOutput("varCards"),  # Summaries for each variable
        br(),
        actionButton("generateData", "Generate Data", icon = icon("cogs"), class = "btn-success")
      )
    ),
    
    # 2) Data Summary
    tabPanel(
      "Data Summary",
      fluidPage(
        br(),
        downloadButton("downloadData", "Export Data to CSV"),
        br(), br(),
        DTOutput("dataPreview")
      )
    ),
    
    # 3) Visualisations (New Tab)
    tabPanel(
      "Visualisations",
      fluidPage(
        br(),
        # Add a selector for the variable and a plot for the distribution/bar chart.
        uiOutput("varSelectUI"),
        plotOutput("distPlot")
      )
    )
  )
)
