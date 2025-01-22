navbarPage(
  "Synthetic Data Generator",
  id = "mainNav",
  theme = bs_theme(),
  
  # 1) Variables Setup
  tabPanel(
    "Variables Setup",
    fluidPage(
      br(),
      strong("Use 'Add Variable' to define numeric or categorical variables. Then 
             press 'Generate Data' to produce the dataset. View results in 'Data Summary' tab."),
      br(),
      actionButton("addVar", "Add Variable", icon = icon("plus"), class = "btn-primary"),
      br(), br(),
      uiOutput("varCards"),  # summaries for each variable
      br(),
      actionButton("generateData", "Generate Data", icon=icon("cogs"), class="btn-success")
    )
  ),
  
  # 2) Data Summary
  tabPanel(
    "Data Summary",
    fluidPage(
      br(),
      DTOutput("dataPreview")
    )
  )
)
