library(shiny)
library(bslib)
library(DT)

ui <- navbarPage(
  "Synthetic Data Generator",
  id = "mainNav",
  theme = bs_theme(),
  
  tabPanel(
    "Variables Setup",
    fluidPage(
      br(),
      actionButton("btn_addVar", "Add Variable", icon=icon("plus"), class="btn-primary"),
      br(), br(),
      uiOutput("varCards"),
      br(),
      actionButton("btn_generate", "Generate Data", icon=icon("cogs"), class="btn-success")
    )
  ),
  
  tabPanel(
    "Data Summary",
    fluidPage(
      br(),
      DTOutput("dataPreview")
    )
  )
)
