library(shiny)
library(ggplot2)
library(DT)
library(MASS)  # for mvrnorm

# Source the advanced UI code (which now uses an R6 class for the modal)
source("advancedUI.R", local = TRUE)
# Source our refactored R6 class for data generation with grouping support.
source("SyntheticDataManager.R", local = TRUE)

server <- function(input, output, session) {

  dataManager <- SyntheticDataManager$new()
  
  rv <- reactiveValues(data = NULL, varListReactive = list())
  

#   Group-Specific Inputs -------------------------------------------------

  # Render dynamic UI for group-specific parameters
  output$groupParamInputs <- renderUI({
    # Look for the grouping variable in the stored variable definitions.
    groupingVar <- NULL
    for (v in rv$varListReactive) {
      if (!is.null(v$grouping) && v$grouping == TRUE) {
        groupingVar <- v
        break
      }
    }
    if (is.null(groupingVar)) {
      return(tags$em("No grouping variable defined."))
    }
    # Use the grouping variable's categories (assumed to be stored in groupingVar$categories)
    groupNames <- groupingVar$categories
    if (length(groupNames) == 0) {
      return(tags$em("No group names provided in the grouping variable."))
    }
    distType <- input$distType_modal  # Distribution type from the numeric modal
    uiList <- lapply(groupNames, function(g) {
      if (distType == "Normal") {
        tagList(
          h5(paste("Parameters for group", g)),
          numericInput(paste0("param_normal_mean_", g), "Mean:", value = 0),
          numericInput(paste0("param_normal_sd_", g), "SD:", value = 1)
        )
      } else if (distType == "Uniform") {
        tagList(
          h5(paste("Parameters for group", g)),
          numericInput(paste0("param_unif_min_", g), "Min:", value = 0),
          numericInput(paste0("param_unif_max_", g), "Max:", value = 1)
        )
      } else if (distType == "Lognormal") {
        tagList(
          h5(paste("Parameters for group", g)),
          numericInput(paste0("param_lnorm_meanlog_", g), "Meanlog:", value = 0),
          numericInput(paste0("param_lnorm_sdlog_", g), "SDlog:", value = 1)
        )
      } else {
        tagList(h5(
          paste("No group-specific inputs available for", distType)
        ))
      }
    })
    do.call(tagList, uiList)
  })
  
  # Render dynamic UI for group-specific correlations
  output$groupCorrInputs <- renderUI({
    # Look for the grouping variable.
    groupingVar <- NULL
    for (v in rv$varListReactive) {
      if (!is.null(v$grouping) && v$grouping == TRUE) {
        groupingVar <- v
        break
      }
    }
    if (is.null(groupingVar)) {
      return(tags$em("No grouping variable defined."))
    }
    groupNames <- groupingVar$categories
    if (length(groupNames) == 0) {
      return(tags$em("No group names provided in the grouping variable."))
    }
    uiList <- lapply(groupNames, function(g) {
      tagList(h5(paste("Correlation for group", g)),
              numericInput(
                paste0("corrValue_", g),
                "Correlation:",
                value = 0,
                min = -1,
                max = 1
              ))
    })
    do.call(tagList, uiList)
  })
  

# ADD VARIABLE ------------------------------------------------------------

 
  
  observeEvent(input$addVar, {
    # Build a list of existing numeric variable names from the manager.
    numericVars <- names(dataManager$varList)
    showModal(getAddVariableModal(existingNumericVars = numericVars))
  })
  
  observeEvent(input$acceptVar, {
    varName <- input$varName_modal
    if (!nzchar(varName)) {
      showNotification("Please provide a variable name.", type = "error")
      return(NULL)
    }
    varType <- input$varType_modal
    varDesc <- input$varDesc_modal
    if (is.null(varDesc))
      varDesc <- ""
    
    newVarDef <- list(varName = varName,
                      varType = varType,
                      desc = varDesc)
    
    if (varType == "numeric") {
      distType <- input$distType_modal
      newVarDef$distType <- distType
      
      # Choose parameter mode
      if (input$paramType_modal == "Variable Parameters") {
        switch(
          distType,
          "Normal" = {
            newVarDef$mean <- input$param_normal_mean
            newVarDef$sd <- input$param_normal_sd
          },
          "Uniform" = {
            newVarDef$minVal <- input$param_unif_min
            newVarDef$maxVal <- input$param_unif_max
          },
          "Lognormal" = {
            newVarDef$meanlog <- input$param_lnorm_meanlog
            newVarDef$sdlog <- input$param_lnorm_sdlog
          },
          "Exponential" = {
            newVarDef$rate <- input$param_exp_rate
          },
          "Gamma" = {
            newVarDef$shape <- input$param_gamma_shape
            newVarDef$rate <- input$param_gamma_rate
          },
          "Beta" = {
            newVarDef$alpha <- input$param_beta_alpha
            newVarDef$beta <- input$param_beta_beta
          },
          "Poisson" = {
            newVarDef$lambda <- input$param_pois_lambda
          },
          "Binomial" = {
            newVarDef$size <- input$param_binom_size
            newVarDef$prob <- input$param_binom_prob
          },
          "NegBinomial" = {
            newVarDef$size <- input$param_nbinom_size
            newVarDef$prob <- input$param_nbinom_prob
          },
          "ChiSquared" = {
            newVarDef$df <- input$param_chisq_df
          },
          "StudentT" = {
            newVarDef$df <- input$param_t_df
          },
          "F" = {
            newVarDef$df1 <- input$param_f_df1
            newVarDef$df2 <- input$param_f_df2
          }
        )
      } else if (input$paramType_modal == "Group Specific Parameters") {
        # Read group-specific parameters from dynamic UI.
        # Look for grouping variable from the stored definitions.
        groupingVar <- NULL
        for (v in rv$varListReactive) {
          if (!is.null(v$grouping) && v$grouping == TRUE) {
            groupingVar <- v
            break
          }
        }
        if (!is.null(groupingVar)) {
          groupNames <- groupingVar$categories
          newVarDef$groupParams <- list()
          for (g in groupNames) {
            if (distType == "Normal") {
              newVarDef$groupParams[[g]] <- list(mean = input[[paste0("param_normal_mean_", g)]], sd = input[[paste0("param_normal_sd_", g)]])
            } else if (distType == "Uniform") {
              newVarDef$groupParams[[g]] <- list(minVal = input[[paste0("param_unif_min_", g)]], maxVal = input[[paste0("param_unif_max_", g)]])
            } else if (distType == "Lognormal") {
              newVarDef$groupParams[[g]] <- list(meanlog = input[[paste0("param_lnorm_meanlog_", g)]], sdlog = input[[paste0("param_lnorm_sdlog_", g)]])
            }
            # we will need logic here for the other variable distributions!
          }
        }
      }
      
      # Process correlations.
      if (!is.null(input$groupCorr_checkbox) &&
          input$groupCorr_checkbox == TRUE) {
        newVarDef$groupCorr <- list(corrVar = input$groupCorrWith_modal,
                                    correlations = list())
        groupingVar <- NULL
        for (v in rv$varListReactive) {
          if (!is.null(v$grouping) && v$grouping == TRUE) {
            groupingVar <- v
            break
          }
        }
        if (!is.null(groupingVar)) {
          groupNames <- groupingVar$categories
          for (g in groupNames) {
            newVarDef$groupCorr$correlations[[g]] <- input[[paste0("corrValue_", g)]]
          }
        }
      } else if (!is.null(input$corrWith_modal) &&
                 input$corrWith_modal != "None") {
        newVarDef$corr <- list(varName = input$corrWith_modal,
                               value = input$corrValue_modal)
      } else {
        newVarDef$corr <- NULL
      }
      
    } else {
      # For categorical variables.
      catVec <- c(input$cat1,
                  input$cat2,
                  input$cat3,
                  input$cat4,
                  input$cat5,
                  input$cat6)
      catVec <- catVec[catVec != ""]
      if (length(catVec) == 0)
        catVec <- "Category1"
      newVarDef$categories <- catVec
      newVarDef$grouping <- ifelse(is.null(input$grouping_modal),
                                   FALSE,
                                   input$grouping_modal)
      newVarDef$corr <- NULL
    }
    
    # Add the variable definition to our SyntheticDataManager.
    tryCatch({
      dataManager$addVariable(newVarDef)
      rv$varListReactive <- dataManager$varList
      removeModal()
      showNotification("Variable added successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  

# VARIABLE CARDS  ---------------------------------------------------------

  
  output$varCards <- renderUI({
    if (length(rv$varListReactive) == 0) {
      return(tags$i("No variables defined yet."))
    }
    cards <- lapply(rv$varListReactive, function(vdef) {
      varName <- vdef$varName
      varType <- vdef$varType
      desc <- ifelse(is.null(vdef$desc), "", vdef$desc)
      body <- NULL
      if (varType == "numeric") {
        distType <- vdef$distType
        paramStr <- ""
        if (!is.null(vdef$groupParams)) {
          paramStr <- "Group-specific parameters defined"
        } else {
          if (distType == "Normal") {
            paramStr <- paste0("mean=", vdef$mean, ", sd=", vdef$sd)
          } else if (distType == "Uniform") {
            paramStr <- paste0("[", vdef$minVal, ", ", vdef$maxVal, "]")
          } else if (distType == "Lognormal") {
            paramStr <- paste0("meanlog=", vdef$meanlog, ", sdlog=", vdef$sdlog)
          }
        }
        corrStr <- ""
        if (!is.null(vdef$corr)) {
          corrStr <- paste0("Corr with ", vdef$corr$varName, "=", vdef$corr$value)
        }
        if (!is.null(vdef$groupCorr)) {
          corrStr <- "Group-specific correlations defined"
        }
        body <- tagList(
          strong("Type: "),
          "Numeric (",
          distType,
          ")",
          br(),
          strong("Parameters: "),
          paramStr,
          br(),
          if (!is.null(corrStr) &&
              nzchar(corrStr))
            tagList(strong("Correlation: "), corrStr, br()),
          if (nzchar(desc))
            tagList(strong("Description: "), desc)
        )
      } else {
        catStr <- paste(vdef$categories, collapse = ", ")
        body <- tagList(
          strong("Type: "),
          "Categorical",
          br(),
          strong("Categories: "),
          catStr,
          br(),
          if (nzchar(desc))
            tagList(strong("Description: "), desc),
          if (!is.null(vdef$grouping) && vdef$grouping == TRUE) {
            tags$em(" [Grouping Variable]")
          }
        )
      }
      wellPanel(h4(varName), body)
    })
    tagList(cards)
  })
  

# GENERATE DATA -----------------------------------------------------------


  observeEvent(input$generateData, {
    if (length(rv$varListReactive) == 0) {
      showNotification("No variables defined. Please add at least one variable.",
                       type = "error")
      return()
    }
    showModal(
      modalDialog(
        title = "Generate Data",
        numericInput("nObs_modal", "Number of Observations (rows):", 100, min = 1),
        sliderInput(
          "globalMissing",
          "Global Missingness (%):",
          min = 0,
          max = 50,
          value = 0
        ),
        sliderInput(
          "globalNoise",
          "Global Noise (std dev for numeric):",
          min = 0,
          max = 5,
          value = 0,
          step = 0.1
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirmGenerate", "Generate", class = "btn-success")
        )
      )
    )
  })
  
  observeEvent(input$confirmGenerate, {
    removeModal()
    nObs <- input$nObs_modal
    noiseSD <- input$globalNoise
    missingPct <- input$globalMissing / 100
    tryCatch({
      generatedData <- dataManager$generateData(nObs, noiseSD, missingPct)
      rv$data <- generatedData
      showNotification("Data generation complete.", type = "message")
      updateTabsetPanel(session, "mainNav", selected = "Data Summary")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  

# DATA PREVIEW  -----------------------------------------------------------


  
  output$dataPreview <- renderDT({
    req(rv$data)
    datatable(rv$data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("synthetic_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$data, file, row.names = FALSE)
    }
  )
  

# VISUALISATION -----------------------------------------------------------


  
  output$varSelectUI <- renderUI({
    req(rv$data)
    selectInput("selectedVar", "Select Variable", choices = names(rv$data))
  })
  
  output$distPlot <- renderPlot({
    req(input$selectedVar, rv$data)
    varData <- rv$data[[input$selectedVar]]
    varData <- na.omit(varData)
    
    if (input$selectedVar %in% names(rv$varListReactive)) {
      vdef <- rv$varListReactive[[input$selectedVar]]
      if (vdef$varType == "numeric") {
        distType <- vdef$distType
        dist_mapping <- list(
          "Normal" = "norm",
          "Uniform" = "unif",
          "Lognormal" = "lnorm",
          "Exponential" = "exp",
          "Gamma" = "gamma",
          "Beta" = "beta",
          "Poisson" = "pois",
          "Binomial" = "binom",
          "NegBinomial" = "nbinom",
          "ChiSquared" = "chisq",
          "StudentT" = "t",
          "F" = "f"
        )
        chosen_dist <- dist_mapping[[distType]]
        if (is.null(chosen_dist)) {
          chosen_dist <- "norm"
        }
        fit <- tryCatch({
          fitdist(varData, chosen_dist)
        }, error = function(e)
          NULL)
        density_fun <- tryCatch({
          get(paste0("d", chosen_dist))
        }, error = function(e)
          NULL)
        if (is.null(density_fun)) {
          density_fun <- dnorm
        }
        if (chosen_dist == "pois") {
          density_fun <- pois_density  
        } else {
          density_fun <- get(paste0("d", chosen_dist))
        }
        if (!is.null(fit)) {
          ggplot(data = data.frame(x = varData), aes(x = x)) +
            geom_histogram(
              aes(y = ..density..),
              bins = 30,
              fill = "blue",
              alpha = 0.6
            ) +
            stat_function(
              fun = density_fun,
              args = as.list(fit$estimate),
              color = "red",
              size = 1
            ) +
            labs(
              title = paste(
                "Fitted",
                distType,
                "Distribution for",
                input$selectedVar
              ),
              x = input$selectedVar,
              y = "Density"
            ) +
            theme_minimal()
        } else {
          ggplot(data = data.frame(x = varData), aes(x = x)) +
            geom_histogram(
              aes(y = ..density..),
              bins = 30,
              fill = "blue",
              alpha = 0.6
            ) +
            labs(
              title = paste("Histogram for", input$selectedVar),
              x = input$selectedVar,
              y = "Density"
            ) +
            theme_minimal()
        }
      } else {
        ggplot(data = as.data.frame(table(varData)), aes(x = varData, y = Freq)) +
          geom_bar(stat = "identity",
                   fill = "blue",
                   alpha = 0.6) +
          labs(
            title = paste("Bar Chart for", input$selectedVar),
            x = input$selectedVar,
            y = "Count"
          ) +
          theme_minimal()
      }
    } else {
      ggplot(data = data.frame(x = varData), aes(x = x)) +
        geom_histogram(
          aes(y = ..density..),
          bins = 30,
          fill = "blue",
          alpha = 0.6
        ) +
        labs(
          title = paste("Histogram for", input$selectedVar),
          x = input$selectedVar,
          y = "Density"
        ) +
        theme_minimal()
    }
  })

}
