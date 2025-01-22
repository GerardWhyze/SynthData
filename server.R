#Source the advanced UI code (so we can call getAddVariableModal())

source("advancedUI.R", local=TRUE)

server <- function(input, output, session) {
  
  # Reactive storage for variables and final data
  rv <- reactiveValues(
    varList = list(),
    data = NULL
  )
  
# ADD VARIABLE BUTTON  -----------------------------------------------------------------
  observeEvent(input$addVar, {
    
    # Build a list of existing numeric variable names
    numericVars <- lapply(rv$varList, function(x){
      if (x$varType == "numeric") x$varName else NULL
    })
    numericVars <- unlist(numericVars, use.names=FALSE)
    
    # Show the modal. We pass numericVars so the correlation dropdown can be updated
    showModal( getAddVariableModal(existingNumericVars = numericVars) )
  })
  

# ACCEPT VARIABLE - read modal inputs -----------------------------------------------------------------
  observeEvent(input$acceptVar, {
    varName <- input$varName_modal
    if (!nzchar(varName)) {
      showNotification("Please provide a variable name.", type="error")
      return(NULL)
    }
    varType <- input$varType_modal
    varDesc <- input$varDesc_modal
    
    newVarDef <- list(
      varName = varName,
      varType = varType,
      desc    = varDesc
    )
    
    if (varType == "numeric") {
      distType <- input$distType_modal
      newVarDef$distType <- distType
      
      # Collect distribution parameters
      switch(distType,
             "Normal" = {
               newVarDef$mean <- input$param_normal_mean
               newVarDef$sd   <- input$param_normal_sd
             },
             "Uniform" = {
               newVarDef$minVal <- input$param_unif_min
               newVarDef$maxVal <- input$param_unif_max
             },
             "Lognormal" = {
               newVarDef$meanlog <- input$param_lnorm_meanlog
               newVarDef$sdlog   <- input$param_lnorm_sdlog
             },
             "Exponential" = {
               newVarDef$rate <- input$param_exp_rate
             },
             "Gamma" = {
               newVarDef$shape <- input$param_gamma_shape
               newVarDef$rate  <- input$param_gamma_rate
             },
             "Beta" = {
               newVarDef$alpha <- input$param_beta_alpha
               newVarDef$beta  <- input$param_beta_beta
             },
             "Poisson" = {
               newVarDef$lambda <- input$param_pois_lambda
             },
             "Binomial" = {
               newVarDef$size <- input$param_binom_size
               newVarDef$prob <- input$param_binom_prob
             },
             "NegBinomial" = {
               newVarDef$size <- input$param_negbin_size
               newVarDef$prob <- input$param_negbin_prob
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
      
      # correlation
      if (!is.null(input$corrWith_modal) && input$corrWith_modal != "None") {
        corrVar <- input$corrWith_modal
        corrVal <- input$corrValue_modal
        newVarDef$corr <- list(varName = corrVar, value = corrVal)
      } else {
        newVarDef$corr <- NULL
      }
      
    } else {
      # varType == "categorical"
      catVec <- c(input$cat1, input$cat2, input$cat3, 
                  input$cat4, input$cat5, input$cat6)
      catVec <- catVec[nzchar(catVec)]  # remove blanks
      if (length(catVec)==0) {
        catVec <- "Category1"
      }
      newVarDef$categories <- catVec
      newVarDef$corr <- NULL  # ignoring correlation for categorical
    }
    
    # Store in rv$varList
    rv$varList[[ varName ]] <- newVarDef
    
    removeModal()
    showNotification("Variable added successfully!", type="message")
  })
  

#VARIABLE CARDS -----------------------------------------------------------------
  output$varCards <- renderUI({
    if (length(rv$varList)==0) {
      return(tags$i("No variables defined yet."))
    }
    
    # For each variable, create a summary "card"
    cards <- lapply(rv$varList, function(vdef){
      varName <- vdef$varName
      varType <- vdef$varType
      desc    <- vdef$desc
      body <- NULL
      
      if (varType=="numeric") {
        distType <- vdef$distType
        paramStr <- ""
        if (distType == "Normal") {
          paramStr <- paste0("mean=", vdef$mean, ", sd=", vdef$sd)
        } else if (distType=="Uniform") {
          paramStr <- paste0("[", vdef$minVal, ", ", vdef$maxVal, "]")
        } else if (distType=="Lognormal") {
          paramStr <- paste0("meanlog=", vdef$meanlog, ", sdlog=", vdef$sdlog)
        } 
        # etc. for more distributions
        
        corrStr <- ""
        if (!is.null(vdef$corr)) {
          corrStr <- paste0("Corr with ", vdef$corr$varName, "=", vdef$corr$value)
        }
        
        body <- tagList(
          strong("Type: "), "Numeric (", distType, ")",
          br(),
          strong("Parameters: "), paramStr, br(),
          if (nzchar(corrStr)) tagList(strong("Correlation: "), corrStr, br()),
          if (nzchar(desc)) tagList(strong("Description: "), desc)
        )
        
      } else {
        # categorical
        catStr <- paste(vdef$categories, collapse=", ")
        body <- tagList(
          strong("Type: "), "Categorical",
          br(),
          strong("Categories: "), catStr, br(),
          if (nzchar(desc)) tagList(strong("Description: "), desc)
        )
      }
      
      wellPanel(
        h4(varName),
        body
      )
    })
    
    tagList(cards)
  })
  

#GENERATE DATA -----------------------------------------------------------------
  observeEvent(input$generateData, {
    if (length(rv$varList)==0) {
      showNotification("No variables defined. Please add at least one variable.", type="error")
      return()
    }
    showModal(modalDialog(
      title="Generate Data",
      numericInput("nObs_modal", "Number of Observations (rows):", 100, min=1),
      sliderInput("globalMissing", "Global Missingness (%):", min=0, max=50, value=0),
      sliderInput("globalNoise", "Global Noise (std dev for numeric):", 
                  min=0, max=5, value=0, step=0.1),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmGenerate", "Generate", class="btn-success")
      )
    ))
  })
  
  observeEvent(input$confirmGenerate, {
    removeModal()
    nObs <- input$nObs_modal
    noiseSD <- input$globalNoise
    missingPct <- input$globalMissing / 100
    
    # 1) separate numeric vs. categorical
    numericVars <- Filter(function(x) x$varType=="numeric", rv$varList)
    catVars     <- Filter(function(x) x$varType=="categorical", rv$varList)
    
    numNames <- sapply(numericVars, `[[`, "varName")
    p <- length(numNames)
    corrMat <- diag(p)
    
    # 2) fill correlation from single correlation link
    if (p>0) {
      for (i in seq_along(numericVars)) {
        v_i <- numericVars[[i]]
        if (!is.null(v_i$corr)) {
          j <- match(v_i$corr$varName, numNames)
          if (!is.na(j)) {
            corrVal <- v_i$corr$value
            corrMat[i,j] <- corrVal
            corrMat[j,i] <- corrVal
          }
        }
      }
      corrMat <- make_positive_definite(corrMat)
      
      # Generate correlated standard normals
      Z <- MASS::mvrnorm(nObs, mu=rep(0,p), Sigma=corrMat)
      Z <- as.data.frame(Z)
      names(Z) <- numNames
      
      # transform each dimension
      for (i in seq_along(numericVars)) {
        vdef <- numericVars[[i]]
        colZ <- Z[[ vdef$varName ]]
        u <- pnorm(colZ)
        
        distType <- vdef$distType
        if (distType=="Normal") {
          colFinal <- vdef$mean + vdef$sd*colZ
        } else if (distType=="Uniform") {
          colFinal <- vdef$minVal + u*(vdef$maxVal - vdef$minVal)
        } else if (distType=="Lognormal") {
          colFinal <- exp(vdef$meanlog + vdef$sdlog*colZ)
        } else {
          # fallback
          colFinal <- colZ
        }
        Z[[ vdef$varName ]] <- colFinal
      }
      numericDF <- Z
    } else {
      numericDF <- NULL
    }
    
    # 3) build final df
    df <- numericDF
    
    # 4) categorical
    if (length(catVars)>0) {
      catDF <- list()
      for (cv in catVars) {
        catName <- cv$varName
        catLevels <- cv$categories
        catDF[[catName]] <- factor(sample(catLevels, size=nObs, replace=TRUE), 
                                   levels=catLevels)
      }
      catDF <- as.data.frame(catDF)
      if (is.null(df)) {
        df <- catDF
      } else {
        df <- cbind(df, catDF)
      }
    }
    
    # 5) global noise for numeric
    if (!is.null(numericDF) && noiseSD>0) {
      for (colName in names(numericDF)) {
        df[[colName]] <- df[[colName]] + rnorm(nObs, mean=0, sd=noiseSD)
      }
    }
    
    # 6) missingness
    if (missingPct>0) {
      for (cn in names(df)) {
        idx <- sample(seq_len(nObs), size=floor(nObs*missingPct))
        df[idx, cn] <- NA
      }
    }
    
    rv$data <- df
    showNotification("Data generation complete.", type="message")
    updateNavbarPage(session, "mainNav", "Data Summary")
  })
  
# Data Preview -----------------------------------------------------------------
  output$dataPreview <- renderDT({
    req(rv$data)
    datatable(rv$data, options=list(pageLength=10, scrollX=TRUE))
  })
}