SyntheticDataManager <- R6Class(
  "SyntheticDataManager",
  public = list(
    varList = NULL,
    
    initialize = function() {
      self$varList <- list()
    },
    
    addVariable = function(varDef) {
      self$varList[[varDef$varName]] <- varDef
    },
    
    generateData = function(nObs,
                            globalNoise = 0,
                            globalMissing = 0) {
      if (length(self$varList) == 0) {
        stop("No variables defined.")
      }
      
      # Separate numeric and categorical variable definitions
      numericVars <- Filter(function(x)
        x$varType == "numeric", self$varList)
      catVars <- Filter(function(x)
        x$varType == "categorical", self$varList)
      
      # Generate numeric data (if any)
      if (length(numericVars) > 0) {
        numNames <- sapply(numericVars, `[[`, "varName")
        p <- length(numNames)
        corrMat <- diag(p)
        if (p > 0) {
          # Fill correlation matrix if any variable has a correlation link defined
          for (i in seq_along(numericVars)) {
            v_i <- numericVars[[i]]
            if (!is.null(v_i$corr)) {
              j <- match(v_i$corr$varName, numNames)
              if (!is.na(j)) {
                corrVal <- v_i$corr$value
                corrMat[i, j] <- corrVal
                corrMat[j, i] <- corrVal
              }
            }
          }
          # Ensure the correlation matrix is positive definite.
          corrMat <- private$make_positive_definite(corrMat)
          
          # Generate correlated standard normal variates.
          Z <- MASS::mvrnorm(nObs, mu = rep(0, p), Sigma = corrMat)
          Z <- as.data.frame(Z)
          names(Z) <- numNames
          
          # Transform each numeric variable based on its distribution.
          for (i in seq_along(numericVars)) {
            vdef <- numericVars[[i]]
            colZ <- Z[[vdef$varName]]
            u <- pnorm(colZ)
            distType <- vdef$distType
            if (distType == "Normal") {
              colFinal <- vdef$mean + vdef$sd * colZ
            } else if (distType == "Uniform") {
              colFinal <- vdef$minVal + u * (vdef$maxVal - vdef$minVal)
            } else if (distType == "Lognormal") {
              colFinal <- exp(vdef$meanlog + vdef$sdlog * colZ)
            } else if (distType == "Exponential") {
              colFinal <- qexp(u, rate = vdef$rate)
            } else if (distType == "Gamma") {
              colFinal <- qgamma(u, shape = vdef$shape, rate = vdef$rate)
            } else if (distType == "Beta") {
              colFinal <- qbeta(u, shape1 = vdef$alpha, shape2 = vdef$beta)
            } else if (distType == "Poisson") {
              colFinal <- qpois(u, lambda = vdef$lambda)
            } else if (distType == "Binomial") {
              colFinal <- qbinom(u, size = vdef$size, prob = vdef$prob)
            } else if (distType == "NegBinomial") {
              colFinal <- qnbinom(u, size = vdef$size, prob = vdef$prob)
            } else if (distType == "ChiSquared") {
              colFinal <- qchisq(u, df = vdef$df)
            } else if (distType == "StudentT") {
              colFinal <- qt(u, df = vdef$df)
            } else if (distType == "F") {
              colFinal <- qf(u, df1 = vdef$df1, df2 = vdef$df2)
            } else {
              colFinal <- colZ  # Fallback to standard normal.
            }
            Z[[vdef$varName]] <- colFinal
          }
          numericDF <- Z
        } else {
          numericDF <- NULL
        }
      } else {
        numericDF <- NULL
      }
      
      # Generate categorical data (if any)
      if (length(catVars) > 0) {
        catDF <- list()
        for (cv in catVars) {
          catName <- cv$varName
          catLevels <- cv$categories
          catDF[[catName]] <- factor(sample(catLevels, size = nObs, replace = TRUE), levels = catLevels)
        }
        catDF <- as.data.frame(catDF)
      } else {
        catDF <- NULL
      }
      
      # Combine numeric and categorical data
      df <- numericDF
      if (!is.null(catDF)) {
        if (is.null(df)) {
          df <- catDF
        } else {
          df <- cbind(df, catDF)
        }
      }
      
      # Add global noise (to numeric columns)
      if (!is.null(numericDF) &&
          globalNoise > 0) {
        for (colName in names(numericDF)) {
          df[[colName]] <- df[[colName]] + rnorm(nObs, mean = 0, sd = globalNoise)
        }
      }
      
      # Impose global missingness.
      if (globalMissing > 0) {
        for (cn in names(df)) {
          idx <- sample(seq_len(nObs), size = floor(nObs * globalMissing))
          df[idx, cn] <- NA
        }
      }
      
      return(df)
    }
  ),
  
  private = list(
    # Helper: Adjust a matrix to be positive definite.
    make_positive_definite = function(mat, tol = 1e-8) {
      if (!isSymmetric(mat)) {
        mat <- (mat + t(mat)) / 2
      }
      eig <- eigen(mat)
      vals <- eig$values
      vecs <- eig$vectors
      vals[vals < tol] <- tol
      diag_mat <- diag(vals)
      adj_mat <- vecs %*% diag_mat %*% t(vecs)
      adj_mat <- (adj_mat + t(adj_mat)) / 2
      adj_mat
    }
  )
)
