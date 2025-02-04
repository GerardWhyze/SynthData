library(R6)
library(MASS)  # for mvrnorm

SyntheticDataManager <- R6Class(
  "SyntheticDataManager",
  public = list(
    varList = NULL,
    
    initialize = function() {
      self$varList <- list()
    },
    
    addVariable = function(varDef) {
      # Enforce that only one grouping variable exists.
      if (!is.null(varDef$grouping) &&
          varDef$grouping == TRUE) {
        for (v in self$varList) {
          if (!is.null(v$grouping) && v$grouping == TRUE) {
            stop("A grouping variable already exists.")
          }
        }
      }
      self$varList[[varDef$varName]] <- varDef
    },
    
    generateData = function(nObs,
                            globalNoise = 0,
                            globalMissing = 0) {
      # Check for a grouping variable.
      groupingVar <- NULL
      for (v in self$varList) {
        if (!is.null(v$grouping) && v$grouping == TRUE) {
          groupingVar <- v
          break
        }
      }
      
      if (!is.null(groupingVar)) {
        # Generate the grouping variable.
        groupValues <- sample(groupingVar$categories,
                              size = nObs,
                              replace = TRUE)
        finalDF <- data.frame(group = factor(groupValues, levels = groupingVar$categories))
        
        # Process numeric variables (excluding the grouping variable).
        numericVars <- Filter(function(x)
          x$varType == "numeric", self$varList)
        numericVars <- Filter(function(x) {
          is.null(x$grouping) || x$grouping == FALSE
        }, numericVars)
        
        if (length(numericVars) > 0) {
          numericDF <- data.frame(matrix(ncol = length(numericVars), nrow = nObs))
          colnames(numericDF) <- sapply(numericVars, function(x)
            x$varName)
          for (g in unique(groupValues)) {
            idx <- which(groupValues == g)
            nGroup <- length(idx)
            numNames <- sapply(numericVars, function(x)
              x$varName)
            p <- length(numNames)
            corrMat <- diag(p)
            if (p > 0) {
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
              corrMat <- private$make_positive_definite(corrMat)
              Z <- MASS::mvrnorm(nGroup, mu = rep(0, p), Sigma = corrMat)
              Z <- as.data.frame(Z)
              names(Z) <- numNames
              for (i in seq_along(numericVars)) {
                vdef <- numericVars[[i]]
                colZ <- Z[[vdef$varName]]
                u <- pnorm(colZ)
                # Check for group-specific parameters.
                if (!is.null(vdef$groupParams) &&
                    !is.null(vdef$groupParams[[g]])) {
                  params <- vdef$groupParams[[g]]
                  if (vdef$distType == "Normal") {
                    colFinal <- params$mean + params$sd * colZ
                  } else if (vdef$distType == "Uniform") {
                    colFinal <- params$minVal + u * (params$maxVal - params$minVal)
                  } else if (vdef$distType == "Lognormal") {
                    colFinal <- exp(params$meanlog + params$sdlog * colZ)
                  } else if (vdef$distType == "Exponential") {
                    colFinal <- qexp(u, rate = params$rate)
                  } else if (vdef$distType == "Gamma") {
                    colFinal <- qgamma(u,
                                       shape = params$shape,
                                       rate = params$rate)
                  } else if (vdef$distType == "Beta") {
                    colFinal <- qbeta(u,
                                      shape1 = params$alpha,
                                      shape2 = params$beta)
                  } else if (vdef$distType == "Poisson") {
                    colFinal <- qpois(u, lambda = params$lambda)
                  } else if (vdef$distType == "Binomial") {
                    colFinal <- qbinom(u,
                                       size = params$size,
                                       prob = params$prob)
                  } else if (vdef$distType == "NegBinomial") {
                    colFinal <- qnbinom(u,
                                        size = params$size,
                                        prob = params$prob)
                  } else if (vdef$distType == "ChiSquared") {
                    colFinal <- qchisq(u, df = params$df)
                  } else if (vdef$distType == "StudentT") {
                    colFinal <- qt(u, df = params$df)
                  } else if (vdef$distType == "F") {
                    colFinal <- qf(u, df1 = params$df1, df2 = params$df2)
                  } else {
                    colFinal <- colZ
                  }
                } else {
                  # Use global parameters.
                  if (vdef$distType == "Normal") {
                    colFinal <- vdef$mean + vdef$sd * colZ
                  } else if (vdef$distType == "Uniform") {
                    colFinal <- vdef$minVal + u * (vdef$maxVal - vdef$minVal)
                  } else if (vdef$distType == "Lognormal") {
                    colFinal <- exp(vdef$meanlog + vdef$sdlog * colZ)
                  } else if (vdef$distType == "Exponential") {
                    colFinal <- qexp(u, rate = vdef$rate)
                  } else if (vdef$distType == "Gamma") {
                    colFinal <- qgamma(u,
                                       shape = vdef$shape,
                                       rate = vdef$rate)
                  } else if (vdef$distType == "Beta") {
                    colFinal <- qbeta(u,
                                      shape1 = vdef$alpha,
                                      shape2 = vdef$beta)
                  } else if (vdef$distType == "Poisson") {
                    colFinal <- qpois(u, lambda = vdef$lambda)
                  } else if (vdef$distType == "Binomial") {
                    colFinal <- qbinom(u,
                                       size = vdef$size,
                                       prob = vdef$prob)
                  } else if (vdef$distType == "NegBinomial") {
                    colFinal <- qnbinom(u,
                                        size = vdef$size,
                                        prob = vdef$prob)
                  } else if (vdef$distType == "ChiSquared") {
                    colFinal <- qchisq(u, df = vdef$df)
                  } else if (vdef$distType == "StudentT") {
                    colFinal <- qt(u, df = vdef$df)
                  } else if (vdef$distType == "F") {
                    colFinal <- qf(u, df1 = vdef$df1, df2 = vdef$df2)
                  } else {
                    colFinal <- colZ
                  }
                }
                Z[[vdef$varName]] <- colFinal
              }
              numericDF[idx, ] <- Z
              # Apply global noise to these rows.
              if (globalNoise > 0) {
                for (colName in colnames(numericDF)) {
                  numericDF[idx, colName] <- numericDF[idx, colName] + rnorm(length(idx), mean = 0, sd = globalNoise)
                }
              }
            }
          }
        } else {
          numericDF <- NULL
        }
        
        # Generate categorical variables (excluding those marked as grouping).
        catVars <- Filter(
          function(x)
            x$varType == "categorical" &&
            (is.null(x$grouping) || x$grouping == FALSE),
          self$varList
        )
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
        
        finalDF <- cbind(finalDF, numericDF)
        if (!is.null(catDF)) {
          finalDF <- cbind(finalDF, catDF)
        }
      } else {
        # No grouping variable defined – use original logic.
        numericVars <- Filter(function(x)
          x$varType == "numeric", self$varList)
        catVars     <- Filter(function(x)
          x$varType == "categorical", self$varList)
        
        numNames <- sapply(numericVars, `[[`, "varName")
        p <- length(numNames)
        corrMat <- diag(p)
        
        if (p > 0) {
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
          corrMat <- private$make_positive_definite(corrMat)
          Z <- MASS::mvrnorm(nObs, mu = rep(0, p), Sigma = corrMat)
          Z <- as.data.frame(Z)
          names(Z) <- numNames
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
              colFinal <- colZ
            }
            Z[[vdef$varName]] <- colFinal
          }
          numericDF <- Z
        } else {
          numericDF <- NULL
        }
        
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
        
        df <- numericDF
        if (!is.null(catDF)) {
          if (is.null(df)) {
            df <- catDF
          } else {
            df <- cbind(df, catDF)
          }
        }
        finalDF <- df
      }
      
      # Impose global missingness.
      if (globalMissing > 0) {
        for (cn in names(finalDF)) {
          idx <- sample(seq_len(nObs), size = floor(nObs * globalMissing))
          finalDF[idx, cn] <- NA
        }
      }
      
      return(finalDF)
    }
  ),
  
  private = list(
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
