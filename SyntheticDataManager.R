# SyntheticDataManager --------------------------------------------------------------------
# Manages a list of variables (numeric + categorical),
# plus data generation.

library(R6)
library(MASS)  # for mvrnorm

make_positive_definite <- function(mat, tol = 1e-8) {
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

SyntheticDataManager <- R6::R6Class(
  "SyntheticDataManager",
  public = list(
    variables = list(),
    
    addVariable = function(varObj) {
      stopifnot(inherits(varObj, "SyntheticVariable"))
      self$variables[[varObj$varName]] <- varObj
    },
    
    getAllVariables = function() {
      self$variables
    },
    
    # A naive data generation approach
    generateData = function(nObs = 100, noiseSD = 0, missingPct = 0) {
      numericVars <- Filter(function(x) x$varType == "numeric", self$variables)
      catVars     <- Filter(function(x) x$varType == "categorical", self$variables)
      
      numNames <- sapply(numericVars, function(x) x$varName)
      p <- length(numNames)
      corrMat <- diag(p)
      
      if (p>0) {
        # Build correlation matrix from single correlation link
        for (i in seq_along(numericVars)) {
          nv <- numericVars[[i]]
          if (!is.null(nv$corr)) {
            j <- match(nv$corr$varName, numNames)
            if (!is.na(j)) {
              corrMat[i,j] <- nv$corr$value
              corrMat[j,i] <- nv$corr$value
            }
          }
        }
        corrMat <- make_positive_definite(corrMat)
        
        Z <- mvrnorm(nObs, mu=rep(0,p), Sigma=corrMat)
        Z <- as.data.frame(Z)
        names(Z) <- numNames
        
        # Transform each column
        for (i in seq_along(numericVars)) {
          vdef <- numericVars[[i]]
          colZ <- Z[[vdef$varName]]
          u <- pnorm(colZ)
          
          # minimal logic for demonstration
          if (vdef$distType == "Normal") {
            colFinal <- vdef$mean + vdef$sd * colZ
          } else if (vdef$distType == "Uniform") {
            colFinal <- vdef$minVal + u*(vdef$maxVal - vdef$minVal)
          } else if (vdef$distType == "Lognormal") {
            colFinal <- exp(vdef$meanlog + vdef$sdlog * colZ)
          } else {
            colFinal <- colZ
          }
          
          Z[[vdef$varName]] <- colFinal
        }
        numericDF <- Z
      } else {
        numericDF <- NULL
      }
      
      df <- numericDF
      
      # Categorical
      if (length(catVars)>0) {
        catDF <- list()
        for (cv in catVars) {
          catDF[[ cv$varName ]] <- factor(
            sample(cv$categories, size=nObs, replace=TRUE),
            levels=cv$categories
          )
        }
        catDF <- as.data.frame(catDF)
        
        if (is.null(df)) {
          df <- catDF
        } else {
          df <- cbind(df, catDF)
        }
      }
      
      # noise
      if (!is.null(numericDF) && noiseSD>0) {
        for (cn in names(numericDF)) {
          df[[cn]] <- df[[cn]] + rnorm(nObs, 0, noiseSD)
        }
      }
      
      # missingness
      if (missingPct>0) {
        n <- nrow(df)
        for (cn in names(df)) {
          idx <- sample(seq_len(n), size=floor(n*missingPct))
          df[idx, cn] <- NA
        }
      }
      
      df
    }
  )
)
