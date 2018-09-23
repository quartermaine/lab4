#' A class for linear regression with many methods for corresponding values calculations
#'
#' @field y vector The orginial labels of observations in data
#' @field X matrix The features of all observations in data
#' @field beta vector The coefficients in the sample
#' @field y_hat matrix The estimated labels
#' @field e_hat matrix The standard errors between orginial and estimated labels
#' @field df numeric The degree of freedom of data
#' @field var_resid numeric The residual variance
#' @field var_hat matrix The variance of the regression coefficients
#' @field t_val vector The t-values for each coefficient
#' @field p_val vector The p-values for each coefficient
#' @field formula character The formula used in the model
#' @field data data.frame The sample data
#'
#' @export linreg
#' @exportClass linreg

linreg <- setRefClass("linreg",
  fields = list(
    y = "numeric",
    X = "matrix",
    y_hat = "matrix",
    beta = "vector",
    e_hat = "matrix",
    df = "numeric",
    var_resid = "numeric",
    var_hat = "matrix",
    t_val = "vector",
    p_val = "vector",
    formula = "character",
    data = "data.frame"
  ),
  methods = list(

    # The initialize methods
    initialize = function(formula, data){
      f <- as.formula(formula)
      stopifnot(is.character(formula),is.data.frame(data) )
      cat("User::initialize")
      formula <<- formula
      data <<- data
      X <<- model.matrix(f,data)
      y <<- data[,all.vars(f)[1]]
      beta <<- as.vector(solve(t(X)%*%X)%*%t(X)%*%y)
      y_hat <<- as.matrix(X%*%beta)
      e_hat <<- y-y_hat
      df <<- length(y) - ncol(X)
      var_resid <<- as.numeric((t(e_hat)%*%e_hat)/df)
      var_hat <<- var_resid * solve(t(X) %*% X)
      t_val <<- beta/(diag(chol(var_hat)))
      p_val <<- pt(beta, df)
      names(p_val) <<- colnames(X)
      names(beta) <<- colnames(X)
    },
    coef = function(){
      return(beta)
    },
    pred = function(){
      return(y_hat)
    },
    resid = function(){
      return(e_hat)
    },
    freedomdegree = function(){
      return(df)
    },
    residualvariance = function(){
      return(var_resid)
    },
    coeffvariance = function(){
      return(var_hat)
    },
    t_values = function(){
      return(t_val)
    },
    p_values = function(){
      return(p_Val)
    },
    print = function(){
      cat("Call:\n")
      cat("formula =",formula, "\n")
      cat("\n")
      cat("Coefficitents:\n")
      beta
    },
    summary = function(){
      cat("Coefficitents:\n")
      base::print(beta)
      cat("Residuals:\n")
      base::print(e_hat)
      cat("Degree of freedom:",df,"\n")
      cat("t value:\n")
      base::print(t_val)
      cat("p value:\n")
      base::print(p_val)
      cat("Residual Variance: \n")
      base::print(var_resid)
    }
  )

)
