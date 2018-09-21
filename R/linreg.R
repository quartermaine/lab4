
linreg <- setRefClass("linreg",
  fields = list(
    y = "numeric",
    X = "data.frame",
    y_hat = "numeric",
    beta = "numeric",
    e_hat = "numeric",
    df = "numeric",
    var_resid = "numeric",
    var_hat = "numeric",
    t_val = "numeric"
  ),
  methods = list(
    set = function(formula, data){
      f <- as.formula(formula)
      X <<- model.matrix(f,data)
      y <<- data[,all.vars(f)[1]]
    },
    coef = function(){
      beta <<- as.vector(solve((t(X)%*%X)%*%t(X)%*%y))
      return(beta)
    },
    pred = function(){
      y_hat <<- X%*%beta
    },
    resid = function(){
      e_hat <<- y-y_hat
    },
    freedomdegree = function(){
      df <<- nrows(y) - ncols(X)
    },
    residualvariance = function(){
      var_resid <<- (t(e_hat)%*%e_hat)/df
    },
    coeffvariance = function(){
      var_hat <<- var_resid%*%solve(t(X)%*%X)
    },
    t_values = function(){
      t_val <<- beta/(sqrt(var_resid))
    },
    summary = function(){
      return("hhhhhh")
    }
  )

)
data <- iris
a <- linreg$

