
linreg <- setRefClass("linreg",
  fields = list(
    y = "numeric",
    X = "matrix",
    y_hat = "matrix",
    beta = "numeric",
    e_hat = "matrix",
    df = "numeric",
    var_resid = "numeric",
    var_hat = "numeric",
    t_val = "matrix",
    data = "data.frame"
  ),
  methods = list(
    initialize = function(formula, data){
      print("User::initialize")
      data <<- data
      f <- as.formula(formula)
      X <<- model.matrix(f,data)
      y <<- data[,all.vars(f)[1]]
      beta <<- as.vector(solve(t(X)%*%X)%*%t(X)%*%y)
      y_hat <<- as.matrix(X%*%beta)
      e_hat <<- y-y_hat
      df <<- length(y) - ncol(X)
      var_resid <<- as.numeric((t(e_hat)%*%e_hat)/df)
      var_hat <<- as.numeric(var_resid %*% solve(X %*% t(X)))
      #t_val <<- beta/(sqrt(var_resid))
    },
    coeff = function(){
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
    }



  )

)


