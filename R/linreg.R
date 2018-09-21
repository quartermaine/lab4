
linreg <- setRefClass("linreg",
  fields = list(
    y = "numeric",
    X = "matrix",
    y_hat = "numeric",
    beta = "numeric",
    e_hat = "numeric",
    df = "numeric",
    var_resid = "numeric",
    var_hat = "numeric",
    t_val = "numeric",
    data = "data.frame"
  ),
  methods = list(
    initialize = function(formula, data){
      print("User::initialize")
      data <<- data
      f <- as.formula(formula)
      X <<- model.matrix(f,data)
      y <<- data[,all.vars(f)[1]]
    },


    coef = function(){
      beta <<- as.vector(solve(t((X)%*%X)%*%t(X)%*%y))
      return(beta)
    }
  )

)


