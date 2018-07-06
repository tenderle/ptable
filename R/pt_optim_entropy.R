#' pt_optim_entropy
#'
#' Function to solve the non-linear optimization problem used within \code{\link{ptable}()}.
#'
#' @param optim optimization paramter (1=standard, 2-4=further test implementations)
#' @param mono (logical) monotony parameter
#' @param v (integer) vector with perturbation values (i.e. deviations to the original frequency)
#' @param variance (numeric) variance parameter
#' @param lb (integer) vector with lower bounds of the controls
#' @param ub (integer) vector with upper bounds of the controls
#' @param x0 (integer) vector with starting values for the optimization
#'
#' @seealso Giessing, S. (2016), 'Computational Issues in the Design of Transition Probabilities and Disclosure Risk Estimation for Additive Noise'. In: Domingo-Ferrer, J. and Pejic-Bach, M. (Eds.), Privacy in Statistical Databases, pp. 237-251, Springer International Publishing, LNCS, vol. 9867.
#' @seealso Fraser, B. and Wooton, J.: A proposed method for confidentialising tabular output to pro-tect against differencing. In: Monographs of Official Statistics. Work session on Statistical Data Confidentiality, Eurostat-Office for Official Publications of the European Communi-ties, Luxembourg, 2006, pp. 299-302
#' @return The return value contains a list with two elements:
#' \describe{
#'  \item{"\code{result}"}{   optimal value of the controls}
#'  \item{"\code{iter}"  }{   number of iterations that were executed}
#' }
#'
#' @author Tobias Enderle, Sarah Giessing, Jonas Peter
#' @keywords optimization
#'
#' @rdname pt_optim_entropy
#' @importFrom nloptr nloptr
#'
pt_optim_entropy <- function(optim=optim, mono=mono,
                     v=v,
                     variance=variance,
                     #epsilon=epsilon,
                     lb=p_lb,
                     ub=p_ub,
                     x0=rep(1, length(v))){

  v <- p <- p_lb <- p_ub <- NULL
  options(digits=7,scipen=7)

  # Fixed parameters
  local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
                      "xtol_rel"  = 1.0e-7 )
  opts <- list( "algorithm" = "NLOPT_LD_SLSQP",
                "xtol_rel"  = 1.0e-7,
                "maxeval"   = 100000,
                "local_opts" = local_opts )

  # Optimization functions (according to parameter optim)
  if (optim == 1) {
    fct_eval_g_ineq <- eval_g_ineq_v1
    fct_eval_g_eq <- eval_g_eq_v1_v2_v3
  }
  if (optim == 2) {
    fct_eval_g_ineq <- eval_g_ineq_v2
    fct_eval_g_eq <- eval_g_eq_v1_v2_v3
  }
  if (optim == 3) {
    fct_eval_g_ineq <- eval_g_ineq_v3
    fct_eval_g_eq <- eval_g_eq_v1_v2_v3
  }
  if (optim == 4) {
    fct_eval_g_ineq <- eval_g_ineq_v4
    fct_eval_g_eq <- eval_g_eq_v4
  }


  # Optimization
  res <- nloptr( x0=x0,
                 eval_f=eval_f,
                 lb=lb,
                 ub=ub,
                 eval_g_ineq=fct_eval_g_ineq,
                 eval_g_eq=fct_eval_g_eq,
                 opts=opts,
                 v=v,
                 variance=variance,
                 mono=mono)

  result<-res$solution
  iter <- res$iterations
  return(list(result=result, iter=iter))

}




## Objective function (=entropy) with gradient
eval_f <- function( x, v=v, variance=variance, mono=mono ) {
  return( list( "objective" = sum(x*log2(x)),
                "gradient" = log2(x)+1 )           )
}


eval_g_mono <- function(x=x, v=v, constr=constr, grad=grad){

  # Returns position z where v[z]=0
  z <- ifelse( 0 %in% v , which(v==0), 0)

  if (z>1)                          {
    for (j in (1:(z-1)))
    {
      constr<-c(constr, x[j]-x[j+1])
      temp12<-rep(0,length(v))
      temp12[j]<-1
      temp12[j+1]<- -1
      grad<-rbind(grad,temp12)
    }
  }
  return(list(constr=constr, grad=grad))
}



## Inequality functions
# constr inequality constraints (each element is applied as '<= 0')

# x-1                   (3 or 4?) all probabilities are >0 and <1
# sum(v^2*x)-variance   (2) constant variance
# -x                    (?)

eval_g_ineq_v1 <- function( x, v=v, variance=variance, mono=mono ) {

  constr <- c(x-1, sum(v^2*x)-variance  )
  grad   <- rbind(diag(1,length(v),length(v)),v^2)

  # monotony condition
  if (mono) {
    mono_fct <- eval_g_mono(x=x, v=v, constr=constr, grad=grad)
    constr <- mono_fct$constr
    grad <- mono_fct$grad
  }

  return( list( "constraints"=constr, "jacobian"=grad ) )
}



eval_g_ineq_v2 <- function( x, v=v, variance=variance, mono=mono ) {

  constr <- c(-x,sum(v^2*x)-variance  )
  grad   <- rbind(diag(-1,length(v),length(v)),v^2)

  # monotony condition
  if (mono) {
    mono_fct <- eval_g_mono(x=x, v=v, constr=constr, grad=grad)
    constr <- mono_fct$constr
    grad <- mono_fct$grad
  }

  return( list( "constraints"=constr, "jacobian"=grad ) )
}



eval_g_ineq_v3 <- function( x, v=v, variance=variance, mono=mono ) {

  constr <- c(sum(v^2*x)-variance  )
  grad   <- rbind(v^2)

  # monotony condition
  if (mono) {
    mono_fct <- eval_g_mono(x=x, v=v, constr=constr, grad=grad)
    constr <- mono_fct$constr
    grad <- mono_fct$grad
  }

  return( list( "constraints"=constr, "jacobian"=grad ) )
}




eval_g_ineq_v4 <- function( x, v=v, variance=variance, mono=mono ) {

  constr <- c(x-1)
  grad   <- rbind(diag(1,length(v),length(v)))

  # monotony condition
  if (mono) {
    mono_fct <- eval_g_mono(x=x, v=v, constr=constr, grad=grad)
    constr <- mono_fct$constr
    grad <- mono_fct$grad
  }

  out <- list( "constraints"=constr, "jacobian"=grad )

  return( out )
}


## Equality functions
# (each element in 'constr' is applied as '= 0')

# sum(x)- 1             (5) probabilities sum up to 1
# sum(v*x)              (1) expectation of 0
# sum(v^2*x)-variance   (2) constant variance

eval_g_eq_v1_v2_v3 <- function( x, v=v, variance=variance, mono=mono ){

  constr <- c( sum(x)- 1, sum(v*x) )
  grad   <- rbind(rep(1,length(v)), v)

  return( list( "constraints"=constr, "jacobian"=grad ) )
}


eval_g_eq_v4 <- function( x, v=v, variance=variance, mono=mono ){

  constr <- c( sum(x)- 1, sum(v*x), sum(v^2*x)-variance )
  grad   <- rbind(rep(1,length(v)), v, v^2)

  return( list( "constraints"=constr, "jacobian"=grad ) )
}


