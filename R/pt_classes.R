#' An S4 class to represent perturbation parameters
#' @slot D (integer) parameter for maximum perturbation
#' @slot V (numeric) parameter for perturbation variance
#' @slot js (integer) parameter for original counts not to be perturbed
#' @slot ncat (integer) number of perturbation classes
#' @slot pstay numeric vector specifying parameter for non-perturbation
#' @slot optim (integer) specifying optimization parameter for optimization function
#' @slot mono (logical) vector specifying optimization parameter for monotony condition
#' @slot label (character) label for output
#' @slot icat (integer) categorized original frequencies i
#' @slot table (character) type of table: frequency counts (cnts) or magnitude (nums)
#' @slot step (integer) step width between 0 and 1
#' @slot type (character) type of 
#' @name ptable_params-class
#' @rdname ptable_params-class
#' @export
setClass("ptable_params",
         representation=list(
           D="integer",
           V="numeric",
           js="integer",
           ncat="integer",
           pstay="numeric",
           optim="integer",
           mono="logical",
           table="character",
           icat="integer",
           step="integer",
           type="character",
           label="character"
         ),
         prototype=list(
           D=c(),
           V=numeric(),
           js=c(),
           ncat=integer(),
           pstay=numeric(),
           optim=integer(),
           mono=logical(),
           table=character(),
           icat=integer(),
           step=integer(),
           type=character(),
           label=character()
         ),
         validity=function(object) {
           stopifnot(is_integerish(object@D))
           stopifnot(is_double(object@V))
           stopifnot(is_integerish(object@js))
           stopifnot(is_double(object@pstay))
           stopifnot(is_integerish(object@optim))
           stopifnot(is_logical(object@mono))
           stopifnot(is_integerish(object@icat))
           stopifnot(is_integerish(object@step))

           if (is_empty(object@label))
             stop("Please label your pTable (without blanks)!")
           
           if( !(all(object@D>0)) )
             stop("Parameter D: must be a positive scalar or vector of positive integer(s)\n")

           if( !(object@js >= 0) )
             stop("Parameter js: must be NULL (in case of no blocking) or a postivie integer value", call. = FALSE)

           if (is_empty(object@table))
             stop("Please define your type of table: either 'cnts' or 'nums'.")
           
           if( !(object@table %in% c("cnts","nums")) )
             stop("Type of table must be either 'cnts' or 'nums'.", call. = FALSE)
           
           if( !(object@type %in% c("all","even","odd")) )
             stop("Type must be either 'all', 'even' or 'odd'.", call. = FALSE)
           
           #if( (object@step <= 0 | object@step > 1) )
          #   stop("Parameter 'step': must be a postivie value and less then 1.", call. = FALSE)
           
           if ((object@table=="nums") & is_empty(object@icat))
             stop("You specified a freuency table (table='nums'). So, please define the argument 'icat' !")
           
           return(TRUE)
         })
NULL

#' An S4 class to represent perturbation table
#' @slot pMatrix (matrix) perturbation matrix with probabilities
#' @slot pClasses (numeric) numeric classes
#' @slot pTable (data.table) perturbation table with probabilities
#' @slot dFrame (data.frame) for visualizing issues (to be done in upcoming releases)
#' @slot empResults (data.table) ...
#' @slot pParams a \code{\linkS4class{ptable_params}} object
#' @slot tStamp (character) ...
#' @slot type (character) 
#' @slot table (character) type of table: frequency counts (cnts) or magnitude (nums)
#' @name ptable-class
#' @rdname ptable-class
#' @export
setClass("ptable",
         representation=list(
           pMatrix="matrix",
           pClasses="numeric",
           pTable="data.table",
           dFrame="data.table",
           empResults="data.table",
           pParams="ptable_params",
           tStamp="character",
           type="character",
           table="character"
         ),
         prototype=list(
           pMatrix=matrix(),
           pClasses=numeric(),
           pTable=data.table(),
           dFrame=data.table(),
           empResults=data.table(),
           pParams=NULL,
           tStamp=character(),
           type=character(),
           table=character()
         ),
         validity=function(object) {


           return(TRUE)
         })
NULL
