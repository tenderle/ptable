#' An S4 class to represent perturbation parameters
#' @slot D (integer) parameter for maximum perturbation
#' @slot V (numeric) parameter for perturbation variance
#' @slot js (integer) parameter for original counts not to be perturbed
#' @slot ncat (integer) number of perturbation classes
#' @slot epsilon (numeric) optimization parameter
#' @slot pstay numeric vector specifying parameter for non-perturbation
#' @slot optim (integer) specifying optimization parameter for optimization function
#' @slot mono (logical) vector specifying optimization parameter for monotony condition
#' @slot pTableSize (integer) number of columns in pTable (abs-format)
#' @slot label (character) label for output
#' @name ptable_params-class
#' @rdname ptable_params-class
#' @export
setClass("ptable_params",
         representation=list(
           D="integer",
           V="numeric",
           js="integer",
           ncat="integer",
           epsilon="numeric",
           pstay="numeric",
           optim="integer",
           mono="logical",
           pTableSize="integer",
           label="character"
         ),
         prototype=list(
           D=c(),
           V=numeric(),
           js=c(),
           ncat=integer(),
           epsilon=double(),
           pstay=numeric(),
           optim=integer(),
           mono=logical(),
           pTableSize=integer(),
           label=character()
         ),
         validity=function(object) {
           stopifnot(is_integerish(object@D))
           stopifnot(is_double(object@V))
           stopifnot(is_integerish(object@js))
           stopifnot(is_double(object@epsilon))
           stopifnot(is_double(object@pstay))
           stopifnot(is_integerish(object@optim))
           stopifnot(is_logical(object@mono))


           #stopifnot(all(object@mTable>0))

           if (is_empty(object@label))
             stop("Please label your pTable (without blanks)!")

           if( !(all(object@D>0)) )
             stop("Parameter D: must be a positive scalar or vector of positive integer(s)\n")

           if( !(object@js >= 0) )
             stop("Parameter js: must be NULL (in case of no blocking) or a postivie integer value", call. = FALSE)

           return(TRUE)
         })
NULL

#' An S4 class to represent perturbation table
#' @slot pMatrix (matrix) perturbation matrix with probabilites
#' @slot pClasses (numeric) numeric classes
#' @slot pTable (data.table) perturbation table with probabilities
#' @slot dFrame (data.frame) for visualizing issues (to be done in upcoming releases)
#' @slot empResults (data.table) ...
#' @slot pParams a \code{\linkS4class{ptable_params}} object
#' @slot tStamp (character) ...
#' @slot type (character) specifying the type of pTable (either 'abs' or 'destatis')
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
           type="character"
         ),
         prototype=list(
           pMatrix=matrix(),
           pClasses=numeric(),
           pTable=data.table(),
           dFrame=data.table(),
           empResults=data.table(),
           pParams=NULL,
           tStamp=character(),
           type=character()
         ),
         validity=function(object) {


           return(TRUE)
         })
NULL
