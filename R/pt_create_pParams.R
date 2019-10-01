#' pt_create_pParams
#'
#' creates the required input for \code{\linkS4class{ptable}}.
#'
#' @param D perturbation parameter for maximum noise/perturbation (scalar or vector)
#' @param V perturbation parameter for variance (scalar)
#' @param js treshold value for blocking of small frequencies (i.e. the perturbation will not produce positive cell values that are equal to or smaller than the treshold value).
#' @param pstay optional parameter to set the probability (0 < p < 1) of an original frequency to remain unperturbed: NA (default) no preset probability (i.e. produces the maximum entropy solution)
#' @param optim optimization parameter: \code{1} standard approach (default)
#' @param mono (logical) vector specifying optimization parameter for monotony condition
#' @param label (character) label of the Output
#' @param step (integer) step width
#' @param icat (integer) categorized original frequencies i
#' @param table (character) type of the table: frequency or magnitude table
#' @param type (character) indicator for the extra column 'type' used for magnitude tables: 'even', 'odd' or 'all' (default)
#'
#' @return an object of \code{\linkS4class{ptable_params}}
#'
#' @author Tobias Enderle, \email{tobias.enderle@@destatis.de}
#' @keywords data
#'
#' @examples
#' # parameter setting for frequency tables
#' pt_create_pParams(D=5, V=2, js=2, pstay=0.5, optim=1, mono=TRUE)
#'
#' # parameter setting for magnitude tables
#' pt_create_pParams(D=5, V=2, table="nums", step=4, icat=c(1,3,5))
#' 
#' @rdname pt_create_pParams
#' @export
#'
pt_create_pParams <-function(D, V, js=0, pstay=NULL, optim=1, mono=TRUE, table="cnts", step=2, icat=NULL, type="all", label=paste("D",D,"V",V*100,sep="")){

  out <- new("ptable_params")


  stopifnot(is_bare_integerish(D))
  stopifnot(is_bare_numeric(V))
  stopifnot(is_bare_integerish(js))
  stopifnot(is_bare_integerish(optim))
  stopifnot(is_bare_logical(mono))
  stopifnot(is_bare_integerish(step))
  

  if (is.null(pstay)) pstay <- NA
  #stopifnot(is_bare_numeric(pstay))
  if(sum(c(0,1) %in% pstay) > 0)
    stop(paste("Parameter 'pstay' must be larger than zero and smaller than one (i.e. 0 < pstay < 1)."))

  if (table=="cnts"){
    if (js==0) ncat <- D
    else ncat <- D+js+1
  }
  if (table=="nums"){
    icat <- sort(unique(icat))
    ncat <- length(icat)
  }
  
  if (table=="cnts") {
    message(paste("Since type of table is frequency table (argument table is set to 'cnts'), the input parameters 'step' and 'icat' will be ignored."))
    step <- 1
    icat_extra <- icat[icat > ncat]
    icat <- c(1:ncat, icat_extra)
    ncat <- length(icat)
  }

  slot(out, "ncat") <- as.integer(ncat)
  
  if (table=="nums"){
    
    if (!all(is.na(pstay)) | js > 0)
        message(paste("Since type of table is magnitude table (argument table is set to 'nums'), the input parameters 'js' and 'pstay' will be ignored in the current version."))
    
    js <- 0
    pstay <- NA
    
    if (!all(c(1,D) %in% icat))
      stop(paste("There must be 1 and 'D' in 'icat', e.g. 'icat=c(1,D)'.\n"), call. = FALSE)
    
    if (max(icat) > D)
      stop(paste("The largest entry of 'icat' can be 'D'.\n"), call. = FALSE)
    
    #if (!(length(icat) > 1))
    #  stop(paste("Argument 'icat' must be of length 2.\n"))
    
    if ( (D*step) >= 50 )
      stop(paste("Pleas reduce either 'D' or 'step' or both of them. 'D/step' isn't allowed to be larger than 50.\n"), call. = FALSE)
  }
  
  
  label <- gsub(" ","_",label)

  if( (!is_scalar_vector(pstay)) && (length(pstay) != ncat) )
    stop(paste("The length of parameter vector 'pstay' is ",length(pstay)," but must be of length ",ncat," (Alternatively the parameter can be set as a scalar.)\n"), call. = FALSE)
  if( (!is_scalar_vector(optim)) && (length(optim) != ncat) )
    stop(paste("The length of parameter vector 'optim' is ",length(optim)," but must be of length ",ncat," (Alternatively the parameter can be set as a scalar.)\n"), call. = FALSE)
  if( (!is_scalar_vector(mono)) && (length(mono) != ncat) )
    stop(paste("The length of parameter vector 'mono' is ",length(mono)," but must be of length ",ncat," (Alternatively the parameter can be set as a scalar.)\n"), call. = FALSE)


  # replicate parameters if scalar
  if(is_scalar_vector(pstay)) pstay <- rep(pstay, ncat)
  if(is_scalar_vector(optim)) optim <- rep(optim, ncat)
  if(is_scalar_vector(mono)) mono <- rep(mono, ncat)
  


  slot(out, "D") <- as.integer(D)
  slot(out, "V") <- as.double(V)
  slot(out, "js") <- as.integer(js)
  
  slot(out, "icat") <- as.integer(icat)

  slot(out, "pstay") <- as.double(pstay)
  slot(out, "optim") <- as.integer(optim)
  slot(out, "mono") <- as.logical(mono)
  slot(out, "type") <- as.character(type)

  slot(out, "table") <- as.character(table)
  slot(out, "step") <- as.integer(step)
  
  slot(out, "label") <- as.character(label)
  validObject(out)
  out
}
