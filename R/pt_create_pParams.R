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
#' @param epsilon (double)
#' @param label (character) label of the Output
#' @param pTableSize (number) defining the number of required columns if abs-formatted ptable is chosen as argument in \code{\link{pt_create_pTable}}
#'
#' @return an object of \code{\linkS4class{ptable_params}}
#'
#' @author Tobias Enderle, \email{tobias.enderle@@destatis.de}
#' @keywords data
#'
#' @examples
#' pt_create_pParams(D=5, V=2, js=2, pstay=0.5, optim=1, mono=TRUE)
#'
#' @rdname pt_create_pParams
#' @export
#'
pt_create_pParams <-function(D, V, js=0, pstay=NULL, optim=1, mono=TRUE, epsilon=0.0000001, label=paste("D",D,"V",V*100,sep=""), pTableSize=70){

  out <- new("ptable_params")


  stopifnot(is_bare_integerish(D))
  stopifnot(is_bare_numeric(V))
  stopifnot(is_bare_integerish(js))
  stopifnot(is_bare_integerish(optim))
  stopifnot(is_bare_logical(mono))
  stopifnot(is_bare_integerish(pTableSize))

  if (is.null(pstay)) pstay <- NA
  #stopifnot(is_bare_numeric(pstay))
  if(sum(c(0,1) %in% pstay) > 0)
    stop(paste("Parameter 'pstay' must be larger than zero and smaller than one (i.e. 0 < pstay < 1)."))


  if (js==0) ncat <- D
  else ncat <- D+js+1
  slot(out, "ncat") <- as.integer(ncat)

  stopifnot(pTableSize >= ncat)

  label <- gsub(" ","_",label)

  if( (!is_scalar_vector(pstay)) && (length(pstay) != ncat) )
    stop(paste("The length of parameter vector 'pstay' is ",length(pstay)," but must be of length ",ncat," (Alternatively the parameter can be set as a scalar.)\n"))
  if( (!is_scalar_vector(optim)) && (length(optim) != ncat) )
    stop(paste("The length of parameter vector 'optim' is ",length(optim)," but must be of length ",ncat," (Alternatively the parameter can be set as a scalar.)\n"))
  if( (!is_scalar_vector(mono)) && (length(mono) != ncat) )
    stop(paste("The length of parameter vector 'mono' is ",length(mono)," but must be of length ",ncat," (Alternatively the parameter can be set as a scalar.)\n"))


  # replicate parameters if scalar
  if(is_scalar_vector(pstay)) pstay <- rep(pstay, ncat)
  if(is_scalar_vector(optim)) optim <- rep(optim, ncat)
  if(is_scalar_vector(mono)) mono <- rep(mono, ncat)
  if(is_scalar_vector(epsilon)) epsilon <- rep(epsilon, ncat)


  slot(out, "D") <- as.integer(D)
  slot(out, "V") <- as.double(V)
  slot(out, "js") <- as.integer(js)

  slot(out, "pstay") <- as.double(pstay)
  slot(out, "optim") <- as.integer(optim)
  slot(out, "mono") <- as.logical(mono)
  #slot(out, "type") <- "destatis"

  slot(out, "epsilon") <- as.double(epsilon)
  slot(out, "label") <- as.character(label)

  slot(out, "pTableSize") <- as.integer(pTableSize)
  
  validObject(out)
  out
}
