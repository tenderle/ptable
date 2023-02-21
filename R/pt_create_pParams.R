#' Define perturbation parameters
#' 
#' creates the required input for [pt_create_pTable()], [create_cnt_ptable()] 
#' or [create_num_ptable()].
#'
#' @details this function may be dropped in future versions; please use 
#' [create_cnt_ptable()] and [create_num_ptable()] directly.
#' @param D perturbation parameter for maximum noise (scalar)
#' @param V perturbation parameter for variance (scalar)
#' @param js threshold value for blocking of small frequencies 
#' (i.e. the perturbation will not produce positive cell values that are equal 
#' to or smaller than the threshold value).
#' @param pstay optional parameter to set the probability (0 < p < 1) of 
#' an original frequency to remain unperturbed: NA (default) no preset 
#' probability (i.e. produces the maximum entropy solution)
#' @param optim optimization parameter: `1` standard approach (default) with
#' regular constraints, `4` alternative approach with simplified constraints 
#' (may work if constraints using the standard approach are violated)
#' @param mono (logical) vector specifying optimization parameter for 
#' monotony condition
#' @param label (character) label of the Output
#' @param step (integer) step width
#' @param icat (integer) categorized original frequencies i
#' @param table (character) type of the table: frequency or magnitude table
#' @param type (character) indicator for the extra column 'type' used for 
#' magnitude tables: 'even', 'odd' or 'all' (default)
#'
#' @md
#' @return an object of class [ptable_params-class]
#'
#' @author Tobias Enderle, \email{tobias.enderle@@destatis.de}
#' @keywords data
#'
#' @examples
#' \dontrun{
#' # parameter setting for frequency tables
#' pt_create_pParams(D = 5, V = 2, js = 2, pstay = 0.5, optim = 1, mono = TRUE)
#'
#' # parameter setting for magnitude tables
#' pt_create_pParams(D = 5, V = 2, table = "nums", step = 4, icat = c(1, 3, 5))
#' }
#'
#' @noRd
pt_create_pParams <- function(D,
                              V,
                              js = 0,
                              pstay = NULL,
                              optim = 1,
                              mono = TRUE,
                              table = "cnts",
                              step = 2,
                              icat = NULL,
                              type = "all",
                              label = paste("D", D, "V", V * 100, sep = "")) {
  
  out <- new("ptable_params")
  stopifnot(rlang::is_bare_character(table))
  stopifnot(table %in% c("cnts", "nums"))
  stopifnot(is_bare_integerish(D))
  stopifnot(is_bare_numeric(V))
  stopifnot(is_bare_integerish(js))
  stopifnot(is_bare_integerish(optim))
  stopifnot(is_bare_logical(mono))
  stopifnot(is_bare_integerish(step))
  
  if (is.null(pstay)) {
    pstay <- NA
  }
  
  if ( sum (!((pstay < 1 ) & (pstay > 0)), na.rm = TRUE) > 0  ) {
    e <- c(
      "Parameter 'pstay' must be larger than zero and smaller",
      "than one (i.e. 0 < pstay < 1).")
    stop(paste(e, collapse = " "), call. = FALSE)
  }
  
  if (table == "cnts") {
    if (js == 0) {
      ncat <- D
    } else {
      ncat <- D + js + 1
    }
  }
  if (table == "nums") {
    icat <- sort(unique(icat))
    ncat <- length(icat)
  }
  
  if (table == "cnts") {
    
    if (!is.null(icat)) {
      message("Ignoring argument `icat` for freqency tables")
    }
    if (step > 1) {
      message("Ignoring argument `step` for freqency tables")
    }
    step <- 1
    icat_extra <- icat[icat > ncat]
    icat <- c(1:ncat, icat_extra)
    ncat <- length(icat)
  }
  
  slot(out, "ncat") <- as.integer(ncat)
  
  if (table == "nums") {
    if (!all(is.na(pstay)) | js > 0) {
      message(
        "Since type of table is magnitude table", 
        " (argument table is set to 'nums'), the input parameters 'js' and ",
        "'pstay' will be ignored in the current version.")
    }
    
    js <- 0
    pstay <- NA
    
    if (!all(c(1, D) %in% icat)) {
      stop("There must be 1 and 'D' in 'icat', e.g. 'icat = c(1, D)'.", 
           call. = FALSE)
    }
    
    if (max(icat) > D) {
      stop("The largest entry of 'icat' can be 'D'.", call. = FALSE)
    }
    
    if (step <= 0) {
      stop("Argument `step` must be a positive integer", call. = FALSE)
    }
    
    if ((D * step) >= 50) {
      e <- c(
        "Please reduce either 'D' or 'step' or both of them.",
        "'D/step' isn't allowed to be larger than 50.")
      stop(paste(e, collapse = " "), call. = FALSE)
    }
  }
  
  label <- gsub(" ", "_", label)
  
  if ((!is_scalar_vector(pstay)) && (length(pstay) != ncat)) {
    e <- c(
      "The length of parameter vector 'pstay' is", length(pstay),
      "but must be of length", ncat,
      "(Alternatively the parameter can be set as a scalar.)")
    stop(paste(e, collapse = " "), call. = FALSE)
  }
  
  if ((!is_scalar_vector(optim)) && (length(optim) != ncat)) {
    e <- c(
      "The length of parameter vector 'optim' is", length(optim),
      "but must be of length ", ncat,
      "(Alternatively the parameter can be set as a scalar.)")
    stop(paste(e, collapse = " "), call. = FALSE)
  }
  if ((!is_scalar_vector(mono)) && (length(mono) != ncat)) {
    e <- c(
      "The length of parameter vector 'mono' is", length(mono),
      "but must be of length", ncat,
      "(Alternatively the parameter can be set as a scalar.)")
    stop(paste(e, collapse = " "), call. = FALSE)
  }
  
  # replicate parameters if scalar
  if (is_scalar_vector(pstay)) {
    pstay <- rep(pstay, ncat)
  }
  if (is_scalar_vector(optim)) {
    optim <- rep(optim, ncat)
  }
  if (is_scalar_vector(mono)) {
    mono <- rep(mono, ncat)
  }
  
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
