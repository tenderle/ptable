#' @rdname ptable_pkg
#' @md
#'
#' @title Noise Probability Generator for the Cell-Key Method (CKM)
#'
#' @description 
#' [ptable](https://github.com/sdcTools/ptable) makes it easy to create perturbation tables
#' that can be used for applying noise to statistical tables with any 
#' cell-key method approach - among others either the [cellKey()](https://github.com/sdcTools/cellKey)-package 
#' or the standalone tool 
#' [TauArgus](https://github.com/sdcTools/tauargus). 
#'
#' The package
#' provides four main functions to create the perturbation tables:
#' 
#' * `create_ptable()`: generic function that creates a ptable, either for frequency
#' count or magnitude tables with a various set of options.
#'
#' * `create_cnt_ptable()`: creates a ptable suitable for frequency count 
#' tables.
#' 
#' * `create_num_ptable()`: creates a ptable suitable for magnitude tables 
#' (i.e. with numerical variables).
#' 
#' * [modify_cnt_ptable()]: modifies the ptable for a higher level of protection
#' 
#' @details The perturbation probabilities are constructed given the following 
#' constraints:
#' - Maximum noise
#' - Zero mean (unbiased noise)
#' - Fixed noise variance
#' - Transition probabilities are between zero and one and the sum up to 1
#' - Perturbations will not produce negative cell values or positive cell values 
#' equal to or less than a specific threshold value 
#' 
#' @param D perturbation parameter for maximum noise/perturbation (scalar or vector)
#' @param V perturbation parameter for variance (scalar)
#' @param js threshold value for blocking of small frequencies (i.e. the perturbation
#' will not produce positive cell values that are equal to or smaller than 
#' the threshold value).
#' @param pstay optional parameter to set the probability (0 < p < 1) of 
#' an original frequency to remain unperturbed: NA (default) no preset 
#' probability (i.e. produces the maximum entropy solution)
#' @param optim optimization parameter: `1` standard approach (default) with
#' regular constraints, `4` alternative approach with simplified constraints (may work
#' if constraints using the standard approach are violated)
#' @param mono (logical) vector specifying optimization parameter for 
#' monotony condition
#' @param label (character) label of the Output
#' @param step (integer) step width
#' @param icat (integer) categorized original frequencies i
#' @param table (character) type of the table: frequency or magnitude table
#' @param type (character) indicator for the extra column 'type' used for 
#' magnitude tables: 'even', 'odd' or 'all' (default)
#' @param monitoring (logical) output monitoring on/off
#' @param debugging (logical) debug monitoring on/off
#' @param create (logical) scalar specifying (default: TRUE)
#' 
#' @seealso [pt_export()] to export the perturbation table for external SDCtools
#' like [TauArgus](https://github.com/sdcTools/tauargus) or SAS.
#'
#' @return an object of [ptable-class]
#' 
#' @examples
#' 
#' # create ptable for frequency or magnitude tables
#' create_ptable(D = 3, V = 1.08, js = 1, table="cnts")
#' 
#' # create ptable for frequency count tables 
#' create_cnt_ptable(D = 3, V = 1.08, js = 1, label = "ptable_frequency_tab")
#' 
#' # create ptable for magnitude tables 
#' create_num_ptable(D = 5, V = 2, step = 4, icat = c(1, 3, 5))
#' 
#' @export
create_ptable <- function(D,
                          V,
                          js = 0,
                          pstay = NULL,
                          optim = 1,
                          mono = TRUE,
                          step = 1,
                          icat = NULL,
                          table = "cnts",
                          type = "all",
                          label = paste0("D", D, "V", V * 100),
                          monitoring = FALSE,
                          debugging = FALSE,
                          create = TRUE) {
  
  params <-   pt_create_pParams(
    table = table,
    D = D,
    V = V,
    js = js,
    pstay = pstay,
    optim = optim,
    mono = mono,
    type = type,
    step = step,
    icat = icat,
    label = label)
  
  if (create){
    
    ptab <- pt_create_pTable(
      params = params,
      monitoring = monitoring,
      debugging = debugging)
    
    out <- ptab
    
  } else {
    
    out <- params
  }
  
  return(out)
}
#' @rdname ptable_pkg
#' @export
create_cnt_ptable <- function(D,
                              V,
                              js = 0,
                              pstay = NULL,
                              optim = 1,
                              mono = TRUE,
                              label = paste0("D", D, "V", V * 100),
                              monitoring = FALSE,
                              create = TRUE) {
  
  params <-   pt_create_pParams(
    table = "cnts",
    type = "all",
    step = 1,
    icat = NULL,
    D = D,
    V = V,
    js = js,
    pstay = pstay,
    optim = optim,
    mono = mono,
    label = label)
  
  if (create){
    
    ptab <- pt_create_pTable(
      params = params,
      monitoring = monitoring,
      debugging = FALSE)
    
    out <- ptab
    
  } else {
    
    out <- params
  }
  
  return(out)
  
}
#' @rdname ptable_pkg
#' @usage NULL
#' @export
create_cnts_ptable <- create_cnt_ptable
#' @rdname ptable_pkg
#' @export
create_num_ptable <- function(D,
                              V,
                              js = 0,
                              pstay = NULL,
                              optim = 1,
                              mono = TRUE,
                              step = 2,
                              icat = NULL,
                              type = "all",
                              label = paste0("D", D, "V", V * 100),
                              monitoring = FALSE,
                              create = TRUE) {
  
  params <-   pt_create_pParams(
    table = "nums",
    D = D,
    V = V,
    js = js,
    pstay = pstay,
    optim = optim,
    mono = mono,
    type = type,
    step = step,
    icat = icat,
    label = label)
  
  if (create){
    
    ptab <- pt_create_pTable(
      params = params,
      monitoring = monitoring,
      debugging = FALSE)
    
    out <- ptab
    
  } else {
    
    out <- params
  }
  
  return(out)
}
#' @rdname ptable_pkg
#' @usage NULL
#' @export
create_nums_ptable <- create_num_ptable