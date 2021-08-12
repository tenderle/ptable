#' A quick ptable that can be used in various examples
#'
#' [pt_ex_cnts()] returns a perturbation table object from
#' [create_cnt_ptable()] with some default parameters. This is useful
#' for quickly creating ptables to demonstate usage in other tools.
#'
#' @return an object of [ptable-class]
#' @export
#' @md
#' @examples
#' ptab <- pt_ex_cnts()
#' plot(ptab, type = "t")
pt_ex_cnts <- function() {
  create_cnt_ptable(D = 2, V = 1.05, mono = c(TRUE, TRUE, FALSE, TRUE), js = 1)
}

# parity: two tables for even/odd numbers
# separation: an extra ptable for very small cells
#' Quick ptables for numeric variables
#'
#' [pt_ex_nums()] returns a perturbation table objects from
#' [create_num_ptable()] with some default parameters. This is useful
#' for quickly creating ptables to demonstate usage in other tools.
#'
#' @param parity a scalar logical; if `TRUE`, a single ptable will be generated.
#' If `FALSE`, two ptables for even and odd numbers are created
#' @param separation a scalar logical; if `TRUE`, an additional ptable with
#' variance `1` will be returned that is designed to perturb small cell values
#'
#' @return a [ptable-class] object if both `parity` and `separation` are `FALSE`,
#' else a named list
#' @export
#' @md
#' @examples
#' # class(pt_ex_nums(parity = FALSE, separation = FALSE)) == "ptable"
#'
#' # extra ptable for small cells
#' # names(pt_ex_nums(parity = FALSE, separation = TRUE))
#'
#' # different ptables for even/odd cells
#' # names(pt_ex_nums(parity = TRUE, separation = TRUE))
pt_ex_nums <- function(parity = TRUE, separation = FALSE) {
  stopifnot(rlang::is_scalar_logical(parity))
  stopifnot(rlang::is_scalar_logical(separation))
  
  res <- list()
  # special ptab for small cells
  if (separation) {
    res$small_cells <- create_num_ptable(
      D = 5,
      V = 1,
      optim = c(4, 1, 1),
      step = 5,
      icat = c(1, 3, 5),
      type = "all")
  }
  
  if (parity) {
    res$all <- create_num_ptable(
      D = 10,
      V = 3,
      step = 2,
      optim=c(4,1,1),
      icat = c(1, 5, 10),
      type = "all")
  } else {
    res$even <- create_num_ptable(
      D = 8,
      V = 1,
      step = 2,
      icat = c(1, 5, 8),
      type = "even")
    res$odd <- create_num_ptable(
      D = 10,
      V = 2,
      optim = c(4, 1),
      step = 4,
      icat = c(1, 10),
      type = "odd")
  }
  
  if (parity == TRUE & separation == FALSE) {
    return(res$all)
  } else {
    return(res[sort(names(res))])
  }
}