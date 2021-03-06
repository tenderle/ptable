#' pt_plot
#'
#' Function to visualize the perturbation table.
#'
#' @param obj an object of class \code{\linkS4class{ptable}}
#' @param type (character) type of graph: distribution "d" (standard), perturbation panel ("p"), transition matrix "t"
#' @param file if not \code{NULL}, a path to a file where the graph is saved to as pdf
#'
#' @author Tobias Enderle
#' @keywords plot
#'
#' @rdname pt_plot
#'
fifi_plot <- function(obj, type="d", file=NULL){
  
  if (!is.null(file)) {
    stopifnot(is_scalar_character(file))
  }
  
  # TODO: generic function plot(...) in pt_methods.R
  
  if (type == "d") {
    cat("Distribution of Perturbation Values\n")
    out <- pt_plot_pD(pert_table=obj, file=file)
  }
  if (type == "p") {
    cat("Perturbation Panel\n")
    out <- pt_plot_pPanel(pert_table=obj, file=file)
  }
  if (type == "t") {
    cat("Transition Matrix\n")
    out <- pt_plot_tMatrix(pert_table=obj, file=file)
  }
  
  return(out)

}
