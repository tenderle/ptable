#' Method to plot results
#' @name plot
#' @rdname plot
#' @param obj an object of class \code{\linkS4class{ptable}}
#' @param type (character) type of graph: distribution "d" (standard), perturbation panel ("p"), transition matrix "t"
#' @param file if not \code{NULL}, a path to a file where the graph is saved to as pdf
#' @param ... additional parameters passed to methods
#' 
#' @examples
#' # Simple Example
#' params <- pt_create_pParams(D=2, V=1.08,js=1, mono=FALSE, label="Example")
#' ptable_destatis <- pt_create_pTable(params=params)
#' plot(ptable_destatis, type="d")
#' 
#' \dontrun{
#' ## Export result
#' plot(ptable_destatis, type="d", file="graph.pdf")
#' }
#' 
#' @exportMethod plot
setGeneric("plot", function(obj, type="d", file=NULL, ...) {
  fifi_plot(obj, type=type, file=file,...)
})
