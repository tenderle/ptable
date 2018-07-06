#' ptable
#'
#' Perturbation Table Visualizer
#'
#' @md
#'
#'
#' @author Tobias Enderle, \email{tobias.enderle@@destatis.de}
#' @keywords perturbation table, visualisation, dashboard, flexdashboard, shiny
#'
#' @examples
#' ptable()
#'
#' @rdname ptable
#' @export
#'
#' @import flexdashboard
#'
ptable <-function(){
  filepath_windows <- paste(.libPaths(),"/ptable/pt_dashboard.Rmd",sep="")
  rmarkdown::run(file = filepath_windows)
}
