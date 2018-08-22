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
#' \dontrun{
#' ptable()
#' }
#'
#' @rdname ptable
#' @export
#'
#' @import flexdashboard
#'
ptable <-function(){
  filepath_windows <- system.file(package="ptable", "pt_dashboard.Rmd")
  run(file = filepath_windows, shiny_args = list(launch.browser = TRUE))
}
