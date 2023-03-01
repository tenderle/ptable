#' @title Perturbation Table Dashboard for Frequency Count Tables
#' @description In the `ptable`-package there is a shiny app for first
#' time users and visual-style learners. [ptable()] makes it easy to experiment
#' with different parameter
#' settings while getting direct feedback by means of graphical plots and
#' summaries. The different result output tabs are:
#'
#' * `Perturbation Table` shows the output used for applying CKM methods.
#' * `Constraints Check` can be used to check the main constraints (e.g., zero 
#' mean, fixed variance)
#' * `Input Code` could be used for replication of the results (i.e. copy&paste 
#' the code for your R script).
#' * `Input Object` shows the input object derived from the parameters a user 
#' set.
#' * `Legend` gives an overview of used parameters.
#'
#' Users can also visually learn how input parameters effect
#' the perturbation table:
#'
#' * `Transition Matrix`
#' * `Distribution` Plot
#' * `Perturbation Panel` Plot
#' @md
#'
#' @seealso
#' See [create_cnt_ptable()] to get more help or [pt_vignette()] for an 
#' introduction
#'
#' @author Tobias Enderle, \email{tobias.enderle@@destatis.de}
#' @keywords perturbation table visualisation dashboard flexdashboard shiny
#' 
#' @note After usage (e.g. closing the browser tab), interrupt R to stop the 
#' application (usually by pressing Ctrl+C or Esc in the console or by using
#' the stop button in RStudio).
#'
#' @return No return value, the dashboard is opened in the default browser.
#'
#' @examples 
#' \donttest{
#' # Run the dashboard in your default browser
#' ptable()
#' }
#'
#' @rdname ptable
#' @export
#'
#' @import flexdashboard
#'
ptable <- function() {
  if (interactive()) {
    
    filepath_windows <-
      system.file(package = "ptable", "pt_dashboard.Rmd")
    
    run(file = filepath_windows,
        shiny_args = list(launch.browser = TRUE,
                          quiet = TRUE))
  }
}
