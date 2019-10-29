#' pt_vignette
#'
#' starts the package vignette that gets you started with the package
#'
#' @return a browser windows/tab with showing the vignette
#' @export
#' @importFrom utils RShowDoc
#' @examples
#' \dontrun{
#' pt_vignette()
#' }
pt_vignette <- function() {
  RShowDoc("introduction", package="ptable")
}