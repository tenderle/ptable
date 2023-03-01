#' @title Vignette
#'
#' @description Starts the package vignette that gets you started with the 
#' package
#'
#' @return a browser windows/tab with showing the vignette
#' @export
#' @importFrom utils RShowDoc
#' @examples
#' \donttest{
#' pt_vignette()
#' }
pt_vignette <- function() {
  if (interactive())
    RShowDoc("introduction", package = "ptable")
}
