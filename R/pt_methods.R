#' @title Method to plot results
#' @description Function to plot the perturbation panel using the R-package \code{\link{ggplot2}}.
#' @name plot
#' @rdname plot

#' @inheritParams fifi_plot
#' @param ... additional parameters passed to methods
#' @inherit fifi_plot examples
#' 
#' @exportMethod plot
setGeneric("plot", function(obj, type="d", file=NULL, ...) {
  fifi_plot(obj, type=type, file=file,...)
})