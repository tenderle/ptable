#' pt_plot_tMatrix
#'
#' Function to plot the transition matrix using the R-package \code{\link{ggplot}}.
#'
#' @param pert_table an object of class \code{\linkS4class{ptable}}
#' @param file if not \code{NULL}, a path to a file where the graph is saved to as pdf
#'
#' @author Tobias Enderle
#' @keywords plot
#'
#' @examples
#' # Simple Example
#' params <- pt_create_pParams(D=5, V=2, label="Example")
#' ptable_destatis <- pt_create_pTable(params=params, type="destatis")
#' fifi_plot(ptable_destatis, type="t")
#'
#' \dontrun{
#' ## Export result
#' fifi_plot(ptable_destatis, type="t", file="example_tMatrix.pdf")
#' }
#' @rdname pt_plot_tMatrix
#' @import ggplot2
#' @import RColorBrewer
#'
pt_plot_tMatrix <- function(pert_table, file=NULL){
  
  i <- j <- p <- NULL
    
  if (!is.null(file)) {
    stopifnot(is_scalar_character(file))
  }
  
  
  tMatrix <- slot(pert_table,"dFrame")
  tMatrix <- tMatrix[,c("i","j","p")]
  
  tMatrix[, i:=factor(i, levels=rev(levels(i)))]
  
  # TODO: relative text sizes
  
  output <- ggplot( tMatrix, aes(j, i)) + 
    geom_tile(aes(fill=p), color="lightgrey") +
    labs(title="Transition Matrix", y="i (original frequency)", x="j (target frequency)") +
    theme(legend.position="bottom") +
    scale_fill_gradient(low = "white", high = "red", name="probability") +
    geom_text(aes(label = round(p, 4)),  size=4) +
    scale_x_discrete(position="top") +
    #scale_y_discrete(limits=rev(levels(i))) +
    #geom_rect(data=..., size=1, fill=NA, colour="black",
    #          aes(xmin=j - 0.5, xmax=j + 0.5, ymin=i - 0.5, ymax=i + 0.5)) +
    theme(axis.text =element_text(size = 16),
          axis.title = element_text(size = 18),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
      legend.position = "right",
          legend.background = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_blank(),
          axis.ticks=element_blank(),
          axis.text.y = element_text(margin = margin(r = 0, l=10), color="black"),
          axis.text.x = element_text( color="black")
    )
  
  if (!is.null(file)) {
    ggsave(filename=file, width=8, height=5)
    cat("graph of transition matrix saved to",shQuote(file),"\n")
  }
  return(output)
}
