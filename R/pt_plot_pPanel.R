#' pt_plot_pPanel
#'
#' Function to plot the perturbation panel using the R-package \code{\link{ggplot}}.
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
#' fifi_plot(ptable_destatis, type="p")
#'
#' \dontrun{
#' ## Export result
#' fifi_plot(ptable_destatis, type="p", file="example_pPanel.png")
#' }
#' @rdname pt_plot_pPanel
#' @import ggplot2
#'
pt_plot_pPanel <- function(pert_table, file=NULL){
  
  i <- i_char <- j <- p <- u <- v <- NULL
  
  if (!is.null(file)) {
    stopifnot(is_scalar_character(file))
  }

  params <- slot(pert_table, "pParams")
  pTable <- slot(pert_table,"pTable")
  
  pTable <- pTable[order(v,p, decreasing = TRUE)]
  pTable <- pTable[,i_char:=as.character(i)]
  pTable[i==max(i), i_char:= paste0("", i_char,"+")]
  
  lev_num <- (unique((pTable$v)))
  lev_sign <- sign(lev_num)
  lev_char <- (unique(as.character(pTable$v)))
  lev_char[lev_sign >0] <- paste0("+",lev_char[lev_sign >0])
  lev_char[lev_num == 0] <- "0 (no perturbation)"
  
  pTable[, u:= factor(v, levels = (unique(as.character(v))), labels = lev_char ) ]
  
  
  D <- slot(params, "D")
  D0 <- D+1
  
  # Colors for perturbation values
  pert_no <- brewer.pal(D0, "Greys")[1]
  pert_pos <- brewer.pal(D0, "Blues")[c(2:D0)]
  pert_neg <- brewer.pal(D0, "Greens")[c(D0:2)]
  
  
  # ggplot figure
  s <- ggplot(pTable, x=i, y=p, aes(i_char,p, fill = u ))
  
  output <- s + geom_bar(stat="identity", position = "fill") + 
    coord_flip() + 
    guides(fill= guide_legend(title="v (perturbation value):", title.position = "top", reverse=TRUE, size=16))+
    scale_fill_manual(values=c(pert_neg,pert_no,pert_pos)) +
    labs(title="Perturbation Panel", y="p (probability)", x="i (original frequency)") +
    theme(axis.text =element_text(size = 16),
          axis.title = element_text(size = 18),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          legend.position = "bottom", 
          legend.box.background = element_rect(colour = "grey"),
          legend.background = element_blank(),
          panel.grid.major.x = element_line(colour = "lightgrey"),
          panel.background = element_blank(),
          axis.ticks=element_blank(),
          axis.text.y = element_text(margin = margin(r = -15, l=5))) +
    ylim(0,1)
  
  if (!is.null(file)) {
    ggsave(filename=file, width=6, height=5)
    cat("graph of perturbation panel saved to",shQuote(file),"\n")
  }
  return(output)
  
}
