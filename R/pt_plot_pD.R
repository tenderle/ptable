#' pt_plot_pD
#'
#' Function to plot the ditribution of the perturbation values using the R-package \code{\link{lattice}}.
#'
#' @param pert_table an object of class \code{\linkS4class{ptable}}
#' @param ylimit (numeric) vector with limits of y-axis (for probabilities)
#' @param file if not \code{NULL}, a path to a file where the graph is saved to as pdf
#'
#' @author Tobias Enderle
#' @keywords plot
#'
#' @examples
#' # Simple Example
#' ptab <- create_cnt_ptable(D = 5, V = 2, label = "Example")
#' plot(ptab, type = "d")
#'
#' \dontrun{
#' ## Export result
#' plot(ptab, type = "d", file = "graph.pdf")
#' }
#' @rdname pt_plot_pD
#' @import ggplot2
#' @importFrom utils packageVersion
#'
pt_plot_pD <- function(pert_table, ylimit=c(-0.05,0.95), file=NULL){
  v <- check <- i_info <- NULL
  i <- iter <- mw <- p <- ps <- pstay <- psum <- var <- xl <- xr <- y <- NULL
  if (!is.null(file)) {
    stopifnot(is_scalar_character(file))
  }
  
  params <- slot(pert_table, "pParams")
  timestamp <- slot(pert_table, "tStamp")
  
  empResults <- slot(pert_table, "empResults")

  D <- slot(params, "D")
  VARIANZ <- slot(params, "V")
  step <- slot(params, "step")
  
  dt_meta <- data.table(
    i = empResults$i,
    xl = -D,
    xr = D,
    y = 0.95,
    mw = empResults$p_mean,
    var = empResults$p_var,
    pstay = round(empResults$p_stay, 2),
    psum = empResults$p_sum,
    iter = empResults$iter,
    col = ifelse(VARIANZ != as.numeric(empResults$p_var), 7, 6)
  )
  
  dt_meta[, ps := ifelse(psum < 0.999999, "< 1", ifelse(psum > 1.000001, "> 1", "= 1"))][]
  dt_meta <- dt_meta[i > 0]
  
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  dFrame <- slot(pert_table,"dFrame")
  type <- slot(pert_table,"type")
  
  subFrame <- copy(dFrame)
  subFrame[, i := as.numeric(as.vector(i)) ]
  
  output <- ggplot(data = subFrame[i > 0 & check == TRUE], aes(x = as.integer(v), y = p)) +
    geom_point(group = 1, colour=cbPalette[6], size = 2) +
    geom_line(group =1, colour=cbPalette[3], size=1) +
    facet_wrap(~ i, labeller = "label_both") +
    scale_x_continuous(name="Noise (v)", limits=c(-D,D), breaks=seq(-D,D,by=2)) +
    scale_y_continuous(name="Perturbation probability (p)", limits=ylimit, breaks=seq(0,1,by=0.2)) +
    theme_bw() + 
    theme(axis.text =element_text(size = 16), 
          axis.title = element_text(size = 18),
          plot.title = element_text(size = 18)) + 
      labs(title = attr(dFrame,"label"), 
           caption = paste("Timestamp: ",timestamp,"\nR-Package 'ptable' (Version ",packageVersion('ptable'),")",  sep="")) +
    theme(strip.background = element_rect(fill=cbPalette[3])) +
    geom_text(data = dt_meta,colour=cbPalette[dt_meta$col],
              aes(xl, y, label = paste0("p_mean = ",mw)), hjust = 0, vjust = 0.5,
              inherit.aes = FALSE) +
    geom_text(data = dt_meta, colour=cbPalette[dt_meta$col],
              aes(xl, y, label = paste0("p_var = ",var)), hjust = 0, vjust = 2,
              inherit.aes = FALSE) +
    geom_text(data = dt_meta,colour=cbPalette[dt_meta$col],
              aes(xr, y, label = paste0("p_stay = ",pstay)), hjust =1, vjust = 0.5,
              inherit.aes = FALSE) +
    geom_text(data = dt_meta,colour=cbPalette[dt_meta$col],
              aes(xr, y, label = paste0("p_sum ",ps)), hjust =1, vjust = 2,
              inherit.aes = FALSE) +
    geom_text(data = dt_meta, colour=cbPalette[dt_meta$col],
              aes(xr, y, label = paste0("iter = ",iter)), hjust =1, vjust = 3.5,
              inherit.aes = FALSE)
  
  
  #update(p, par.settings = list(par.sub.text = list(lineheight = 5)))
  if (!is.null(file)) {
    ggsave(filename=file, width=8, height=5)
    cat("graph saved to",shQuote(file),"\n")
  }
  return(output)
}