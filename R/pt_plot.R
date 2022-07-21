#' @title pt_plot
#'
#' @description Function to visualize the perturbation table.
#'
#' @param obj an object of class \code{\linkS4class{ptable}}
#' @param type (character) type of graph: distribution "d" (standard), perturbation panel ("p"), transition matrix "t"
#' @param file if not \code{NULL}, a path to a file where the graph is saved to as pdf
#' 
#' @author Tobias Enderle
#' @keywords plot
#'
#' @examples
#' 
#' ptab <- create_cnt_ptable(D = 5, V = 2, label = "Example")
#' 
#' # Distribution Plot of the Noise
#' plot(ptab, type = "d")
#' 
#' # Perturbations Panel of the Noise
#' plot(ptab, type = "p")
#' 
#' # Transition Matrix
#' plot(ptab, type = "t")
#' 
#' \dontrun{
#' ## Export result
#' plot(ptab, type ="t", file = "example_tMatrix.pdf")
#' }
#' 
#' @md
#' @rdname pt_plot
#' 
fifi_plot <- function(obj, type="d", file=NULL){
  
  if (!is.null(file)) {
    stopifnot(is_scalar_character(file))
  }
  
  if (type == "d") {
    cat("Distribution of Perturbation Values\n")
    out <- pt_plot_pD(pert_table=obj, file=file)
  }
  if (type == "p") {
    cat("Perturbation Panel\n")
    out <- pt_plot_pPanel(pert_table=obj, file=file)
  }
  if (type == "t") {
    cat("Transition Matrix\n")
    out <- pt_plot_tMatrix(pert_table=obj, file=file)
  }
  
  return(out)

}

#' @title pt_plot_pD
#' @description Function to plot the ditribution of the perturbation values using the R-package \code{\link{ggplot2}}.
#' @param pert_table an object of class \code{\linkS4class{ptable}}
#' @param ylimit (numeric) vector with limits of y-axis (probabilities)
#' @param file if not \code{NULL}, a path to a file where the graph is saved to as pdf
#' @import ggplot2
#' @importFrom utils packageVersion
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

#' @title pt_plot_pPanel
#' @description Function to plot the perturbation panel using the R-package \code{\link{ggplot2}}.
#' @param pert_table an object of class \code{\linkS4class{ptable}}
#' @param file if not \code{NULL}, a path to a file where the graph is saved to as pdf
#' @import ggplot2
#' @importFrom grDevices colorRampPalette
#' @importFrom stats aggregate
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
  step <- slot(params, "step")
  D0 <- (D*step)+1
  
  # Colors for perturbation values
  if (D0 <= 9){
    pert_no <- brewer.pal(D0, "Greys")[2]
    pert_pos <- brewer.pal(D0, "Blues")[c(2:D0)]
    pert_neg <- brewer.pal(D0, "Greens")[c(D0:2)]
  } else {
    getPalette <- colorRampPalette(c(brewer.pal(9, "Greens")[c(D0:1)], 
                                     brewer.pal(9,"Greys")[2],
                                     brewer.pal(9, "Blues")[c(1:D0)]
    ))
  }
  
  # ggplot figure
  s <- ggplot(pTable, x=i, y=p, aes(i_char,p, fill = u ))
  
  if (D0 <= 9){
    output <- s + geom_bar(stat="identity", position = "fill") + 
      coord_flip() + 
      guides(fill= guide_legend(title="v (perturbation value):", title.position = "top", reverse=TRUE, size=16))+
      scale_fill_manual(values=c(pert_neg,pert_no,pert_pos)) +
      #scale_fill_manual(values=getPalette((2*D/step)+1)) +
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
  } else {
    output <- s + geom_bar(stat="identity", position = "fill") + 
      coord_flip() + 
      guides(fill= guide_legend(title="v (perturbation value):", title.position = "top", reverse=TRUE, size=16))+
      #scale_fill_manual(values=c(pert_neg,pert_no,pert_pos)) +
      scale_fill_manual(values=getPalette((2*D*step)+1)) +
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
    
  }
  if (!is.null(file)) {
    ggsave(filename=file, width=6, height=5)
    cat("graph of perturbation panel saved to",shQuote(file),"\n")
  }
  return(output)
  
}

#' @title pt_plot_tMatrix
#' @description Function to plot the transition matrix using the R-package \code{\link{ggplot2}}.
#' @param pert_table an object of class \code{\linkS4class{ptable}}
#' @param file if not \code{NULL}, a path to a file where the graph is saved to as pdf
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
  
  #tMatrix[, i:=factor(i)]
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