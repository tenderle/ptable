#' pt_plot
#'
#' Function to plot the perturbation table using the R-package \code{\link{lattice}}.
#'
#' @param pert_table an object of class \code{\linkS4class{ptable}}
#' @param ylimit (numeric) vector with limits of y-axis (for probabilities)
#'
#'
#' @author Tobias Enderle
#' @keywords plot
#'
#' @examples
#' # Simple Example
#' params <- pt_create_pParams(D=5, V=2, js=2, label="Example")
#' ptable_destatis <- pt_create_pTable(params=params, type="destatis")
#' fifi_plot(ptable_destatis)
#'
#' # Export result
#' pdf(paste("graph.pdf", sep=""))
#'   print(fifi_plot(ptable_destatis))
#' dev.off()
#'
#' @rdname pt_plot
#' @import lattice
#' @import RColorBrewer
#'



fifi_plot <- function(pert_table, ylimit=c(-0.05,0.95)){
  v <- check <- i_info <- NULL
  params <- slot(pert_table, "pParams")
  timestamp <- slot(pert_table, "tStamp")

  empResults <- slot(pert_table, "empResults")

  M <- empResults$p_mean
  V <- empResults$p_var
  BW <- round(empResults$p_stay, 2)
  SUM_W <- empResults$p_sum
  ITER <- empResults$iter

  D <- slot(params, "D")
  VARIANZ <- slot(params, "V")


  mypanel_zwo<-function(x,y,...){

    panel.abline(v = seq(-D,D, by=2), lty = "solid", lwd=0.25, col = "light grey")
    panel.abline(v = 0, lty = "solid", col = "white")
    panel.abline(v = 0, lty = "dashed", lwd=0.5, col = "light grey")

    # Important: +1 in order to start with i=1
    m <- format(M[panel.number()+1], nsmall=3)
    v <- format(V[panel.number()+1], nsmall=3)
    bw <- format(round(BW[panel.number()+1],2), nsmall=2)
    sv <- SUM_W[panel.number()+1]
    iter <- ITER[panel.number()+1]


    if(sv < 0.999999) sv <- "< 1"
    else {
      if(sv > 1.000001) sv <- "> 1"
      else sv = "= 1"
    }

    highlightColor <- "black"
    if (VARIANZ != as.numeric(v)) highlightColor <- "red"

    panel.text(-(D-(0.3*D)),0.8, paste("M = ",m,sep=""), cex=0.7)
    panel.text(-(D-(0.3*D)),0.7, paste("V = ",v,sep=""), cex=0.7, col=highlightColor)
    panel.text((D-(0.3*D)),0.8, paste("p_stay = ",bw,sep=""), cex=0.7)
    panel.text((D-(0.3*D)),0.7, paste("p_sum ",sv,sep=""), cex=0.7)
    panel.text((D-(0.3*D)),0.6, paste("iter = ",iter,sep=""), cex=0.7)

    panel.xyplot(x, y, type="l", lwd=c(1.5),...)
    panel.xyplot(x, y, type="p", pch=16,...)
  }

  # Settings:
  myColours <- brewer.pal(6,"Blues")
  my.settings <- list(
    superpose.polygon=list(col=myColours[2:5], border="transparent"),
    strip.background=list(col=myColours[5]),
    strip.border=list(col="black"),
    par.sub.text = list(cex = 0.5, font=1, just="right", x = grid::unit(0.95, "npc"))
  )

  dFrame <- slot(pert_table,"dFrame")
  type <- slot(pert_table,"type")

  if(!type %in% c("destatis")) {
    stop("type must be 'destatis'\n")
  }

  subFrame <- dFrame[v %in% seq(-5,5,by=1) & check == TRUE & i_info!="i=0",,]
  p <- xyplot(p ~ v | i_info, data=subFrame, ylim=ylimit,
              xlab="v (=perturbation value)", ylab="p (=perturbation probability)",
              par.settings = my.settings,
              par.strip.text=list(col="white", font=1.5),
              panel=mypanel_zwo,
              main=attr(dFrame,"label"),
              sub=paste("Timestamp: ",timestamp,"\nR-Package 'ptable' (Version 0.1.0)",  sep=""))

  #update(p, par.settings = list(par.sub.text = list(lineheight = 5)))

    return(p)

}
