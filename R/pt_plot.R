#' @title Plot the results of the perturbation table generator
#'
#' @description [plot()] makes it easy to visualize the results of the created
#' ptable-object that has been created by [create_cnt_ptable()],
#' [create_cnt_ptable()] or [modify_cnt_ptable()].
#'
#' @param obj an object of class \code{\linkS4class{ptable}}
#' @param type (character) type of graph: distribution "d" (standard), 
#' perturbation panel ("p"), transition matrix "t"
#' @param file if not \code{NULL}, a path to a file (with file extension, e.g.
#' '.pdf' or '.png') where the graph is saved to
#' @param ... additional parameters passed to methods
#' @author Tobias Enderle
#' @keywords plot
#'
#' @return The selected graph is displayed, but there is no direct return 
#' value. The output could also be assigned to an object to receive an object 
#' of class `ggplot`.
#'
#' @examples
#'
#' # Create a ptable for frequency count tables and modify the intervals
#' ptab <- create_cnt_ptable(D = 3, V = 1.05, js = 1, label = "Example")
#' ptab_mod <- modify_cnt_ptable(ptab, threshold = 0.3, seed = 5432)
#'
#' # Distribution Plot of the Noise
#' plot(ptab_mod, type = "d")
#'
#' # Perturbations Panel of the Noise
#' plot(ptab_mod, type = "p")
#'
#' \donttest{
#' ## Plot and Save the Transition Matrix
#' plot(ptab_mod, type ="t", 
#'      file = tempfile("example_tMatrix", fileext = ".pdf"))
#' }
#'
#' @md
#' @rdname plot
#' @exportMethod plot
setGeneric("plot", function(obj,
                            type = "d",
                            file = NULL,
                            ...) {
  fifi_plot(obj, type = type, file = file, ...)
})
#' @title Plot the results of the perturbation table generator
#' @description Internal function
#' @noRd
fifi_plot <- function(obj, type = "d", file = NULL) {
  if (!is.null(file)) {
    stopifnot(is_scalar_character(file))
    
    if (tolower(file_ext(file)) == ""){
      stop("Please specify a file extension:", "
           '.pdf', '.png', '.jpeg' or '.tiff'.")
    }
    
    if (!(tolower(file_ext(file)) %in% c("pdf", "png", "jpeg", "tiff"))) {
      stop("Only 'pdf', 'png', 'jpeg' and 'tiff' are allowed", 
           "as file extension.")
    }
  }
  
  if (!(type %in% c("d", "p", "t")))
      stop("Only three options 'd', 'p' or 't' are allowed for input type.")
  
  if (type == "d") {
    # Distribution of Perturbation Values
    out <- pt_plot_pD(pert_table = obj, file = file)
  }
  if (type == "p") {
    # Perturbation Panel
    out <- pt_plot_pPanel(pert_table = obj, file = file)
  }
  if (type == "t") {
    # Transition Matrix
    out <- pt_plot_tMatrix(pert_table = obj, file = file)
  }
  
  return(out)
  
}

#' @title Distribution Plot
#' @description [pt_plot_pD()] plots the distribution of the perturbation 
#' values using the R-package \code{\link{ggplot2}}.
#' @param pert_table an object of class \code{\linkS4class{ptable}}
#' @param ylimit (numeric) vector with limits of y-axis (probabilities)
#' @param file if not \code{NULL}, a path to a file where the graph is saved 
#' to as pdf
#' @import ggplot2
#' @importFrom utils packageVersion
#' @noRd
pt_plot_pD <- function(pert_table,
                       ylimit = c(-0.05, 0.95),
                       file = NULL) {
  v <- check <- NULL
  . <- i <- j <- iter <- mw <- p <- ps <- pstay <- psum <- NULL
  var <- xl <- xr <- y <- NULL
  
  params <- slot(pert_table, "pParams")
  label_ <- slot(params, "label")
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
  
  dt_meta[, ps := ifelse(psum < 0.999999, 
                         "< 1", ifelse(psum > 1.000001, "> 1", "= 1"))][]
  dt_meta <- dt_meta[i > 0]
  
  cbPalette <-
    c(
      "#999999",
               "#E69F00",
               "#56B4E9",
               "#009E73",
               "#F0E442",
               "#0072B2",
               "#D55E00",
               "#CC79A7"
    )
  
  pTable <- copy(slot(pert_table, "pTable"))
  pTable[, check := TRUE]
  pTable <- pTable[, .(p = sum(p), v, check), by = .(i, j)]
  pTable[, i := as.factor(i)]
  pTable[, i := factor(i, levels = rev(levels(i)))]
  pTable[, i := as.numeric(as.vector(i))]
  
  output <-
    ggplot(data = pTable[i > 0 & check == TRUE],
           aes(x = as.integer(v), y = p)) +
    geom_point(group = 1,
               colour = cbPalette[6],
               size = 2) +
    geom_line(group = 1,
              colour = cbPalette[3],
              linewidth = 1) +
    facet_wrap(~ i, labeller = "label_both") +
    scale_x_continuous(
      name = "Noise (v)",
      limits = c(-D, D),
      breaks = seq(-D, D, by = 2)
    ) +
    scale_y_continuous(name = "Perturbation probability (p)",
                       limits = ylimit,
                       breaks = seq(0, 1, by = 0.2)) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 18),
      plot.title = element_text(size = 18)
    ) +
    labs(
      title = paste0("Distribution Plot: ", label_),
      caption = paste(
        "Timestamp: ",
        timestamp,
        "\nR-Package 'ptable' (Version ",
        packageVersion('ptable'),
        ")",
        sep = ""
      )
    ) +
    theme(strip.background = element_rect(fill = cbPalette[3])) +
    geom_text(
      data = dt_meta,
      colour = cbPalette[dt_meta$col],
      aes(xl, y, label = paste0("p_mean = ", mw)),
      hjust = 0,
      vjust = 0.5,
      inherit.aes = FALSE
    ) +
    geom_text(
      data = dt_meta,
      colour = cbPalette[dt_meta$col],
      aes(xl, y, label = paste0("p_var = ", var)),
      hjust = 0,
      vjust = 2,
      inherit.aes = FALSE
    ) +
    geom_text(
      data = dt_meta,
      colour = cbPalette[dt_meta$col],
      aes(xr, y, label = paste0("p_stay = ", pstay)),
      hjust = 1,
      vjust = 0.5,
      inherit.aes = FALSE
    ) +
    geom_text(
      data = dt_meta,
      colour = cbPalette[dt_meta$col],
      aes(xr, y, label = paste0("p_sum ", ps)),
      hjust = 1,
      vjust = 2,
      inherit.aes = FALSE
    ) +
    geom_text(
      data = dt_meta,
      colour = cbPalette[dt_meta$col],
      aes(xr, y, label = paste0("iter = ", iter)),
      hjust = 1,
      vjust = 3.5,
      inherit.aes = FALSE
    )
  
  #update(p, par.settings = list(par.sub.text = list(lineheight = 5)))
  if (!is.null(file)) {
    ggsave(filename = file,
           width = 8,
           height = 5)
    message("graph saved to", shQuote(file), "\n")
  }
  return(output)
}

#' @title Perturbation Panel Plot
#' @description [pt_plot_pPanel()] to plot the perturbation panel using the 
#' R-package \code{\link{ggplot2}}.
#' @param pert_table an object of class \code{\linkS4class{ptable}}
#' @param file if not \code{NULL}, a path to a file where the graph is saved 
#' to as pdf
#' @import ggplot2
#'
#' @noRd
pt_plot_pPanel <- function(pert_table, file = NULL) {
  i <- i_char <- j <- p <- u <- v <- p_int_lb <- NULL
  
  params <- slot(pert_table, "pParams")
  pTable <- copy(slot(pert_table, "pTable"))
  
  pTable <- pTable[order(v, p, decreasing = TRUE)]
  pTable <- pTable[, i_char := as.character(i)]
  pTable[, i_char := factor(i_char, levels = sort(unique(i)))]
  pTable[i == max(i), i_char := paste0("", i_char, "+")]

  lev_num <- (unique((pTable$v)))
  lev_sign <- sign(lev_num)
  lev_char <- (unique(as.character(pTable$v)))
  lev_char[lev_sign > 0] <- paste0("+", lev_char[lev_sign > 0])
  lev_char[lev_num == 0] <- "0 (no perturbation)"

  pTable[, u := factor(v, 
                       levels = (unique(as.character(v))), 
                       labels = lev_char)]
  pTable <- pTable[order(i, p_int_lb, decreasing = FALSE)]

  D <- slot(params, "D")
  intervals <- attr(pTable, "intervals")

  if (D > 6) {
    myBreaks <- c(-D, 0, D)
  } else {
    myBreaks <- c(-D:D)
  }

  # ggplot figure
  s <- ggplot(pTable, x = i, aes(i_char, p, fill = v))

  output <- s + geom_bar(stat = "identity", position = "fill") +
    coord_flip() +
    {
      if (D <= 6)
        guides(fill = guide_legend(
          title = "v (noise value):",
          title.position = "top",
          reverse = FALSE,
          size = 16
        ))
    } +
    scale_fill_gradient2(
      low = brewer.pal(9, "Greens")[8],
      mid = "grey95",
      high = brewer.pal(9, "Blues")[8],
      midpoint = 0,
      space = "Lab",
      na.value = "grey50",
      guide = "colourbar",
      aesthetics = "fill",
      breaks = myBreaks
    ) +
    labs(
      title = paste0("Perturbation Panel (", intervals, " intervals)"),
      y = "p (probability)",
      x = "i (original frequency)"
    ) +
    theme(
      plot.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 16),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 16),
      legend.position = "bottom",
      legend.box.background = element_rect(colour = "grey"),
      legend.background = element_blank(),
      panel.grid.major.x = element_line(colour = "lightgrey"),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_text(margin = margin(r = -15, l = 5))
    ) +
    ylim(0, 1)
  
  if (!is.null(file)) {
    ggsave(filename = file,
           width = 6,
           height = 5)
    message("graph of perturbation panel saved to", shQuote(file), "\n")
  }
  return(output)
  
}

#' @title Transition Matrix Plot
#' @description [pt_plot_tMatrix()] plots the transition matrix using the 
#' R-package \code{\link{ggplot2}}.
#' @param pert_table an object of class \code{\linkS4class{ptable}}
#' @param file if not \code{NULL}, a path to a file where the graph is saved 
#' to as pdf
#' @import ggplot2
#' @import RColorBrewer
#' @noRd
pt_plot_tMatrix <- function(pert_table, file = NULL) {
  . <- i <- j <- p <- NULL
  
  pTable <- slot(pert_table, "pTable")
  tTable <- copy(pTable[, .(i, j, p)])
  tTable <- tTable[, .(p = sum(p)), by = .(i, j)]
  tTable[, i := as.factor(i)]
  tTable[, j := as.factor(j)]
  tTable[, i := factor(i, levels = rev(levels(i)))]
  
  #  tMatrix <- slot(pert_table,"dFrame")
  #  tMatrix <- tMatrix[,c("i","j","p")]
  
  #tMatrix[, i:=factor(i)]
  #  tMatrix[, i:=factor(i, levels=rev(levels(i)))]
  
  # TODO: relative text sizes
  
  output <- ggplot(tTable, aes(j, i)) +
    geom_tile(aes(fill = p), color = "lightgrey") +
    labs(title = "Transition Matrix", y = "i (original frequency)", x =
           "j (target frequency)") +
    theme(legend.position = "bottom") +
    scale_fill_gradient(low = "white",
                        high = "red",
                        name = "probability") +
    geom_text(aes(label = round(p, 4)),  size = 4) +
    scale_x_discrete(position = "top") +
    #scale_y_discrete(limits=rev(levels(i))) +
    #geom_rect(data=..., size=1, fill=NA, colour="black",
    #          aes(xmin=j - 0.5, xmax=j + 0.5, ymin=i - 0.5, ymax=i + 0.5)) +
    theme(
      plot.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 16),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 16),
      legend.position = "right",
      legend.background = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_text(margin = margin(r = 0, l = 10), color =
                                   "black"),
                                 axis.text.x = element_text(color = "black")
    )
  
  if (!is.null(file)) {
    ggsave(filename = file,
           width = 8,
           height = 5)
    message("graph of transition matrix saved to", shQuote(file), "\n")
  }
  return(output)
}
