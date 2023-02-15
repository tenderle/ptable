#' @title Create a perturbation table
#'
#' @description [pt_create_pTable()] produces a perturbation table that is needed to 
#' add noise to statistical frequency tables. The perturbation probabilities 
#' are constructed given the following constraints:
#' - Unbiasedness of the noise
#' - Fixed noise variance
#' - Transition probabilities are between zero and one and the sum up to 1
#' - Perturbations will not produce negative cell values or positive cell values 
#' equal to or less than a specific threshold value 
#' - The absolute value of any perturbation is less than a specific integer 
#' value (i.e. the maximum noise)
#' 
#' @details Please note that [create_cnt_ptable()] and [create_num_ptable()] 
#' provide direct wrappers to create perturbation tables for count- and numeric
#' variables. For more information, see the vignette using [pt_vignette()].
#' @md
#' @seealso [create_cnt_ptable()], [create_num_ptable()]
#' @param params a `list` or an object generated with [pt_create_pParams()]. In
#' case a simple list is provided, the named list-arguments are expected to be
#' the same as those documented in [pt_create_pParams()].
#' @param monitoring (logical) output monitoring on/off
#' @param debugging (logical) debug monitoring on/off
#'
#' @return an object of [ptable-class]
#'
#' @author Tobias Enderle, \email{tobias.enderle@@destatis.de}
#' @keywords perturbation table sdc
#'
#' @examples
#' # ptable for frequency tables 
#' # old way
#' \dontrun{
#' params_cnts <- pt_create_pParams(D = 5, V = 3, js = 2, label = "test1")
#' pt_create_pTable(params = params_cnts)
#' }
#' # new, direct way
#' create_cnt_ptable(D = 5, V = 3, js = 2, label = "test2")
#' 
#' # ptable for magnitude tables
#' # old way
#' \dontrun{ 
#' params_nums <- pt_create_pParams(D=5, V=2, table="nums", step=4, icat=c(1, 3, 5))
#' pt_create_pTable(params = params_nums)
#' }
#' # new way
#' create_num_ptable(D = 5, V = 2, step = 4, icat = c(1, 3, 5))
#' @noRd
pt_create_pTable <- function(params, monitoring = FALSE, debugging = FALSE) {
  . <- v <- p <- NULL
  p_int_ub <- p_int_lb <- i_info <- type <- symmetry <- NULL
  pert_params <- params
  
  stopifnot(isS4(params))
  stopifnot(class(params) == "ptable_params")
  
  D <- slot(pert_params, "D")
  V <- slot(pert_params, "V")
  js <- slot(pert_params, "js")
  pstay <- slot(pert_params, "pstay")
  pstay[is.na(pstay)] <- 0
  
  step <- slot(pert_params, "step")
  stepwidth <- 1 / step
  table <- slot(pert_params, "table")
  ttype <-  slot(pert_params, "type")
  
  mono <- slot(pert_params, "mono")
  optim <- slot(pert_params, "optim")
  ncat <- slot(pert_params, "ncat")
  label <- slot(pert_params, "label")
  
  icat <- slot(pert_params, "icat")
  icat_ <- c(0, icat)
  
  # Blocking: target frequencies that are not allowed
  if (js == 0) {
    blocking <- NULL
  } else {
    blocking <- c(1:js)
  }
  
  # Number of rows = maximum frequency + 1
  if (table == "cnts") {
    nrows <- ncat + 1
  }
  if (table == "nums") {
    nrows <- length(icat_)
  }
  
  RESULT <- vector("list", length = nrows)
  
  # set result manually for 'i=0'
  RESULT[[1]]$i <- 0
  RESULT[[1]]$iter <- 0
  RESULT[[1]]$dt <- data.table(
    i = 0,
    j = 0,
    v = 0,
    p_init = 1,
    p_lb = 1,
    p_ub = 1,
    p = 1,
    p_int_ub = 1,
    p_int_lb = 0)
  
  ndigits <- 8
  epsilon <- 1 / 10 ^ ndigits
  options(digits = ndigits, scipen = ndigits)
  
  for (r in 2:nrows) {    # looping through all frequencies categories (i.e. icat's)
    i <- icat_[r]
    
    if (monitoring) {
      message("\nRow ", r, ": original frequency i = ", i)
      message("-------------------------------")
    }
    
    # Define Vector with possible target frequencies
    j <- seq(max(i - D, 0), i + D, by = stepwidth)
    if (step != 1) {
      j <- round(j, 4)
    }
    if (debugging) {
      message("j1 ", j)
    }
    
    # Remove blocked target frequencies
    j <- j[!(j %in% blocking)]
    if (debugging) {
      message("j2 ", j)
    }
    
    # Derive current vector with deviations
    v_current <- j - i
    if (step != 1) {
      v_current <- round(v_current, 4)
    }
    if (debugging) {
      message("v  ", v_current)
      message("i_char  ", as.character(i))
      message("j_char  ", as.character(j))
      message("v_char  ", as.character(v_current))
    }
    
    # Initializing perturbation probabilities
    p_init <- rep(0, length(j))
    p_init[which(i == j)] <- pstay[r - 1]
    # lower bound: either initialized value or epsilon
    p_lb <- ifelse(p_init < epsilon , epsilon, p_init)
    p_ub <- rep(1, length(v_current))
    if (debugging) {
      message("i ", i)
      message("j ", j)
      message("p_init ", p_init)
      message("p_lb ", p_lb)
      message("p_ub ", p_ub)
      message("")
      message("pstay ", pstay)
      message("epsilon ", epsilon)
    }
    
    if (r <= ncat + 1) {
      # (D+js+1) + 1
      check_var <- TRUE
      check_psum <- TRUE
      check_pstay <- TRUE
      iter <- 0
      
      while (check_pstay & check_var & (iter < 20)) {
        if (iter > 0) {
          # If check_var=='FALSE' then prevent a further WHILE-loop, 
          # otherwise reduce p_lb by 0.05 to allow smaller p_stay in order to 
          # fulfill the variance constraint
          p_lb[which(v_current == 0)] <- p_lb[which(v_current == 0)] - 0.05
          p_lb <- ifelse(p_lb < epsilon , epsilon, p_lb) # prevents negative values
        }
        
        iter <- iter + 1
        optout <- pt_optim_entropy(
          optim = optim[r - 1],
          mono = mono[r - 1],
          v = v_current,
          variance = V,
          lb = p_lb,
          ub = p_ub,
          ndigits = 7)
        
        p_new <- optout$result
        if (debugging) {
          message("p_new:" , p_new)
        }
        
        # Check, whether variance constraints hold
        check_iteration <- fifi_check_p(p = p_new, v = v_current)
        if (debugging) {
          print(check_iteration)
        }
        
        # Check Variance
        check_var <- check_iteration$p_var != V
        if (debugging) {
          message("Variable ", i, " - Iter ", iter)
          message("check_var: ", check_var)
          message("check_pstay: ", check_pstay)
          message("p_lb for p_stay: ", p_lb[which(v_current == 0)])
        }
        
        if ((i) %in% blocking) {
          check_var <- FALSE # if blocking, then prevent a further WHILE-loop
        }
        
        chp <- check_iteration$p_sum
        if (debugging) {
          message("Sum of p: ", chp %% 1)
        }
        if (chp != 1) {
          stop(paste0("The ptable can't be calculated without a violation of the constraints. The combination of the input parameters you set (e.g. D=",D,", V=",V,", js=",js," or pstay) doesn't work. Please try another specification: either change the arguments 'mono=' or 'optim=' or try to use a different combination of input parameters (hint: changing the variance is sufficient in most cases)."), call. = FALSE)
        }
      }
    }
    
    # Monitoring output of current iteration
    # if (FALSE) { #FALSE
    #   test <- rbind(
    #     v = v_current,
    #     p_init = p_init,
    #     p_lb = p_lb,
    #     p_new = p_new)
    #   colnames(test) <- j
    #   message("Current iteration ", iter, " for i=", i, ":")
    #   print(test)
    #   message("")
    # }
    
    dt <- data.table(
      i = i,
      j = j,
      v = v_current,
      p_init = p_init,
      p_lb = p_lb,
      p_ub = p_ub,
      p = p_new)
    dt[, p_int_ub := cumsum(p), by = list(i)]
    dt[, p_int_lb := p_int_ub - p]
    
    # Alternative monitoring output of current iteration
    if (monitoring){
      message("Current iteration ",iter," for i=",i,":")
      print(dt)
      message("")
    }
    
    RESULT[[r]]$i <- i
    RESULT[[r]]$iter <- iter
    RESULT[[r]]$dt <- dt
  }
  
  pSymmetry <- (2 * (D * step)) + 1
  
  # Perturbation Matrix
  erg_dt <- do.call(rbind, sapply(RESULT, '[', 'dt'))
  erg_dt[, i_info := paste("i=", i, sep = ""), ]
  if (table == "cnts") {
    erg_dt[i == max(as.integer(as.character(i))),  i_info := paste0("i>=", i, " (symmtery)")]
  }
  
  erg_dt[, symmetry := .N, by = .(i)]
  if (table == "nums") {
    erg_dt[symmetry == pSymmetry, i_info := paste0("i=", i, " (symmtery)")]
  }
  if (debugging) {
    print(erg_dt)
  }
  
  # Transition Matrix
  dt2mat <- dcast(erg_dt, i ~ j, value.var = "p", fill = 0)
  Matrix <- as.matrix(dt2mat[, -1])
  rownames(Matrix) <- as.character(dt2mat$i)
  if (debugging) {
    print(Matrix)
  }
  
  # Output Check: Constraints (1) Mean, (2) Variance and (5) sum of probabilities
  check <- cbind(
    fifi_check_pTable(DT = erg_dt),
    iter = as.integer(do.call(rbind, sapply(RESULT, '[', 'iter'))))
  
  if (monitoring) {
    message("")
    message("Perturbation probabilities (in %):")
    message("----------------------------------")
    message("")
    print(round(Matrix * 100, 2))
    message("")
    message("Check of constraints (1), (2) and (5) plus further checks:")
    message("----------------------------------------------------------")
    message("")
    print(check)
    message("")
  }
  
  # Perturbation Table/Matrix
  pTable <- copy(erg_dt)
  pTable[, type := ttype]
  pTable[, p_int_lb := round(p_int_lb, ndigits)]
  pTable[, p_int_ub := round(p_int_ub, ndigits)]
  # IMPORTANT step: Due to rounding errors, 'p' is replaced by the differences of the rounded intervals
  pTable[, p := p_int_ub - p_int_lb]
  pTable <- pTable[, .(i, j, p, v, p_int_lb, p_int_ub, type)]
  
  # Ouput
  out <- new("ptable")
  attr(pTable, "intervals") <- "default"
  
  slot(out, "pTable") <- pTable
  slot(out, "tMatrix") <- as.matrix(Matrix)
  
  slot(out, "pClasses") <- icat_
  slot(out, "pParams") <- pert_params
  slot(out, "empResults") <- check
  
  slot(out, "tStamp") <- format(Sys.time(), "%Y%m%d%H%M%S")
  slot(out, "type") <- ttype
  slot(out, "table") <- table
  
  validObject(out)
  out
}