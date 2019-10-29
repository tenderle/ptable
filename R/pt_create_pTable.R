#' pt_create_pTable
#'
#' produces perturbation table that is needed to add noise to statistical frequency or magnitude tables. The perturbation probabilities are constructed given the following constraints:
#' - Unbiasedness of the noise
#' - Fixed noise variance
#' - Transition probabilities are between zero and one and the sum up to 1
#' - Perturbations will not produce negaive cell values or positive cell values equal to or less than a specific threshold value 
#' - The absolute value of any perturbation is less than a specific integer value (i.e. the maxiumum noise)
#' 
#' For more information, see the vignette using \code{pt_vignette()}.
#' @md
#'
#' @param params an object of class \code{\linkS4class{ptable_params}}
#' generated with \code{\link{pt_create_pParams}}
#' @param monitoring (logical) output monitoring on/off
#' @param debugging (logical) debug monitoring on/off
#'
#' @return an object of \code{\linkS4class{ptable}}
#'
#' @author Tobias Enderle, \email{tobias.enderle@@destatis.de}
#' @keywords perturbation table, sdc
#'
#' @examples
#' # ptable for frequency tables
#' params_cnts <- pt_create_pParams(D=5, V=3, js=2, label="test")
#' pt_create_pTable(params=params_cnts)
#' 
#' # ptable for magnitude tables
#' params_nums <- pt_create_pParams(D=5, V=2, table="nums", step=4, icat=c(1,3,5))
#' pt_create_pTable(params=params_nums)
#' 
#' @rdname pt_create_pTable
#' @export
#'
pt_create_pTable <-function(params, monitoring=FALSE, debugging=FALSE){
  . <- v <- p <- NULL
  p_int_ub <- p_int_lb <- i_info <- type <- symmetry <- NULL
  pert_params <- params
  
  stopifnot(isS4(params))
  stopifnot(class(params)=="ptable_params")
  
  
  D <- slot(pert_params, "D")
  V <- slot(pert_params, "V")
  js <- slot(pert_params, "js")
  pstay <- slot(pert_params, "pstay")
  pstay[is.na(pstay)] <- 0
  
  step <- slot(pert_params, "step")
  stepwidth <- 1/step
  table <- slot(pert_params, "table")
  ttype <-  slot(pert_params, "type")
  
  mono <- slot(pert_params, "mono")
  optim <- slot(pert_params, "optim")
  ncat <- slot(pert_params, "ncat")
  label <- slot(pert_params, "label")
  
  icat <- slot(pert_params, "icat")
  icat_ <- c(0, icat)
  
  # Blocking: target frequencies that are not allowed
  if (js == 0) blocking <- NULL
  else blocking <- c(1:js)
  
  # Number of rows = maximum frequency + 1
  if (table=="cnts") nrows <- ncat+1
  if (table=="nums") nrows <- length(icat_)
  
  RESULT <- vector("list", length=nrows)
  
  # set result manually for 'i=0'
  RESULT[[1]]$i <- 0
  RESULT[[1]]$iter <- 0
  RESULT[[1]]$dt <- data.table(i=0, j=0, v=0, p_init=1, p_lb=1,p_ub=1, p=1, p_int_ub=1, p_int_lb=0)
  
  ndigits <- 8
  epsilon <- 1/10^ndigits
  options(digits=ndigits,scipen=ndigits)
  
  for (r in 2:nrows)     # looping through all frequencies categories (i.e. icat's)
    
  {
    
    i <- icat_[r]
    
    
    if (monitoring){
      cat("\nRow", r, ": original frequency i =",i,"\n------------------------------- \n")
    }
    
    # Define Vector with possible target frequencies
    j<-seq(max(i-D,0),i+D,by=stepwidth)
    if (step!=1) j<- round(j,4)
    
    if (debugging) cat("j1 ",j,"\n")
    
    # Remove blocked target frequencies
    j<-j[ !(j %in% blocking) ]
    if (debugging) cat("j2 ",j,"\n")
    
    
    # Derive current vector with deviations
    v_current<-j-i
    if (step!=1) v_current<- round(v_current,4)
    if (debugging) {
      cat("v  ",v_current,"\n")
      cat("i_char  ",as.character(i),"\n")
      cat("j_char  ",as.character(j),"\n")
      cat("v_char  ",as.character(v_current),"\n")
    }
    
    # Initializing perturbation probabilities
    p_init <- rep(0,length(j))
    p_init[which(i==j)] <- pstay[r-1]
    p_lb <- ifelse(p_init < epsilon , epsilon, p_init) # lower bound: either initialized value or epsilon
    p_ub <- rep(1, length(v_current))
    if (debugging) {
      cat("i ",i,"\n")
      cat("j ",j,"\n")
      cat("p_init ",p_init,"\n")
      cat("p_lb ",p_lb,"\n")
      cat("p_ub ",p_ub,"\n")
      cat("\n")
      cat("pstay ",pstay,"\n")
      cat("epsilon ",epsilon,"\n")
    }
    
    if (r <= ncat+1)   { # (D+js+1) + 1
      
      check_var <- TRUE
      check_psum <- TRUE
      check_pstay <- TRUE
      iter <- 0
      
      while(check_pstay & check_var & (iter < 20) ) {
        
        if (iter > 0) {
          # If check_var=='FALSE' then prevent a further WHILE-loop, otherwise reduce p_lb by 0.05 to allow smaller p_stay in order to fullfill the variance contraint
          p_lb[which(v_current==0)] <- p_lb[which(v_current==0)]-0.05
          p_lb <- ifelse(p_lb < epsilon , epsilon, p_lb) # prevents negative values
        }
        
        iter <- iter + 1
        optout <- pt_optim_entropy(optim=optim[r-1],
                                   mono=mono[r-1],
                                   v=v_current,
                                   variance=V,
                                   lb=p_lb,
                                   ub=p_ub,
                                   ndigits=7)
        
        p_new <- optout$result
        #p_new <- round(p_new, digits=ndigits)
        if (debugging) cat("p_new:" , p_new,"\n")
        
        # Check, whether variance constraints hold
        check_iteration <- fifi_check_p(p=p_new,v=v_current)
        if (debugging) print(check_iteration)
        
        # Check Variance
        check_var <- check_iteration$p_var != V
        #check_pstay <- !(is.null(pstay))
        
        if (debugging) {
          cat(paste("Variable ",i," - Iter ",iter,"\n",sep=""))
          cat(paste("check_var: ",check_var,"\n",sep=""))
          cat(paste("check_pstay: ",check_pstay,"\n",sep=""))
          #cat(paste("check_psum: ",check_psum,"\n",sep=""))
          cat(paste("p_lb for p_stay: ",p_lb[which(v_current==0)],"\n",sep=""))
        }
        
        if ((i) %in% blocking) check_var <- FALSE # if blocking, then prevent a further WHILE-loop
        
        
        
        chp <- check_iteration$p_sum
        if (debugging) cat("Sum of p: ",chp%%1,"\n")
        if (chp != 1)
          stop("\nThe variance parameter you set is too small!")
        
      }
      
    }
    
    # Monitoring output of current iteration
    if (FALSE){
      test <- rbind(v=v_current, p_init=p_init, p_lb=p_lb, p_new=p_new)
      colnames(test) <- j
      cat("Current iteration ",iter," for i=",i,":\n")
      print(test)
      cat("\n")
    }
    
    dt <- data.table(i=i, j=j, v=v_current, p_init=p_init, p_lb=p_lb,p_ub=p_ub, p=p_new)
    #dt[, p:=round(p,digits = ndigits)]
    dt[, p_int_ub := cumsum(p), by=list(i)]
    dt[, p_int_lb := p_int_ub-p]
    

    # Alternative monitoring output of current iteration
    if (monitoring){
      cat("Current iteration ",iter," for i=",i,":\n")
      print(dt)
      cat("\n")
    }
    
    RESULT[[r]]$i <- i
    RESULT[[r]]$iter <- iter
    RESULT[[r]]$dt <- dt
    
  }
  
  
  #pSymmetry <- (2*(D/step))+1
  pSymmetry <- (2*(D*step))+1
  
  
  # Perturbation Matrix
  erg_dt <- do.call(rbind, sapply(RESULT, '[', 'dt'))
  erg_dt[,i_info:=paste("i=",i,sep=""),]
  if (table=="cnts")
    erg_dt[i==max(as.integer(as.character(i))),i_info:=paste("i>=",i," (symmtery)",sep="")]
  
  erg_dt[, symmetry:=.N, by = .(i)]
  if (table=="nums")
    erg_dt[symmetry==pSymmetry,i_info:=paste("i=",i," (symmtery)",sep="")]
  if (debugging) print(erg_dt)
  
  
  # Transition Matrix
  dt2mat <- dcast(erg_dt, i~j, value.var = "p", fill=0)
  Matrix <- as.matrix(dt2mat[,-1])
  rownames(Matrix) <- as.character(dt2mat$i)
  if (debugging) print(Matrix)
  
  
  # Output Check: Constraints (1) Mean, (2) Variance and (5) sum of probabilities
  check <- cbind(fifi_check_pTable(DT=erg_dt), 
                 iter=as.integer(do.call(rbind, sapply(RESULT, '[', 'iter'))) )
  
  if (monitoring==TRUE){
    cat("\nPerturbation probabilities (in %): \n----------------------------------\n\n")
    print(round(Matrix*100,2))
    cat("\n")
    cat("\nCheck of constraints (1), (2) and (5) plus further checks: \n----------------------------------------------------------\n\n")
    print(check)
    cat("\n")
  }
  
  
  #Matrix[Matrix < 1.0e-7] <- 0
  
  
  # Perturbation Table with additional information
  DF <- copy(erg_dt)
  DF[,check:=TRUE]
  DF[,i:=as.factor(i)]
  DF[,j:=as.factor(j)]
  DF[, type:=ttype]
  DF[,p_int_lb:=round(p_int_lb, ndigits)]
  DF[,p_int_ub:=round(p_int_ub, ndigits)]
  # IMPORTANT step: Due to rounding errors, 'p' is replaced by the differences of the rounded intervals
  DF[, p := p_int_ub-p_int_lb ]
  
  # Perturbation Table/Matrix
  pTable <- copy(erg_dt)
  pTable[, type:=ttype]
  pTable[,p_int_lb:=round(p_int_lb, ndigits)]
  pTable[,p_int_ub:=round(p_int_ub, ndigits)]
  # IMPORTANT step: Due to rounding errors, 'p' is replaced by the differences of the rounded intervals
  pTable[, p := p_int_ub-p_int_lb ]
  pTable <- pTable[,.(i,j,p,v,p_int_lb,p_int_ub, type)]
  
  
  # Ouput
  out <- new("ptable")
  attr(DF,"label") <- label
  
  slot(out,"dFrame") <- DF
  slot(out, "pTable") <- pTable
  slot(out, "pMatrix") <- as.matrix(Matrix)
  
  slot(out, "pClasses") <- icat_
  slot(out, "pParams") <- pert_params
  slot(out, "empResults") <- check
  
  slot(out, "tStamp") <- format(Sys.time(), "%Y%m%d%H%M%S")
  slot(out, "type") <- ttype
  slot(out, "table") <- table
  
  validObject(out)
  out
}



