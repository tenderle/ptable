#' pt_create_pTable
#'
#' produces perturbation table that is needed to add noise to statistical frequency tables. The perturbation probabilities are constructed given the following constraints:
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
#' @param type (character) type of pTable, either 'destatis' (default) or 'abs' or 'abs2'
#' @param monitoring (logical) output monitoring on/off
#' @param debugging (logical) debug monitoring on/off
#'
#' @return an object of \code{\linkS4class{ptable}}
#'
#' @author Tobias Enderle, \email{tobias.enderle@@destatis.de}
#' @keywords perturbation table, sdc
#'
#' @examples
#' params_destatis <- pt_create_pParams(D=5, V=3, js=2, label="test")
#' pt_create_pTable(params=params_destatis, type="destatis")
#' 
#' params_abs <- pt_create_pParams(D=5, V=3, js=2, label="test", pTableSize=75)
#' pt_create_pTable(params=params_abs, type="abs")
#' 
#' @rdname pt_create_pTable
#' @export
#'
pt_create_pTable <-function(params, type="destatis", monitoring=FALSE, debugging=FALSE){
  . <- v <- p <- NULL
  pert_params <- params

  stopifnot(isS4(params))
  stopifnot(class(params)=="ptable_params")


  D <- slot(pert_params, "D")
  V <- slot(pert_params, "V")
  js <- slot(pert_params, "js")
  pstay <- slot(pert_params, "pstay")
  pstay[is.na(pstay)] <- 0
  
  mono <- slot(pert_params, "mono")
  epsilon <- slot(pert_params, "epsilon")

  optim <- slot(pert_params, "optim")
  ncat <- slot(pert_params, "ncat")
  label <- slot(pert_params, "label")
  
  pTableSize <- slot(pert_params, "pTableSize")

  # Blocking: target frequencies that are not allowed
  if (js == 0) blocking <- NULL
  else blocking <- c(1:js)

  # Initialize (empty) Perturbation Matrix
  Pinit <- Matrix <- fifi_matrix(D=D, ncat=ncat, pstay=pstay, blocking=blocking)

  # Number of rows = maximum frequency + 1
  nrows <- ncat+1


  # Grid for Live-Output (TODO: more flexible)
  if (ncat %in% c(3) ) par(mfrow=c(1,3))
  if (ncat %in% c(4) ) par(mfrow=c(2,2))
  if (ncat %in% c(5,6) ) par(mfrow=c(3,2))
  if (ncat %in% c(7,8) ) par(mfrow=c(2,4))
  if (ncat %in% c(9) ) par(mfrow=c(3,3))


  ITER <- numeric(nrows)


  for (i in (2:nrows))     # looping through all frequencies

  {

    if (monitoring)
    cat("\nRow", i, ": original frequency i =",i-1,"\n------------------------------- \n")

    # Define Vector with possible target frequencies
    j<-seq(max(i-1-D,0),i-1+D,by=1)

    if (debugging) cat("j1 ",j,"\n")

    # Remove blocked target frequencies
    j<-j[ !(j %in% blocking) ]
    if (debugging) cat("j2 ",j,"\n")

    # Derive current vector with deviations
    v_current<-j-(i-1)
    if (debugging) cat("v  ",v_current,"\n")

    # Initializing perturbation probabilities
    p_init <- Pinit[i,j+1]
    p_lb <- ifelse(p_init < epsilon[i-1] , epsilon[i-1], p_init) # lower bound: either initialized value or epsilon
    p_ub <- rep(1, length(v_current))


    if (i <= ncat+1)   { # (D+js+1) + 1

      check_var <- TRUE
      check_psum <- TRUE
      check_pstay <- TRUE
      iter <- 0

      while(check_pstay & check_var & (iter < 20) ) {

        if (iter > 0) {
          # If check_var='FALSE' then prevent a further WHILE-loop, otherwise reduce p_lb by 0.05 to allow smaller p_stay in order to fullfill the variance contraint
          p_lb[which(v_current==0)] <- p_lb[which(v_current==0)]-0.05
          p_lb <- ifelse(p_lb < epsilon[i-1] , epsilon[i-1], p_lb) # prevents negative values
        }
        
        iter <- iter + 1
        optout <- pt_optim_entropy(optim=optim[i-1],
                                 mono=mono[i-1],
                                 v=v_current,
                                 variance=V,
                                 #epsilon=epsilon[i-1],
                                 lb=p_lb,
                                 ub=p_ub)
        p_new <- optout$result

        # Check, whether variance constraints hold
        check_var <- fifi_check_p(p=p_new,v=v_current)$p_var != V
        #check_pstay <- !(is.null(pstay))

        if (debugging) {
          cat(paste("Variable ",i," - Iter ",iter,"\n",sep=""))
          cat(paste("check_var: ",check_var,"\n",sep=""))
          cat(paste("check_pstay: ",check_pstay,"\n",sep=""))
          #cat(paste("check_psum: ",check_psum,"\n",sep=""))
          cat(paste("p_lb for p_stay: ",p_lb[which(v_current==0)],"\n",sep=""))
        }

        if ((i-1) %in% blocking) check_var <- FALSE # if blocking, then prevent a further WHILE-loop

        
        
        chp <- fifi_check_p(p=p_new,v=v_current)$p_sum
        if (debugging) cat("Sum of p: ",chp%%1,"\n")
        if (chp != 1)
          stop("\nThe variance parameter you set is too small!")

      }

      #if (i > ncat) plot(v_current,p_new,type="b",main=paste("i>=",i-1," (symmetry)",sep=""), ylab="p", xlab="v")
      #else plot(v_current,p_new,type="b",main=paste("i=",i-1,sep=""), ylab="p", xlab="v")
    }

    Matrix[i,i+v_current] <- p_new
    ITER[i] <- iter


    test <- rbind(v=v_current, p_init=p_init, p_lb=p_lb, p_new=p_new)
    colnames(test) <- j
    if (monitoring){
      print(test)
      cat("\n")
    }

  }




  # Output Check: Constraints (1) Mean, (2) Variance and (5) sum of probabilities
  check <- cbind(fifi_check(P=Matrix, D=D, ncat=ncat), iter=as.integer(ITER))

  if (monitoring==TRUE){
    cat("\nPerturbation probabilities: \n---------------------------\n\n")
    print(round(Matrix,3))
    cat("\n")
    cat("Check of constraints (1), (2) and (5) plus further checks: \n----------------------------------------------------------\n\n")
    print(check)
    cat("\n")
  }


  Matrix[Matrix < 1.0e-7] <- 0
  DF <- fifi_df(probMat=Matrix, D=D, szenario=label, blocking=blocking, ncat=ncat)
  DF2 <- fifi_probframe(DF=DF, D=D)


  out <- new("ptable")
  attr(DF,"label") <- label
  slot(out,"dFrame") <- DF



  slot(out, "pMatrix") <- as.matrix(Matrix)
  slot(out, "pClasses") <- as.vector(c(0:(dim(Matrix)[1]-1)) )

  #type <- slot(pert_params, "type")
  type <- tolower(type)
  stopifnot(type %in% c("abs","destatis", "abs2"))

  if (type=="abs"){

    # Perturbation Table for cellKey(): type="abs"
    #pTableSize <- 75
    nrows <- 256

    dt <- as.data.table(DF2)
    p_values_small <- dt[i>0, .(sample(v, nrows, prob=p, replace=TRUE)), i]$V1
    p_values_symmetry <- dt[i==ncat, .(sample(v, nrows*(pTableSize-ncat), prob=p, replace=TRUE)), i]$V1
    dt <- as.data.table(matrix(c(p_values_small, p_values_symmetry), nrow=nrows))

    setattr(dt, "type", type)
    slot(out, "pTable") <- dt

  }
  if (type=="abs2"){
    
    # TODO: add optimization
    # Perturbation Table for cellKey(): type="abs2"
    #pTableSize <- 75
    nrows <- 256
    
    dt <- as.data.table(DF2)
    p_values_small <- dt[i > 0,.(fifi_allocate(v=v, p=p, nrows = nrows) ),i]$V1
    p_values_symmetry <- dt[i == ncat,.(fifi_allocate(v=v, p=p, nrows = nrows ) ),i]$V1
    # TODO: in cae of symmetry: sampling must be repeated instead of replicating the vector
    dt <- as.data.table(matrix(c(p_values_small, rep(p_values_symmetry, (pTableSize-ncat) )), nrow=nrows))
    
    setattr(dt, "type", type)
    slot(out, "pTable") <- dt
    
  }
  if (type=="destatis"){
    setattr(DF2, "type", type)
    slot(out, "pTable") <- as.data.table(DF2)
  }

  slot(out, "pParams") <- pert_params
  slot(out, "empResults") <- check

  slot(out, "tStamp") <- format(Sys.time(), "%Y%m%d%H%M%S")
  slot(out, "type") <- type
  
  validObject(out)
  out
}


#dimT <- dim(erg@pTable)[1]
#library(data.table)
#as.data.table(with(erg@pTable, tapply(1:dimT, i, function(x) sample(pert[x], nrows, prob=p[x],replace=TRUE) ) ))

#p_values_small <- erg@pTable[i>0, .(sample(pert, nrows, prob=p, replace=TRUE)), i]$V1
#p_values_symmetry <- erg@pTable[i==4, .(sample(pert, nrows*(75-4), prob=p, replace=TRUE)), i]$V1

#p_values <- as.data.table(matrix(c(p_values_small, p_values_symmetry), nrow=nrows))

#erg@pTable[, .(sum(p)), i]


