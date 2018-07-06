
# initializing perturbation matrix
# fifi_matrix is used in ptable()
fifi_matrix <- function(ncat, D, pstay=NULL, blocking=blocking){

  nrows <- ncat + 1
  ncols <- nrows+D

  # probabilities for no perturbation
  Matrix<-diag(pstay,nrows,ncols)

  # blocked target frequencies
  Matrix[,blocking+1] <- 0

  # Zero retains zero
  Matrix[1,1]<-1

  colnames(Matrix) <-  0:(ncols-1)
  rownames(Matrix) <- 0:ncat

  return(Matrix)
}


# ================================================ #
# Check of conditions #
# ================================================ #

fifi_check <- function(P, D=D, ncat=ncat){

  devMat <- fifi_devMat(D=D, ncat=ncat)

  nrow <- nrow(P)
  ncol <- nrow+D

  cat.i <- (nrow-1)
  cat.j <- (ncol-1)

  constr1 <- sapply(1:nrow, function(row) P[row,] %*%  devMat[row,] )
  constr2 <- sapply(1:nrow, function(row) P[row,] %*%  (devMat[row,])^2 )
  constr5 <- rowSums(P)

  p_stay=diag(P)

  out <- data.table(i=c(0:cat.i), p_mean=round(constr1,3), p_var=round(constr2,3), p_sum=round(constr5,10), p_stay=p_stay)
  #rownames(out) <- c(0:cat.i)

  return(out)

}


fifi_check_p <- function(p, v){

  l <- length(v)

  constr1 <- p %*%  v
  constr2 <- p %*%  (v)^2
  constr5 <- sum(p)

  p_stay=p[which(v==0)]

  list(p_mean=round(constr1,3), p_var=round(constr2,3), p_sum=round(constr5,10), p_stay=p_stay)

}


# matrix with deviations #
# ====================== #

fifi_devMat <- function(D=D, ncat=ncat){

  nrows <- ncat + 1
  ncols <- nrows+D

  cat_i <- (nrows-1)
  cat_j <- (ncols-1)

  a <- 0:cat_j
  devMat <- sapply(1:ncols, function(x) x-a-1)[1:nrows,]

  colnames(devMat) <- 0:(cat_j)
  rownames(devMat) <- 0:(cat_i)

  return(devMat)

}



fifi_df <- function(probMat,D, szenario, blocking, ncat) {
  "-<-" <- p_int_lb <- p_int_ub <- i_info <- NULL
  devMat <- fifi_devMat(D=D, ncat=ncat)

  ncol <- ncol(probMat)
  nrow <- nrow(probMat)

  OUT <- data.table()

  # TODO: rewrite code using data.table
  for (z.i in 1:nrow){
    for (z.j in 1:ncol){

      i <- z.i - 1
      j <- z.j - 1

      check <- j %in% c((i-D) : (i+D) )
      if (j %in% blocking) check <- FALSE

      OUT <- rbind(OUT,
                   data.table(i=i, j=j, p=probMat[z.i, z.j], v=devMat[z.i, z.j], check=check) )
    }
  }

  OUT$i <- as.factor(OUT$i)
  OUT$j <- as.factor(OUT$j)
  #cat(ncol,"\n")

  kum_oben <- unlist(sapply(0:(nrow-1), function(i) cumsum(OUT$p[OUT$i == i]), simplify=TRUE))
  kum_unten <- kum_oben[1:(nrow(kum_oben)-1),]
  kum_unten <- rbind(0, kum_unten)

  OUT[,p_int_lb:=as.vector(kum_unten),]
  OUT[,p_int_ub:=as.vector(kum_oben),]

  OUT[,i_info:=paste("i=",i,sep=""),]
  OUT[i==max(as.integer(as.character(i))),i_info:=paste("i>=",i," (symmtery)",sep=""),]

  return(OUT)
}


fifi_probframe <- function(DF=DF, D=D){ #, file=NULL, saveDF=FALSE , date=format(Sys.time(), "%Y%m%d")
  v <- i <- j <- p_int_lb <- p_int_ub <- NULL
  neu <- DF[v %in% c(-D : D),c('i','j','p','v','p_int_lb','p_int_ub'),]
  neu <- neu[!(i==0 & j!=0),,]
  neu[,i:=as.integer(as.character(i))]
  neu[,j:=as.integer(as.character(j))]
  neu[,v:=as.integer(v)]

  neu[,p_int_lb:=round(p_int_lb, 10)]
  neu[,p_int_ub:=round(p_int_ub, 10)]

  neu <- neu[p_int_lb != p_int_ub,]

  # Save (different formats)
  #if (saveDF) write.csv2(neu, file=paste("de_",file,".csv",sep=""), row.names = FALSE)
  #if (saveDF) write.table(neu, file=paste(file,".csv",sep=""), sep=";", dec=".", row.names = FALSE, col.names = TRUE)

  return(neu)

}

pt_export <- function(input, file){

  stopifnot(isS4(input))
  stopifnot(class(input)=="ptable")

  params <- slot(input, "pParams")
  type <- slot(params, "type")
  stopifnot(type=="destatis")

  pTable <- slot(input, "pTable")[,c('i','v','p_int_ub'),]

  write.table(pTable, file=paste(file,".csv",sep=""), sep=";", dec=".", row.names = FALSE, col.names = TRUE)
}
