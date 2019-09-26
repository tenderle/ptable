

# ================================================ #
# Check of conditions #
# ================================================ #

fifi_check_p <- function(p, v){

  l <- length(v)

  constr1 <- p %*%  v
  constr2 <- p %*%  (v)^2
  constr5 <- sum(p)

  p_stay=p[which(v==0)]

  list(p_mean=round(constr1,3), p_var=round(constr2,3), p_sum=round(constr5,10), p_stay=p_stay)

}

fifi_check_pTable <- function(DT){
  
  . <- i <- j <- p_mean <- p_var <- p_sum <- p <- NULL
  
  constr1 <- aggregate((p*v)~i, data=DT, sum)
  constr2 <- aggregate((p*v^2)~i, data=DT,sum)
  constr5 <- aggregate(p~i, data=DT,sum) 
  pstay <- round(DT[i==j, c("i","p")],4)
  
  out <- data.table(i=constr1[,1], 
                    p_mean=round(constr1[,2],5), 
                    p_var=round(constr2[,2],5), 
                    p_sum=round(constr5[,2],5))
  
  out <- merge(out, pstay, by="i", all = TRUE)
  out$p[is.na(out$p)] <- 0
  out <- out[,.(i, p_mean, p_var, p_sum, p_stay=p)]
  
  
  return(out)
  
}

