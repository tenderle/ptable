

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
  
  out <- DT[, list(p_mean = sum(p*v),
                   p_var = sum(p*v^2),
                   p_sum = sum(p)), by = list(i)]

  pstay <- DT[i==j, list(p_stay = sum(p)), by = list(i)]
  
  out <- merge(out, pstay)
  out <- round(out, 5)
  
  out$p_stay[is.na(out$p_stay)] <- 0
  out[is.na(p_stay), p_stay := 0][]
  
  return(out)
  
}

