#' @title Check the constraint of the ptable
#'
#' @description [pt_check()] checks the constraints of the ptable
#' @md
#' @param ptab a `data.table` or an an object of [ptable-class] generated with [create_cnt_ptable()].
#' @return a data.table object
#'
#' @author Tobias Enderle, \email{tobias.enderle@@destatis.de}
#' @keywords check ptable
#'
#' @examples
#' # create ptable 
#' ptab1 <- create_cnt_ptable(D = 5, V = 3, js = 2, label = "test2")
#' 
#' # check ptable
#' pt_check(ptab1)
#' 
#' @rdname pt_check
#' @export

pt_check <- function(ptab){
  
  if (class(ptab)[1] == "ptable")
    out <- fifi_check_pTable(ptab@pTable)
  
  if (class(ptab)[1] == "data.table")  
    out <- fifi_check_pTable(ptab)
  
  return(out)
  
}

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
  
  . <- i <- j <- p_mean <- p_var <- p_sum <- p <- v <- p_stay <- NULL
  
  out <- DT[, list(p_mean = sum(p*v),
                   p_var = sum(p*v^2),
                   p_sum = sum(p)), by = list(i)]

  pstay <- DT[i==j, list(p_stay = sum(p)), by = list(i)]
  
  out <- merge(out, pstay, all = TRUE)
  out <- round(out, 5)
  
  out$p_stay[is.na(out$p_stay)] <- 0
  out[is.na(p_stay), p_stay := 0][]
  
  return(out)
  
}

