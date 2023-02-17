#' @title Export ptables as a csv-file
#'
#' @description Function to export perturbation table to Tau-Argus or SAS (as csv-file).
#'
#' @param ... 1 or 2 input object of class \code{\linkS4class{ptable}}
#' @param file (character) filename
#' @param SDCtool (character) either "TauArgus" or "SAS"
#'
#' @author Tobias Enderle
#' @keywords export
#'
#' @examples 
#' ptab <- create_cnt_ptable(D = 5, V = 3, js = 2, label = "test")
#' \dontrun{
#' pt_export(ptab, file = "Test", SDCtool = "TauArgus")
#' }
#' @rdname pt_export
#' @export
#'
pt_export <- function(..., file, SDCtool="TauArgus"){
  
  if (!is.null(file)) {
    stopifnot(is_scalar_character(file))
  }


  stopifnot(SDCtool %in% c("TauArgus","SAS"))

  inp.names <- names(list(...))
  inp <- list(...)
  #input <- eval(substitute(alist(...)))

  nr <- length(inp)
  message(inp.names)

  if( !(nr %in% c(1,2)) )
    stop(paste("At least 1 but no more than 2 input objects \n"))

  for (i in 1:nr){
    stopifnot(isS4(inp[[i]]))
    stopifnot(class(inp[[i]])=="ptable")
  }


  # Input Object 1
  params1 <- slot(inp[[1]], "pParams")
  table1 <- slot(params1, "table")
  type1 <- slot(params1, "type")

  if (table1=="cnts" & nr==2)
    stop(paste0("You have specified a frequency table (table='cnts'). So you need only 1 input object. That must be of type='all'."))
  
  # if only 1 object is set
  if (table1=="nums" & nr==1){

    query <- type1 == "all"

    if ( !(query) ) 
      stop(paste0("You have specified a magnitude table (table='nums') and set the argument type='",type1,"'. However, you need either 1 input object of type='all' OR you need 2 input objects: the first with type='even' and the second with type='odd'."))

  }

  if (table1=="cnts"){

    if (SDCtool=="TauArgus"){
      pTable <- slot(inp[[1]], "pTable")[,c('i','j','p','v','p_int_ub'),]
      message("\n NOTE: If you have modified the ptable using 'modify_cnt_ptable()': Please use a new Tau-Argus Release (>= 4.2.3).")
    }

    if (SDCtool=="SAS")
      pTable <- slot(inp[[1]], "pTable")[,c('i','j','p','v','p_int_lb','p_int_ub'),]

  }

  if (table1=="nums" & nr==2){
    params2 <- slot(inp[[2]], "pParams")
    table2 <- slot(params2, "table")
    type2 <- slot(params2, "type")

    query <- c("odd","even") %in% c(type1, type2)

    if ( !(all(query)) ) 
      stop(paste0("You have specified the types '",type1,"' and '",type2,"'. However, you need 'type=even' and also 'type=odd'."))

    pTable <- rbind(slot(inp[[1]], "pTable"),
                    slot(inp[[2]], "pTable"))
    pTable <- pTable[,c('i','j','p','v','p_int_ub','type'),]
  }

  if (table1=="nums" & nr==1){
  
    if (SDCtool=="TauArgus")
      pTable <- slot(inp[[1]], "pTable")[,c('i','j','p','v','p_int_ub','type'),]

    if (SDCtool=="SAS")
      pTable <- slot(inp[[1]], "pTable")[,c('i','j','p','v','p_int_lb','p_int_ub','type'),]
  } 
  write.table(format(pTable, digits=8), file=paste(file,".csv",sep=""), sep=";", dec=".", row.names = FALSE, col.names = TRUE, quote=FALSE)

  #return(pTable)

}
