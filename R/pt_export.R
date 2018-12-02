#' pt_export
#'
#' Function to export perturbation table to Tau-Argus or SAS (as csv-file).
#'
#' @param input an object of class \code{\linkS4class{ptable}}
#' @param file (character) filename
#' @param SDCtool (character) either "TauArgus" or "SAS"
#'
#' @author Tobias Enderle
#' @keywords export
#'
#' @examples 
#' params <- pt_create_pParams(D=5, V=3, js=2, label="test")
#' ptable_destatis <- pt_create_pTable(params=params, type="destatis")
#' \dontrun{
#' pt_export(ptable_destatis,file="Test", SDCtool="TauArgus")
#' }
#' 
#' @rdname pt_export
#' @export
#'
pt_export <- function(input, file, SDCtool="TauArgus"){
  
  stopifnot(isS4(input))
  stopifnot(class(input)=="ptable")
  
  params <- slot(input, "pParams")
  type <- slot(params, "type")
  stopifnot(type=="destatis")
  
  stopifnot(SDCtool %in% c("TauArgus","SAS"))
  
  if (SDCtool=="TauArgus")
    pTable <- slot(input, "pTable")[,c('i','j','p','v','p_int_ub'),]
  
  if (SDCtool=="SAS")
    pTable <- slot(input, "pTable")[,c('i','j','p','v','p_int_lb','p_int_ub'),]
  
  
  write.table(pTable, file=paste(file,".csv",sep=""), sep=";", dec=".", row.names = FALSE, col.names = TRUE)
}