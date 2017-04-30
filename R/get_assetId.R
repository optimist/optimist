#' @title get_assetId
#' @description Creates a unique id from 'tipo valor',
#' 'emisora' and 'serie'.
#' @param tipo character vector containing 'tipo valor'.
#' @param emisora character vector containing 'emisora'.
#' @param serie character vector conatining 'serie'.
#' @details xx
#' @return xx
#' @examples
#' get_AssetId("M", "BONOS", "191211")
#' @export
get_assetId <- function(tipo, emisora, serie) {
  # suggestion- verify inputs are character vectors
  tipo[nchar(tipo) == 1] <- paste(tipo[nchar(tipo) == 1], "_", sep = "")
  id <- tolower(paste(tipo, emisora, serie, sep = ""))
  id <- gsub("\\*|\\+|\\-", "_", id)
  id
}
