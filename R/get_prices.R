#' @title get_prices_csv
#' @description Builds an xts containing asset prices
#' from a csv.
#' @param file name of the file from which the data
#' will be read from
#' @details Date should be provided on the first column
#' with yyyy-mm-dd format
#' @export
get_prices_csv <- function(
  file
){
  prices <- read.csv(file, stringsAsFactors = FALSE)
  prices <- xts(prices[, -1], order.by = as.Date(prices[, 1]))
  prices
}

#' @title get_prices_yahoo
#' @description Builds an xts containing asset prices
#' downloaded from http://ichart.finance.yahoo.com/.
#' @param yahoo_id character array containining assets'
#' yahoo ids.
#' @param from start date.
#' @param to end date. Sys.Date() is the default value
#' @param boolean indicating whether adjusted prices
#' should be returned.
#' @details xx
#' @export
get_prices_yahoo <- function(
  yahoo_id,
  from,
  to = Sys.Date(),
  adjust = TRUE
) {
  yahoo.URL <- "http://ichart.finance.yahoo.com/table.csv?"

  from.y <- year(from)
  from.m <- month(from) - 1
  from.d <- day(from)

  to.y <- year(to)
  to.m <- month(to) - 1
  to.d <- day(to)

  tmp <- tempfile()
  on.exit(unlink(tmp))

  prices <- list()
  i <- 0

  for(id in yahoo_id) {
    file <- paste(yahoo.URL,
                  "s=", id,
                  "&a=", from.m,
                  "&b=", sprintf('%.2d',from.d),
                  "&c=", from.y,
                  "&d=", to.m,
                  "&e=", sprintf('%.2d',to.d),
                  "&f=", to.y,
                  "&g=d&q=q&y=0",
                  "&z=", id, "&x=.csv",
                  sep='')

    download.file(file, destfile = tmp)

    asset_prices <- read.csv(tmp)

    if(adjust) {
      asset_prices <- xts(as.matrix(asset_prices[, "Adj.Close"]),
                          as.Date(asset_prices[, "Date"]))
    } else {
      asset_prices <- xts(as.matrix(asset_prices[, "Close"]),
                          as.Date(asset_prices[, "Date"]))
    }

    i <- i + 1
    prices[[i]] <- asset_prices
    asset_alias <- toupper(gsub('\\^','',id))
    names(prices)[[i]] <- asset_alias
  }

  prices <- do.call(cbind, prices)
  prices
}
