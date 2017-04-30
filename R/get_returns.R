#' @title get_returns
#' @description Creates xts of returns
#' @param roll default is 1
#' @param periods_on default is 'days'
#' @details xx
#' @export
get_returns <- function(prices, roll = 1, periods_on = 'days') {
  if(periods_on == 'end') end_pts <- c(0, nrow(prices)) else end_pts <- endpoints(prices, periods_on)
  if(periods_on == 'days') {
    scale <- rep(roll, nrow(prices))
  } else {
    if(roll > 1) stop("roll > 1 only used if periods_on equal to 'days'")
    scale <- lapply(diff(end_pts), function(x) 1:x)
    scale[[1]] <- scale[[1]] - 1
    scale <- unlist(scale)
  }
  width <- as.list(-scale)
  returns <- prices / xts(rollapply(zoo(prices), width, c, fill = NA)) - 1
  returns <- reclass(returns, prices)

  scale <- matrix(rep(scale, ncol(returns)), nrow(returns), ncol(returns))
  scale[is.na(returns)] <- 0

  xtsAttributes(returns)$roll <- roll
  xtsAttributes(returns)$periods_on <- periods_on
  xtsAttributes(returns)$end_pts <- end_pts
  xtsAttributes(returns)$scale <- zoo(scale)
  returns
}

#' @title accumulate_returns
#' @description Accumulates returns
#' @param returns xts containing returns
#' @param roll default is 1
#' @param periods_on default is NULL. It can take any
#' value that is supported by xts::endpoints
#' @details xx
#' @export
accumulate_returns <- function(returns, roll = 1, periods_on = 'end') {
  if(periods_on == 'end') end_pts <- c(0, nrow(returns)) else end_pts <- endpoints(returns, periods_on)
  if(periods_on == 'days') {
    width <- rep(roll, nrow(returns))
  } else {
    if(roll > 1) stop("roll > 1 only used if periods_on equal to 'days'")
    width <- lapply(diff(end_pts), function(x) 1:x)
    width <- unlist(width)
  }
  width <- lapply(width, function(x) (-x + 1):0)
  ret <- log(returns + 1)
  ret <- zoo(ret)
  ret <- exp(rollapply(ret, width, sum, na.rm = TRUE, fill = NA)) - 1 #should na.rm be TRUE
  returns <- reclass(ret, returns)

  scale <- xtsAttributes(returns)$scale
  scale <- rollapply(scale, width, sum, fill = 0)

  xtsAttributes(returns)$roll <- roll
  xtsAttributes(returns)$periods_on <- periods_on
  xtsAttributes(returns)$end_pts <- end_pts
  xtsAttributes(returns)$scale <- scale
  returns
}

#' @title aggregate_returns
#' @description Aggregates returns
#' @param returns xts containing returns
#' @details xx
#' @export
aggregate_returns <- function(returns) {
  end_pts <- xtsAttributes(returns)$end_pts
  returns <- returns[end_pts[-1], ]

  scale <- xtsAttributes(returns)$scale

  xtsAttributes(returns)$end_pts <- 0:nrow(returns)
  xtsAttributes(returns)$scale <- scale[end_pts[-1]]
  returns
}

#' @title annualize_returns
#' @description Annualize returns
#' @param returns xts containing returns
#' @param factor annualization factor
#' @details xx
#' @export
annualize_returns <- function(returns, factor = 252) {
  scale <- xtsAttributes(returns)$scale

  returns <- exp(log(returns + 1) * factor / xts(scale, index(returns))) - 1

  scale <- matrix(rep(factor, nrow(returns) * ncol(returns)), nrow(returns), ncol(returns))

  xtsAttributes(returns)$scale <- zoo(scale)
  returns
}

#' @title mean_returns
#' @description Compute mean returns
#' @param returns xts containing mean returns
#' @param roll default is 12
#' @param periods_on default is "days"
#' @param halflife default is 1. As percentage
#' @details xx
#' @export
mean_returns <- function(returns, roll = 12, periods_on = "days", halflife = 1) {
  ret <- returns + 1
  ret <- zoo(ret)

  if(periods_on == 'end') end_pts <- c(0, nrow(returns)) else end_pts <- endpoints(returns, periods_on)
  if(periods_on == 'days') {
    decay <- get_decay(roll, halflife, method = "semi-geometric") #set as semi-geometric
    ret <- rollapply(ret, width = roll, align = 'right', FUN = weighted.geometric.mean, w = decay, na.rm = TRUE, fill = NA) - 1
  } else {
    if(roll > 1) stop("roll > 1 only used if periods_on equal to 'days'")

    put_decay <- function(x) {
      n <- length(x) - sum(is.na(x))
      decay <- get_decay(n)
      replace(x, !is.na(x), decay)
    }

    by <- cut(1:nrow(ret), end_pts)
    decay <- lapply(split(ret, by), function (x) apply(x, 2, put_decay))
    decay <- do.call(rbind, decay)
    ret <- log(ret) * decay

    width <- lapply(diff(end_pts), function(x) 1:x)
    width <- unlist(width)
    width <- lapply(width, function(x) (-x + 1):0)

    ret <- exp(rollapply(ret, width, sum, na.rm = TRUE, fill = NA)) - 1 #should na.rm be TRUE

    scale <- xtsAttributes(returns)$scale * decay
    scale <- rollapply(scale, width, sum, na.rm = TRUE, fill = NA)
    xtsAttributes(returns)$scale <- scale
  }
  returns <- reclass(ret, returns)

  xtsAttributes(returns)$roll <- roll
  xtsAttributes(returns)$periods_on <- periods_on
  xtsAttributes(returns)$end_pts <- end_pts
  returns
}
