#' Robust Standard Error
#' @export
#' @param model
#' @param cluster What variable should the data be clustered by
robust_se <- function(model, cluster){
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(sandwich::estfun(model), 2, function(x) tapply(x, cluster, sum))
  rcse_cov <- dfc * sandwich::sandwich(model, meat = crossprod(uj)/N)
  rcse_se <- lmtest::coeftest(model, rcse_cov)
  return(list(rcse_cov, rcse_se))
}
