#' Standardized Result
#'
#' @param eff.type
#' @param u.s.d
#' @param ctrl.sd
#' @param n.t
#' @param n.c
#'
#' @return
#' @export
#'
#' @examples
stand.result <- function( eff.type , u.s.d , ctrl.sd , n.t, n.c){
  ## All calculations taken from Cooper, Hedges, and Valentine (2009)

  # difference in differences
  if (eff.type == "d.i.d"){
    d <- round(u.s.d / ctrl.sd, digits = 3)
  }
  # difference in means
  else if (eff.type == "d.i.m"){
    d <- round(u.s.d / ctrl.sd, digits = 3)
  }
  # reporting of change of SDs in text:
  else if(eff.type == "d"){
    d <- u.s.d
  }

  # regression coefficient
  else if (eff.type == "reg.coef"){
    d <- round(u.s.d / ctrl.sd, digits = 3)
  }

  # t test
  else if (eff.type == "t.test"){
    d <- round(u.s.d * sqrt( (n.t + n.c ) / (n.t * n.c) ) , digits = 3)
  }
  # f.test
  else if (eff.type == "f.test"){
    d <- round(sqrt( ( u.s.d * (n.t + n.c) ) / (n.t * n.c) ), digits = 3)
  }
  # compute variance of the estimated effect size

  ust.var.d <- (((n.t + n.c)
                 / (n.t * n.c))
                +
                  ((d^2) / (2*(n.t + n.c)) )
  )
  # Apply hedge's g correction
  hedge.g <- 1 - (3
                  /
                    (4*(n.t + n.c -2 ) -1))

  var.d <- round((hedge.g^2) * ust.var.d, digits = 3)

  # standard error is the square root of variance
  st.err.g <- round(sqrt(var.d), digits = 3)

  # print everything out
  results <- c(d, var.d, st.err.g)

  col.names <- c("Standardized Effect (Cohen's D)" ,
                 "Variance of D" , "Standard Error of D")

  results.table <-data.frame(col.names, results)

  #print(results.table)
  return(results.table)

}



#' CI converter
#'
#' @param upper Upper confidence interval
#' @param lower Lower confidence interval
#' @param n Sample size
#' @param interval Either 95, or 90. 95 by default.
#'
#' @return
#' @export
#'
#' @examples
CI_converter <- function(upper, lower, n, interval = 95) {
  # https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm
  if (interval == 95){
    sd <- (sqrt(n) * (upper - lower)) / 3.92
    print(sd)
    return( (sqrt(n) * (upper - lower)) / 3.92)
  }
  else if(interval == 90){
    sd <- (sqrt(n) * (upper -lower)) / 3.29
    print(sd)
    return(sd)
  } else {
    stop("Interval is not equal to 90 or 95")
  }
}


#' Difference in Difference
#'
#' @param mean_treatment_post
#' @param mean_treatment_pre
#' @param mean_control_post
#' @param mean_control_pre
#' @param sd
#'
#' @return
#' @export
#'
#' @examples
did_calculator <- function(mean_treatment_post, mean_treatment_pre, mean_control_post, mean_control_pre, sd){
  did <-( (mean_treatment_post - mean_treatment_pre) - (mean_control_post - mean_control_pre)) / sd
  print(did)
  did
}


#' Standard Error to Standard Deviation
#'
#' @param se Standard error
#' @param n Sample size
#'
#' @return
#' @export
#'
#' @examples
se_2_sd <- function(se, n){
  sd <- (se * sqrt(n))
  print(sd)
  sd
}



#' Standard Deviation Pooled
#'
#' @param sd Vector of standard deviations
#' @param n Vector of sample sizes
#'
#' @return
#' @export
#'
#' @examples
sd_pooled <- function(sd, n){
  #taken from Hedges, 1981:110
  if (length(sd) != length(n)){
    stop("Length of sd and length of n need to be the same")
  }

  k <- length(sd)
  sd2 <- sd^2
  df <- n - 1

  num <- sum(sd2*df)
  dem <- sum(n) - k

  sd_pooled <- sqrt(num/dem)
  print(sd_pooled)
  sd_pooled

}
