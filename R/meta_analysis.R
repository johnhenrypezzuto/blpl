#' Standardized Result
#'
#' @param eff.type Effect type. Either `d.i.d`, `d.i.m`, `d`, `reg.coef`, `t.test`, `f.test`
#' @param u.s.d Unstandardized effect size
#' @param ctrl.sd Control standard deviation
#' @param n.t Treatment sample size
#' @param n.c Control group sample size
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



#' Confidence Interval to Standard Deviation
#'
#' @param upper_ci Upper confidence interval
#' @param lower_ci Lower confidence interval
#' @param n Sample size
#' @param interval Either 95, or 90. 95 by default.
#'
#' @return
#' @export
#'
#' @examples
ci_2_sd <- function(upper_ci, lower_ci, n, interval = 95) {
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
#' @param sd Control standard deviation
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



#' Regression Coefficient To D
#'
#' @param beta Beta from regression
#' @param standard_error Corrosponding standard error
#' @param n Corrosponding sample size
#'
#' @return
#' @export
#'
#' @examples
beta_2_d <- function(beta, standard_error, n){
  t = beta / standard_error
  d <- (t*2)/sqrt(n-2)
  d
}


#' Regression Coefficient To R
#'
#' @param beta Beta from regression
#' @param standard_error Corrosponding standard error
#' @param n Corrosponding sample size
#'
#' @return r value
#' @export
#'
#' @examples
beta_2_r <- function(beta, standard_error, n){
  t = beta / standard_error
  d <- (t*2)/sqrt(n-2)
  r <- sqrt(d^2 / (4 + d^2))
  r
}


#' Pearson's r to d
#'
#' @param r Pearson's r
#'
#' @return
#' @export
#'
#' @examples
r_2_d <- function(r){
  d <- (4 * r^2) / (1 - r^2)
  d
}


#' Cohen's d to r
#'
#' @param d Cohen's d
#'
#' @return
#' @export
#'
#' @examples
d_2_r <- function(d){
  r <- sqrt(d^2 / (4 + d^2))
  r
}

#' T Inverse
#'
#'Calculates t value from p, and n values
#'
#' @param p P-value associated with t-test
#' @param n Sample size associated with t-test
#'
#' @return
#' @export
#'
#' @examples
t_inverse <- function(p, n){
  # https://stackoverflow.com/questions/21730285/calculating-t-inverse
  qt(1-p/2, n-2)
}

#' Convert t-test to d
#'
#' @param t A t-test t value
#' @param n Corrosponding sample size
#'
#' @return
#' @export
#'
#' @examples
t_2_d <- function(t, n){
  d <- (t*2)/sqrt(n-2)
  d
}


#' Convert t-test to r
#'
#' @param t A t-test value
#' @param n Corrosponding sample size
#'
#' @return
#' @export
#'
#' @examples
t_2_r <- function(t, n){
  d <- (t*2)/sqrt(n-2)
  r <- sqrt(d^2 / (4 + d^2))
  r
}



#' Weighted R
#'
#' @param r A pearson's r
#' @param n Corrosponding sample size
#'
#' @return
#' @export
#'
#' @examples
weighted_r <- function(r, n){
  Wr <- (n - 1) / ((1 - r^2)^2)
  Wr
}


#' Merged R
#'
#' @param r `Vector` of pearson's R correlations
#' @param weighted_r `Vector` of weighted R functions. See `weighted_r`
#'
#' @return Single meta-analysis r value
#' @export
#'
#' @examples
merged_r <- function(r, weighted_r){
  if (length(r) != length(weighted_r)){
    stop("Length of r and length of weighted_r need to be the same")
  }
    total_weighted_r <- sum(weighted_r)
    new_weighted_r <- weighted_r / total_weighted_r
    total_product <- new_weighted_r * r
    sum_total_product <- sum(total_product)
    sum_total_product
}

