
#' Lasso Regression
#'
#' @param dataset
#' @param dv
#' @param startpred
#' @param const
#'
#' @return
#' @export
#'
#' @examples
lasso_regression <- function(dataset, dv, startpred, const = 1.1){
  dataset <- as.matrix(dataset)
  dv<-as.matrix(dv)

  # STEP 1: select variables that predict outcomes
  n=nrow(dataset)
  p=ncol(dataset)
  sda = sd(residuals(lm(dv ~ startpred)))
  lambda1 = sda*(const/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
  summary(lambda1)

  k = 1
  while(k < 15){
    fitY = glmnet::glmnet(dataset, dv, lambda=lambda1)
    ba = coef(fitY, s = lambda1)
    ea = dv-predict(fitY,dataset)
    sda = sd(ea)
    lambda1 = sda*(const/sqrt(n))* qnorm(1 - (.1/log(n))/(2*p))
    k = k+1
  }
  ba

  # STEP 2: linear regression with selected variables
  use = which(abs(fitY$beta)>.0001)
  summary(use)
  length(use)
  # Demographic variables to use as controls in other analyses
  demvar1 <- dataset[,use]

  #Show final regression model
  if (length(use)>0) {
    print("Regression using all data (Lasso selected)")
    fitr <- lm(dv ~ dataset[,use])
    print(summary(fitr))
    }

  if (length(use)==0) {
    print("No significant predictors using all data (Lasso selected)")
  }
  demvar1
}


#demvar <- lasso_regression(dataset = testvar.beh, dv = db$td.Patience, startpred = startpred.beh)

# =======================================================


# Permutation test for regression relating demographics to measures of patience

#' Permutation Test
#'
#' @param data
#' @param dv
#' @param nrep
#'
#' @return
#' @export
#'
#' @examples
permutation_test <- function(data, dv, nrep = 100){
  # Select predictors (all demographic words) & DV (individual meaasures of patience)
  testvar<-as.matrix(data)
  dv<-as.matrix(dv)


  # Run the regression
  reg<-lm(dv ~ testvar)
  summary(reg)
  regsum <- summary(reg)

  # Loop to run the simulation
  f<-numeric(nrep+1)
  for (i in 1:nrep) {
    dvx<-sample(dv, length(dv), replace = FALSE, prob = NULL)
    regx<-lm(dvx ~ testvar)
    regsumx<-summary(regx)
    f[i]<-regsumx$fstatistic[1]
  }
  f[nrep+1] <- regsum$fstatistic[1]
  print("Permutation test for F-stat")
  simp <- 1 - match(regsum$fstatistic[1], sort(f))/nrep
  print("Significance test for regression")
  regsum$fstatistic[1]
  print(simp)
}
# permutation_test(data = testvar,dv = db$td.Patience)

# ====================================================

#' Correlations for Multiple Testing
#'
#' @param dataset
#' @param dv
#' @param unadj_p
#' @param bh
#' @param permutation
#' @param nrep
#'
#' @return
#' @export
#'
#' @examples
corr_multiple_testing <- function(dataset, dv, unadj_p = TRUE, bh = TRUE, permutation = TRUE,
                             save_conf_level = FALSE, print_permutation_summary = FALSE, conf_level = 0.95, nrep = 100){

  # how many additinal columns to add
  extra_cols = 0
  if(unadj_p == TRUE){
    extra_cols = extra_cols + 1
    if(save_conf_level == TRUE){
      extra_cols = extra_cols + 2
    }
  }
  if(bh == TRUE){
    extra_cols = extra_cols + 1
  }
  if(permutation == TRUE){
    extra_cols = extra_cols + 1
    # if(save_conf_level == TRUE){
    #   extra_cols = extra_cols + 2
    #   f_add_2 <- 2
    # } else if (save_conf_level == FALSE) {f_add_2 <- 0}
  }
  split = (1 - conf_level)/2

  dv<-as.matrix(dv)
  tname <- colnames(dataset)
  dname <- colnames(dv)
  if(is.null(dname)){
    dname = "DV"
  }



  k <- ncol(dataset)
  d <- 1
  dr <- length(dv)



  # matrix to hold results
  results = matrix(0,k*d, 3 + extra_cols)
  for (i in 1:d){
    for (j in 1:k){
      results[(i - 1)*k + j, 1] = tname[j]
      results[(i - 1)*k + j, 2] = dname[i]
    }
  }

  results = as.data.frame(results)
  results = results %>% dplyr::mutate_at(.vars = 3:ncol(results), as.numeric) %>% dplyr::mutate_at(.vars = 1:2, as.character)
  colnames(results)[1] <- "var"
  colnames(results)[2] <- "dv"
  colnames(results)[3] <- "r"

  # Correlations
  corr.values <- matrix(0, nrow = d, ncol = k)
  p.values <- matrix(0, nrow = d, ncol = k)
  ci.lower <- matrix(0, nrow = d, ncol = k)
  ci.upper <- matrix(0, nrow = d, ncol = k)
  for (i in 1:k){
    ccor<- cor.test(dv, dataset[,i],
                    method="pearson",
                    use="pairwise.complete.obs",
                    conf.level = conf_level)
    corr.values[1,i] <- ccor$estimate
    p.values[1,i] <- ccor$p.value
    ci.lower[1,i] <- ccor$conf.int[1] # ci_lower
    ci.upper[1,i] <- ccor$conf.int[2] # ci_upper
  }
  acorr.values<-abs(corr.values)
  #max(acorr.values)
  results[,3] <- round(corr.values[1,],3)  # save correlations

  # counter for columns
  count = 4
  if(save_conf_level == TRUE){
    results[,count] <- round(ci.lower[1,], 3)  # save low_ci
    colnames(results)[count] <- "unadj_ci_low"
    count = count + 1
    results[,count] <- round(ci.upper[1,], 3)  # save high_ci
    colnames(results)[count] <- "unadj_ci_high"
    count = count + 1
  }
  if(unadj_p == TRUE){
    results[,count] <- round(p.values[1,],3)  # save p-values
    colnames(results)[count] <- "unadj_p"
    count = count + 1
  }
  # BENJAMINI-HOCHBERG P-VALUE ADJUSTMENT
  if(bh == TRUE){
    results[,count]<-round(p.adjust(p.values, "BH"), 3)     # save BH-corrected p-values
    colnames(results)[count] <- "bh_p"
    count = count + 1
  }

  # Permutation Test
  if(permutation == TRUE){
    # Loop to run the simulation
    f_add_2 <- 0
    f <- matrix(0, nrow = nrep, ncol = 2 + f_add_2)

    for (j in 1:nrep) {
      dvx <- dv[sample(dr)] # reorder the rows of the dv matrix
      for (i in 1:k){
        ccor <- cor.test(dvx, dataset[,i],
                         method="pearson",
                         use="pairwise.complete.obs",
                         conf.level = conf_level)
        corr.values[1,i] <- ccor$estimate
        p.values[1,i] <- ccor$p.value
        # ci.lower[1,i] <- ccor$conf.int[1] # ci_lower
        # ci.upper[1,i] <- ccor$conf.int[2] # ci_upper
      }
      f[j,1] <- max(abs(corr.values))
      f[j,2] <- sum(p.values < .05) # number significant by chance
      # if (save_conf_level ==TRUE){
      #   f[j,3] <- ci.lower[1,i]  # ci_lower
      #   f[j,4] <- ci.upper[1,i]  # ci_upper
      # }
    }
    if (print_permutation_summary == TRUE){
      fs<-sort(f[,1])
      cat("95th percentile of largest correlation by chance: ")
      cat(fs[trunc(.95*nrep)], "\n \n")
      cat ("average number of correlations significant by chance: ")
      fs<-sort(f[,2])
      cat(mean(fs), "\n \n")
      cat ("95th percentile number of correlations significant by chance: ")
      cat(fs[trunc(.95*nrep)], "\n \n")
    }
    # save permuation-based p-values
    colnames(results)[count] <- "permutation_p"
    results$permutation_p <- 0

    for (i in 1:nrow(results)){
      x <- abs(as.numeric(results[i,3])) # regular correlation
      # place regular into distribution and see which place it lands, later landing is lower p value
      results[i,count] <- 1 - round(match(x, sort(c(f[,1],x))) / (length(f[,1]) + 1), 3)
      #  if (save_conf_level ==TRUE){
      #   results[i, count + 1] <- quantile(f[,3], probs = 0 + split)  # ci_lower
      #   results[i, count + 2] <- quantile(f[,4], probs = 1 - split)  # ci_upper
      # }
    }
    # if (save_conf_level == TRUE){
    #   count = count + 1
    #   colnames(results)[count] <- "perm_ci_low"
    #   count = count + 1
    #   colnames(results)[count] <- "perm_ci_high"
    # }
    count = count + 1
  }
  results
}

#multiple_testing(dataset = testvar, dv = db$td.Patience, nrep = 1000)

# =============================================================
#' Partial Correlations for Multiple Testing
#'
#' @param data
#' @param dv
#' @param controls
#' @param unadj_p
#' @param bh
#' @param permutation
#' @param nrep
#'
#' @return
#' @export
#'
#' @examples
partial_corr_multiple_testing <- function(data, dv, controls, unadj_p = TRUE, bh = TRUE, permutation = TRUE, nrep = 100){

  tname<-colnames(data)
  dv<-as.matrix(dv)
  dname<-colnames(dv)
  if(is.null(dname)){
    dname = "DV"
  }

  k<-ncol(data)
  d<-1
  dr<-length(dv)


  # matrix to hold results
  results = matrix(0,k*d,6)
  for (i in 1:d){
    for (j in 1:k){
      results[(i-1)*k+j,1]=tname[j]
      results[(i-1)*k+j,2]=dname[i]
    }
  }

  # Correlations
  corr.values <- matrix(0,nrow=d,ncol=k)
  p.values <- matrix(0,nrow=d,ncol=k)
  for (i in 1:k){
    pcor<- ppcor::pcor.test(dv, data[,i], controls,
                     method="pearson")
    corr.values[1,i]<-pcor$estimate
    p.values[1,i]<-pcor$p.value

  }
  acorr.values<-abs(corr.values)
  max(acorr.values)
  results <- as.data.frame(results)
  results[,3] <- round(corr.values[1,],3)  # save correlations


  colnames(results)[1] <- "var"
  colnames(results)[2] <- "dv"
  colnames(results)[3] <- "r"

  # counter for columns
  count = 4
  if(unadj_p == TRUE){
    results[,count] <- round(p.values[1,],3)  # save p-values
    colnames(results)[count] <- "unadj_p"
    count = count + 1
  }

  # BENJAMINI-HOCHBERG P-VALUE ADJUSTMENT
  if(bh == TRUE){
    results[,count]<-round(p.adjust(p.values, "BH"), 3)     # save BH-corrected p-values
    colnames(results)[count] <- "bh_p"
    count = count + 1
  }

  # Permutation Test
  if(permutation == TRUE){
    f <- matrix(0,nrow=nrep,ncol=2)

    for (j in 1:nrep) {
      dvx<-dv[sample(dr)] # reorder the rows of the dv matrix
      for (i in 1:k){
        pcor<- ppcor::pcor.test(dvx, data[,i], controls,
                         method="pearson")
        corr.values[1,i]<-pcor$estimate
        p.values[1,i]<-pcor$p.value
      }
      f[j,1] <- max(abs(corr.values))
      f[j,2] <- sum(p.values < .05)
    }

    fs<-sort(f[,1])
    print ("95th percentile of largest correlation by chance:")
    print(fs[trunc(.95*nrep)])
    print ("average number of correlations significant by chance:")
    fs<-sort(f[,2])
    print(mean(fs))
    print ("95th percentile number of correlations significant by chance:")
    print(fs[trunc(.95*nrep)])

    # save permuation-based p-values
    colnames(results)[count] <- "permutation_p"
    results$permutation_p <- NA_real_

    # save permuation-based p-values
    for (i in 1:nrow(results)){
      x <- abs(as.numeric(results[i,3]))
      results[i,count] <- 1 - round(match(x,sort(c(f[,1],x)))/(length(f[,1])+1),3)
    }
  }
  results
}

# partial_multiple_testing(data = testvar.beh, controls = demvar1, dv = db$td.Patience, nrep = 100)

# ==============================================================

#' Canonical Correlations
#'
#' @param data
#' @param dv
#'
#' @return
#' @export
#'
#' @examples
canonical_correlation <- function(data, dv){

  dvset<-as.matrix(dv)
  testvar<-as.matrix(data)

  # Run the canonical correlation analysis
  ccA <- CCA::cc(testvar, dvset)

  # display the canonical correlations
  ccA$cor

  # compute canonical loadings
  ccA_loadings <- CCA::comput(testvar, dvset, ccA)
  ccA_loadings[3:6]

  # standardized testvar canonical coefficients diagonal matrix of testvar sd's
  s1 <- diag(sqrt(diag(cov(testvar))))
  s1 %*% ccA$xcoef
  # standardized dvset canonical coefficients diagonal matrix of dvset sd's
  s2 <- diag(sqrt(diag(cov(dvset))))
  s2 %*% ccA$ycoef

  # tests of canonical dimensions
  ev <- (1 - ccA$cor^2)
  n <- dim(testvar)[1]
  p <- ncol(testvar)
  q <- ncol(dvset)
  k <- min(p, q)
  m <- n - 3/2 - (p + q)/2
  w <- rev(cumprod(rev(ev)))

  # initialize
  d1 <- d2 <- f <- vector("numeric", k)

  for (i in 1:k) {
    s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si <- 1/s
    d1[i] <- p * q
    d2[i] <- m * s - p * q/2 + 1
    r <- (1 - w[i]^si)/w[i]^si
    f[i] <- r * d2[i]/d1[i]
    p <- p - 1
    q <- q - 1
  }

  pv <- pf(f, d1, d2, lower.tail = FALSE)
  print("Significance test for canonical correlation")
  dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
  dmat

  # Source: https://stats.idre.ucla.edu/r/dae/canonical-correlation-analysis/
  # Note: omnibus test is whether the first dimension is significant
}

