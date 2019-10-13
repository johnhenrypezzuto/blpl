#' tidy_lm
#'
#' A function to streamline the process of running regressions in experimental psychology
#' @param dv Vector of one or more dependent variables. The function will regress all the terms on each dv separately
#' @param terms Vector of terms to regress on dv.
#' @param style
#' Character string either "default", "incremental", "chord", or "bivariate". In what way should the terms regress on dv.
#'  *  "default" uses all terms at once on all the DVs.
#' * "incremental" starts off with one term and adds one more term each model for each dv.
#' * "chord" begins by running a bivariate regression the first term. Then runs a trivariate regression, holding the dv, first is term constant looping through the remaining terms.
#' Lastly, all terms are ran together.
#' * "bivariate" uses all terms individually on each dv.
#' @param treatment Vector for terms to include in the output dataset. Returns coefficent, lower confidence interval,
#' upper confidence interval, standard error, t-value, and p-value.
#' @param clusters Clusters for grouping standard error.
#' @param robust_se TRUE/FALSE. Whether to use to use robust standard errors ("HC2"). Equivalent to default standard error from lm_robust(). Set to FALSE by default.
#' @param alpha Numeric p-value for the standard errors. Default .05.
#' @param print_summary TRUE/FALSE. Whether to print model summaries or not. Default is FALSE.
#' @param data A `data.frame`
#' @export
#' @return  A `tibble` containing all linear models, and data on what variables are included in each model.
# data = mtcars; data %<>% mutate(cyl = factor(cyl))
# dv = c("mpg", "hp"); style = "default"; terms = "cyl"; treatment = "cyl"; robust_se = FALSE; alpha = .05; print_summary = FALSE
# lm(mpg ~ cyl, data = data)


tidy_lm <- function(data, dv, terms, treatment = NULL, style = "default", clusters = NULL, robust_se = FALSE, alpha = .05, multiple_testing = NULL, print_summary = FALSE){
  if(hasArg(dv) == FALSE) stop("must have dv")
  if(hasArg(terms) == FALSE) stop("must have terms")

  # Tidy eval workaround
  ## function for cleaning vectors
  quote_machine <- function(vector){
    if(stringr::str_sub(vector[2], 1, 2) != "c("){
      x2 <- vector[2]
    } else {
      x2 <- trimws(stringr::str_split(stringr::str_sub(vector[2], 3, -2), ",")[[1]])
    }
    x3 <-
      x2 %>%
      dplyr::as_tibble() %>%
      mutate(value = ifelse(stringr::str_detect(x2, "\"") == TRUE, str_remove_all(value, "\""), value)) %>%
      as_vector() %>%
      unname()
    x3
  }

  ## Run through function if necessary
  ### dv
  test <- try(length(dv), silent = TRUE)
  if (class(test) == "try-error"){
    dv <- as.character(rlang::enquo(dv))
    dv <- quote_machine(dv)
  }
  ### terms
  test <- try(length(terms), silent = TRUE)
  if (class(test) == "try-error"){
    terms <- as.character(rlang::enquo(terms))
    terms <- quote_machine(terms)
  }
  ### treatment
  test <- try(length(treatment), silent = TRUE)
  if (class(test) == "try-error"){
    treatment <- as.character(rlang::enquo(treatment))
    treatment <- quote_machine(treatment)
  }



  # Check Inputs
  ## Check Style
  if (style == "incremental" | style == "bivariate" || style == "default" || style == "chord" || style == "weave") {
  } else {
    stop("Style must be 'incremental', 'bivariate', 'chord', 'weave', or 'default'")
  }
  ## Check multiple testing
  if (multiple_testing == "FDR" | multiple_testing == "BH" || multiple_testing == "permutation" || is.null(multiple_testing)) { # check spelling
  } else {
    stop("multiple_testing must be 'FDR', 'BH', 'permutation'")
  }

  ## Check DV Variables (this is intends to check if dv exist)
  i = 1

  for(i in 1:length(dv)) {
    if (!dv[i] %in% colnames(data)){
      stop("'", dv[i], "'", " in the dv is not a variable in the dataset")
    }
    if (sapply(data[,dv[i]], class) == "factor"){
      stop("'", dv[i], "'", " variable in the dv can't be type factor")
    } else if (sapply(data[,dv[i]], class) == "list"){
      stop("'", dv[i], "'", " variable in the dv can't be type list")
    }
  }
  ## No Matches In DV And Terms
  if (sum(dv %in% terms) >= 1){
    num <- which(dv %in% terms)
    stop("'",dv[num],"'", " is in both the dv and terms")
  }
  ## Check Terms Variables
  for(i in 1:length(terms)) {
    if (!terms[i] %in% colnames(data)){
      stop("'", terms[i], "'", " in the dv is not a variable in the dataset")
    }
  }

  # alpha level
  if (is.numeric(alpha) == FALSE){
    stop("alpha needs to be numeric")
  }

  if (alpha != .05){
    alpha_print = paste0(", alpha = ", alpha)
  } else {
    alpha_print = NULL
  }


  # SE standard
  standard_error = "classical"
  cluster = NULL
  cluster_data <- NULL


  ## robust se
  if (robust_se == TRUE) {
    standard_error = "HC2"
  }

  ## assign standard error type, and ready cluster to be printed
  if (is.null(clusters) == FALSE) {
    clusters <- as.character(rlang::enquo(clusters))
    clusters <- quote_machine(clusters)
    standard_error = "stata"
    cluster = paste0(", clusters = ", clusters)
    cluster_data <- data[, clusters]
  }

  ## check that treatment is in terms
  if (style == "default" || style == "weave"){
    unique_terms <- dplyr::setdiff(treatment, terms)
  } else {
    if (length(treatment) != sum(treatment %in% terms)){
      stop("'", treatment[which((treatment %in% terms) == FALSE)], "'", " is included as a treatment, but not included as a term")
    }
  }



  # lm function
  simple_model = FALSE # default is lm_robust
  lm_function <- function(){
    data %>% do(y = estimatr::lm_robust(as.formula(paste(j, "~", i)),
                                        data = data,
                                        alpha = alpha,
                                        clusters =  cluster_data,
                                        se_type = standard_error))
  }
  if (robust_se == FALSE & is.null(clusters)){
    simple_model = TRUE
    lm_function <- function(){
      model <- paste(j, "~", i)
      data %>% do(y = lm(model, data = data))
    }
  }
  # get df name
  data_name <-deparse(substitute(data))


  # Tidy Summaries (if print_summary = TRUE)
  tidy_summaries <- function(){
    cat("\n#", loopnum, "-------------------------------------------------- ")
    cat(paste0("\nlm_robust(", j, " ~ ", i, cluster,
               alpha_print,
               ", se_type = \"", standard_error,
               "\", data = ", data_name, ")\n"))
    working_sum <- summary(results[loopnum, ncol(results)][[1]][[1]])
    try(cat(paste0("N = ", working_sum$N, "\n")), silent = TRUE) # doesn't work in basic version
    print(working_sum$coefficients)
    cat("\n")
    cat("R-Sq = ", working_sum$r.squared,
        "\nR-Sq(adj) = ",  working_sum$adj.r.squared)
    cat("\n", paste0("\nF[", working_sum$fstatistic[2], ",",working_sum$fstatistic[3], "] = ", round(working_sum$fstatistic[1], 2),
                     ", p = ", pf(working_sum$fstatistic[1],working_sum$fstatistic[2],working_sum$fstatistic[3],lower.tail=FALSE), "\n"))
  }

  # Create Matrix
  ## Size of Matrix
  if (style == "incremental"){
    t = length(terms)
    l = t
  } else if (style == "bivariate"){
    t = length(terms)
    l = 1  # length t in expanded version (didn't double check)
  } else if (style == "default"){
    if (length(unique_terms) == 0){
      t = 1
      l = length(terms)
    } else if (length(unique_terms) != 0){ ##### edit here for expansion
      t = length(unique_terms)
      l = length(terms) + 1
    }
  } else if (style == "chord"){
    t = length(terms) + 1
    l = t - 1
    term_loop <- terms[2:length(terms)]
  } else if (style == "weave"){
    t = 2
    l = length(terms) + length(treatment)
  }


  ## Create Empty Matrix
  results <- matrix(0, t*length(dv), 3 + l)

  ## Fill Matrix
  ### Row 1 (model number)
  results[,1] = dplyr::row_number(results[,1])

  ### Column 2 (populate DV)
  i=1
  if (style == "incremental"){
    for(i in i:length(dv)){
      results[((i*length(terms)) - (length(terms) - 1)):(i*length(terms)), 2] <- rep(dv[i], nrow(results)/length(dv))
    }
  } else if (style == "bivariate"){
    for(i in i:length(dv)){
      results[((i*length(terms)) - (length(terms) - 1)):(i*length(terms)), 2] <- rep(dv[i], nrow(results)/length(dv))
    }
  } else if (style == "chord"){
    for(i in i:length(dv)){
      results[((i*length(terms)) - (length(terms) - i)):((i*length(terms)) + i), 2] <- rep(dv[i], (nrow(results)/length(dv)))
    }
  } else if (style == "default"){
    if (length(unique_terms) == 0){
      for(i in i:length(dv)){
        results[i, 2] <- rep(dv[i], nrow(results)/length(dv))
      }
    } else if (length(unique_terms) != 0){
      for(i in i:length(dv)){
        results[((i * length(unique_terms)) - (length(unique_terms) - 1)):(i * length(unique_terms)), 2] <- rep(dv[i], nrow(results)/length(dv))
      }
    }
  } else if (style == "weave"){
    j = 1
    i = 1
    for(i in i:length(dv)){
      results[j:(j+1), 2] <- rep(dv[i], 2)
      i = i + 1
      j = i+(i-1)
    }
  }

  ### Column 3 : n terms
  i = 1
  j = 1
  if (style == "incremental"){
    results[ , 3] <- rep(terms[1], nrow(results))
    for(j in j:length(dv)){
      i = 1
      for(i in i:length(terms) - 1){
        results[seq(i+(j*length(terms))-(length(terms))+1, j * length(terms), by = 1), 3 + i] <- terms[1 + i]
      }
    }
  } else if (style == "bivariate"){
    for(i in i:length(terms)){
      results[seq(i, nrow(results), by = length(terms)), 3] <- rep(terms[i], length(seq(i, nrow(results), by = length(terms))))
    }
  } else if (style == "chord"){
    results[ , 3] <- rep(terms[1], nrow(results))
    for(j in j:length(dv)){
      i = 1
      for(i in i:length(term_loop)){
        results[seq(i+(j*length(terms))-(length(terms)) + j, (j * length(terms)) + j, by = length(terms) - i), 3 + i] <- terms[1 + i]
      }
    }
  } else if (style == "default"){
    for(i in i:length(terms)){
      results[ , 2 + i] <- rep(terms[i], nrow(results))
    }
    if (length(unique_terms) != 0){
      i = 1
      for(i in i:length(unique_terms)){
        results[seq(i, nrow(results), by = length(unique_terms)), length(terms) + 3] <-
          rep(unique_terms[i], length(seq(i, nrow(results), by = length(unique_terms))))
      }
    }
  } else if (style == "weave"){
    for(i in i:length(treatment)){
      results[ , 2 + i] <- rep(treatment[i], nrow(results))
    }
    i = 1
    for(i in i:length(terms)){
      results[seq(2, nrow(results), by = 2), 2 + length(treatment) + i] <- rep(terms[i], nrow(results)/2)
    }
  }

  ### Rename Columns
  results <- dplyr::as_tibble(results)
  colnames(results)[1] <- "model_number"
  colnames(results)[2] <- "dv"
  results$model_number <- as.integer(results$model_number)
  names(results)[3:(2+l)] <- paste("var", seq(1, l, by = 1), sep = "_")
  names(results)[ncol(results)]<-"lm"
  results$lm <- as.list(results$lm)

  ### Final Column - lm model
  loopnum = 0
  restart = 0
  j = 1
  z = 1
  if (style == "incremental"){
    for (j in dv){
      restart = 0
      for (z in terms){
        loopnum = loopnum + 1
        restart = restart + 1
        i_working <- terms[restart]
        if (restart == 1){
          i <- i_working
        } else {
          i <- stringr::str_c(i, i_working, sep = " + ")
        }
        results[loopnum, ncol(results)] <- lm_function()
        if(print_summary) tidy_summaries()
      }
    }
  } else if (style == "bivariate"){
    for (j in dv){
      for (i in terms){
        loopnum = loopnum + 1
        results[loopnum, ncol(results)] <- lm_function()

        if(print_summary) tidy_summaries()
      }
    }
  } else if (style == "chord"){
    for (j in dv){
      loopnum = loopnum + 1
      i = paste(terms[1])
      results[loopnum, ncol(results)] <- lm_function()

      if(print_summary == TRUE) tidy_summaries()

      for (z in term_loop){
        loopnum = loopnum + 1
        i = paste(terms[1], " + ", z)
        results[loopnum, ncol(results)] <- lm_function()

        if(print_summary) tidy_summaries()

        if (z == tail(term_loop, n = 1)){
          loopnum = loopnum + 1
          i <- paste(terms, collapse = " + ")
          results[loopnum, ncol(results)] <- lm_function()
          if(print_summary) tidy_summaries()
        }
      }
    }
  } else if (style == "weave"){
    full_terms <- results[,3:(ncol(results)-1)]
    for (z in 1:nrow(results)){
      loopnum = loopnum + 1
      j <- as.character(results[z,2])
      i <- paste(full_terms[z,], collapse=" + ")
      i <- str_remove_all(i, " \\+ 0")
      results[z, ncol(results)] <- lm_function()

      if(print_summary) tidy_summaries()
    }
  } else if (style == "default"){
    full_terms <- results[,3:(ncol(results)-1)]

    for (z in 1:nrow(results)){
      loopnum = loopnum + 1
      j <- as.character(results[z,2])
      i <- paste(full_terms[z,], collapse=" + ")
      results[loopnum, ncol(results)] <- lm_function()

      if(print_summary) tidy_summaries()
    }
  }

  # Replace 0 with NA
  results[results == "0"] <- NA_real_

  ## include clusters column if clusters exist (necessary for star_ready)
  if (is.null(clusters) == FALSE) {
    try(results$clusters <- clusters, silent = TRUE)
    results <- results %>% dplyr::select(1:(2 + l), clusters, lm)
  }

  # Extract Treatment Info
  i = 1
  if (is.null(treatment) == FALSE){
    if(stringr::str_detect(treatment, "\\*")) { # for interactions. note as of now, doesn't show interaction in df
      treatment <- unlist(stringr::str_split(treatment, "\\*"))
      treatment <- unique(treatment)
    }
    for(i in 1:length(treatment)) {
      has_backticks <- stringr::str_count(treatment[i], "`")
      treatment[i] <- stringr::str_remove_all(treatment[i], "`") # necessary for running applys
      # check for backticks factor
      test <- try(lapply(data[,treatment[i]], class) == "factor", silent = T)
      if (class(test) != "try-error"){
        if (lapply(data[,treatment[i]], class) == "factor") { # if there are multiple representations e.g., factors logical, typeof(data[,treatment[i]])
          treatment_factors <- stringr::str_c(treatment[i], sapply(data[,treatment[i]], levels)[-1])
          treatment_factors <- treatment_factors[-length(treatment_factors)] # don't take last
          if(has_backticks > 0){ # put them back
            treatment_factors <- str_replace_all(treatment_factors, treatment[i], stringr::str_c("`", treatment[i], "`"))
          }
          treatment <- append(treatment, treatment_factors, after = i)
          treatment <- setdiff(treatment, treatment[i])

        } else if (sapply(data[,treatment[i]], class)[1] == "logical"){  # check for backticks in logical
          if(has_backticks > 0){
            treatment[i] <- stringr::str_c("`", treatment[i], "`")
          }
          treatment[i] <- stringr::str_c(treatment[i], "TRUE")
        }
      }
    }
  }
  i = 1


  # print treatment info

  if (is.null(treatment) == FALSE){
    for(i in 1:length(treatment)) {

      ## populate treatment vars
      nam_coef <- paste(treatment[i], "coef",sep = "_")
      results[, nam_coef] <- NA_real_

      nam_ci_low <- paste(treatment[i], "ci_lower",sep = "_")
      results[, nam_ci_low] <- NA_real_

      nam_ci_upper <- paste(treatment[i], "ci_upper",sep = "_")
      results[, nam_ci_upper] <- NA_real_

      nam_se <- paste(treatment[i], "se",sep = "_")
      results[, nam_se] <- NA_real_

      nam_t <- paste(treatment[i], "t",sep = "_")
      results[, nam_t] <- NA_real_

      nam_p <- paste(treatment[i], "p",sep = "_")
      results[, nam_p] <- NA_real_



      j = 1
      for (j in 1:nrow(results)){
        take <- which(names(results[j, "lm"][[1]][[1]][[1]]) %in% treatment[i] == TRUE)

        try(lm_summary <- summary(results[j, "lm"][[1]][[1]]), silent = TRUE)
        ### get coeficients
        try(results[j, nam_coef] <- results[j, "lm"][[1]][[1]][[1]][take], silent = TRUE)

        ### get se
        try(results[j, nam_se] <- lm_summary$coefficients[,2][take], silent = TRUE)

        ### get t
        try(results[j, nam_t] <- lm_summary$coefficients[,3][take], silent = TRUE)

        ### get p values
        try(results[j, nam_p] <- lm_summary$coefficients[,4][take], silent = TRUE)

        if(simple_model == FALSE){
          ### get ci low
          try(results[j, nam_ci_low] <- lm_summary$coefficients[,5][take], silent = TRUE)
          ### get ci upper
          try(results[j, nam_ci_upper] <- lm_summary$coefficients[,6][take], silent = TRUE)
        } else if(simple_model == TRUE){
          ### get ci low
          try(results[j, nam_ci_low] <- results[j, nam_coef] - results[j, nam_se]*2, silent = TRUE) ## not adapted for other CI yet
          ### get ci upper
          try(results[j, nam_ci_upper] <- results[j, nam_coef] + results[j, nam_se]*2, silent = TRUE) ## not adapted for other CI yet
        }
      }
    }
  }
  results
}
