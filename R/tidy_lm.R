#' tidy_lm
#'
#' A function to streamline the process of running regressions in experimental psychology
#' @param dv Vector of one or more dependent variables. The function will regress all the terms on each dv separately
#' @param terms Vector of terms to regress on dv.
#' @param style Character string either "default", "incremental", or "single". In what way should the terms regress on dv. "default" uses all terms at once on all the DVs. "incremental" starts off with one term and adds one more term each model for each dv. "single" uses all terms individually on each dv.
#' @param treatment Vector for terms to include in the output dataset. Will return coefficent, p value, and SE.
#' @param clusters Clusters for grouping standard error.
#' @param print_summary Boolean whether to print model summaries or not. Default is FALSE.
#' @param data A data.frame
#'
#' @return  A tibble containing all linear models, and data on what variables are included in each model.

tidy_lm <- function(dv, terms, style = "default", treatment = NULL, clusters = NULL, print_summary = FALSE, data = .){

  # Check Inputs
  ## Check DV Variables
  for(i in 1:length(dv)) {
    test <- try(data[,dv[i]])
    if (class(test) == "try-error"){
      stop("'", dv[i], "'", " is not a variable in the dataset")
    }
  }
  ## No Matches In DV And Terms
  if (sum(dv %in% terms) >= 1){
    num <- which(dv %in% terms)
    stop("'",dv[num],"'", " is in both the dv and terms")
  }
  ## Check Terms Variables
  for(i in terms) {
    test <- try(lm(paste(dv[1], "~", i), data = data))
    if (class(test) == "try-error"){
      stop("'", i, "'", " is not a variable in the dataset")
    }
  }

  ## assign standard error type, and ready cluster to be printed
  if (is.null(clusters) == FALSE) {
    standard_error = "stata"
    cluster = paste0(", clusters = ", clusters)
    cluster_data <- data[, clusters]
  } else {
    standard_error = "classical"
    cluster = NULL
    cluster_data <- NULL
  }

  ## check that treatment is in terms
  if (length(treatment) != sum(treatment %in% terms)){
    stop("'", treatment[which((treatment %in% terms) == FALSE)], "'", " is included as a treatment, but not included as a term")
  }

  ## Check Style
  if (style == "incremental" | style == "single" || style == "default") {
  } else {
    stop("Style must be 'incremental', 'single' or 'default'")
  }

  # Tidy Summaries (if print_summary = TRUE)
  tidy_summaries <- function(){
    cat("\n################################################# ", loopnum)
    cat(paste0("\nlm_robust(", j, " ~ ", i, cluster, ", se_type = \"", standard_error,  "\")\n"))
    working_sum <- summary(results[loopnum, ncol(results)][[1]][[1]]) # 1 needs to switch to be current row, lm needs to be nrow
    cat(paste0("N = ", working_sum$N, "\n"))
    print(working_sum$coefficients)
    cat("\n")
    cat(paste0("F[", working_sum$fstatistic[2], ",",working_sum$fstatistic[3], "] = ", round(working_sum$fstatistic[1], 2),
               ", p = ", round(pf(working_sum$fstatistic[1],working_sum$fstatistic[2],working_sum$fstatistic[3],lower.tail=FALSE), 4), "\n"))
  }

  # Create Matrix
  ## Size of Matrix
  if (style == "incremental"){
    t = length(terms)
    l = t
  } else if (style == "single"){
    t = length(terms)
    l = 1  # length t in expanded version (didn't double check)
  } else if (style == "default"){
    t = 1
    l = length(terms)
  }

  ## Create Empty Matrix
  results = matrix(0, t*length(dv), 3 + l)

  ## Fill Matrix
  ### Row 1 (model number)
  results[,1] = dplyr::row_number(results[,1])

  ### Column 2 (populate DV)
  i=1
  if (style == "incremental"){
    for(i in i:length(dv)){
      results[((i*length(terms)) - (length(terms) - 1)):(i*length(terms)), 2] <- rep(dv[i], nrow(results)/length(dv))
    }
  } else if (style == "single"){
    for(i in i:length(dv)){
      results[((i*length(terms)) - (length(terms) - 1)):(i*length(terms)), 2] <- rep(dv[i], nrow(results)/length(dv))
    }
  } else if (style == "default"){
    for(i in i:length(dv)){
      results[i, 2] <- rep(dv[i], nrow(results)/length(dv))
    }
  }

  ### Column 3 : n terms
  i=1
  j = 1
  if (style == "incremental"){
    results[ , 3] <- rep(terms[1], nrow(results))
    for(j in j:length(dv)){
      i = 1
      for(i in i:length(terms) - 1){
        results[seq(i+(j*length(terms))-(length(terms))+1, j * length(terms), by = 1), 3 + i] <- terms[1 + i]
      }
    }
  } else if (style == "single"){
    for(i in i:length(terms)){
      results[seq(i, nrow(results), by = length(terms)), 3] <- rep(terms[i], length(seq(i, nrow(results), by = length(terms))))
    }
  } else if (style == "default"){
    for(i in i:length(terms)){
      results[ , 2 + i] <- rep(terms[i], nrow(results))
    }
  }

  ### Rename Columns
  results <- dplyr::as_tibble(results) %>%
    dplyr::rename("model_number"= V1,
                  "dv" = V2) %>%
    dplyr::mutate(model_number = as.integer(model_number))
  names(results)[3:(2+l)] <- paste("var", seq(1, l, by = 1), sep = "_")
  names(results)[ncol(results)]<-"lm"
  results$lm <- as.list(results$lm)

  ### Final Column - estimatr::lm_robust model
  loopnum = 0
  restart = 0
  z=1
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
        results[loopnum, ncol(results)] <- data %>% do(y = estimatr::lm_robust(as.formula(paste(j, "~", i)),
                                                                               data = data,
                                                                               clusters =  cluster_data,
                                                                               se_type = standard_error))
        if(print_summary == TRUE){
          tidy_summaries()
        }
      }
    }
  } else if (style == "single"){
    for (j in dv){
      for (i in terms){
        loopnum = loopnum + 1
        results[loopnum, ncol(results)] <- data %>% do(y = estimatr::lm_robust(as.formula(paste(j, "~", i)),
                                                                               data = data,
                                                                               clusters = cluster_data,
                                                                               se_type = standard_error))

        if(print_summary == TRUE){
          tidy_summaries()
        }
      }
    }
  } else if (style == "default"){
    i <- paste(terms,collapse=" + ")
    for (j in dv){
      loopnum = loopnum + 1
      results[loopnum, ncol(results)] <- data %>% do(y = estimatr::lm_robust(as.formula(paste(j, "~", i)),
                                                                             data = data,
                                                                             clusters = cluster_data,
                                                                             se_type = standard_error))
      if(print_summary == TRUE){
        tidy_summaries()
      }
    }
  }

  # Replace 0 with NA
  results[results == "0"] <- NA_real_

  # Extract Treatment Info
  i = 1
  if (is.null(treatment) == FALSE){
    for(i in 1:length(treatment)) {
      ## populate treatment vars
      nam_int <- paste(treatment[i], "int",sep = "_")
      results[, nam_int] <- NA_real_
      nam_p <- paste(treatment[i], "p",sep = "_")
      results[, nam_p] <- NA_real_
      nam_se <- paste(treatment[i], "se",sep = "_")
      results[, nam_se] <- NA_real_


      for (j in 1:nrow(results)){
        take <- which(names(results[j, "lm"][[1]][[1]][[1]]) %in% treatment[i] == TRUE)
        ### get intercepts
        try(results[j, nam_int] <- intercepts <- results[j, "lm"][[1]][[1]][[1]][take], silent = TRUE)

        ### get p values
        try(results[j, nam_p] <- summary(results[j, "lm"][[1]][[1]])$coefficients[,4][take], silent = TRUE)

        ### get se
        try(results[j, nam_se] <- summary(results[j, "lm"][[1]][[1]])$coefficients[,2][take], silent = TRUE)
      }
    }
  }
  return(results)
}
