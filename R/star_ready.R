#' Star Ready
#' Prepares output from `tidy_lm` for use with the package stargazer
#'
#' @param tidy_lm_df A `tibble` from the `tidy_lm` function. No columns can be missing or out of the original order. Rows can be appended so long as they use the same data.
#' @param data Dataframe with corresponding data that was used in the tidy_lm function.
#' @export
#' @return Returns a tibble with all lms that can be directly used with stargazer.
#tidy_lm_df <- four_a_
star_ready <- function(tidy_lm_df, data){

  # define grouped cluster_fun
  cluster_fun <- function(model, cluster) {
    vcovCL <- multiwayvcov::cluster.vcov(model, cluster) # return variance covariance matrix under clustered SEs
    coef <- lmtest::coeftest(model, vcovCL) # the clustered SEs by indicating the correct var-covar matrix
    return(coef)   # return coef test
  }

  ## shrink tidy df
  tidy_lm_short <- tidy_lm_df %>%
   # dplyr::select(1:"dv") %>%
    dplyr::select(dv, dplyr::starts_with("var_"))

  ## Create Empty Tibble
  results <- matrix(0, nrow(tidy_lm_df), 1)
  results <- dplyr::as_tibble(results)
  colnames(results) <- "lm"
  results$lm <- as.list(results$lm)

  ## fill with lm
  z = 1
  for (z in 1:nrow(results)){
    j <- tidy_lm_short$dv[z] # take dv
    terms <- tidy_lm_short[z,-c(1)] # take terms
    i <- paste(terms,collapse=" + ")
    i <- stringr::str_remove_all(i, " \\+ NA")
    results[z,1] <- data %>% do(y = lm(paste(j, "~", i), data = data)) # redo model
    results$lm[[z]]$call[[2]][2] <- tidy_lm_df$dv[z] # fix dv
  }
  output <- results

  # give models new SE based on se_type
  z = 1
  for (z in 1:nrow(results)){
    if(tidy_lm_df["lm"][[1]][[z]]$se_type == "classical") {
      #output$lm[[z]]$call[[2]][2] <- tidy_lm_df$dv[z] # fix dv
    }
    if(tidy_lm_df["lm"][[1]][[z]]$se_type == "stata") {
      obj <- dplyr::tribble(~model, cluster_fun(results[z, "lm"][[1]][[1]], cluster = data[,tidy_lm_df$clusters[z]]))
      output[z, 1] <- obj
      #
      # output[z, 1] <- data %>% do(y = list(model = obj,
      #                                  call = results$lm[[z]]$call))
      #output[[z]] <- output$lm[z]
      #output$lm[[1]]$call[[2]][2] <- tidy_lm_df$dv[z] need to get dvs automatically
    }
    if(tidy_lm_df["lm"][[1]][[z]]$se_type == "HC2") {
      obj <- dplyr::tribble(~model, lmtest::coeftest(results[z,"lm"][[1]][[1]],
                                              vcov = sandwich::vcovHC(results[z,"lm"][[1]][[1]], "HC2")))
      output[z, 1] <- obj
    }
  }
  output <- output[[1]]
  output
}

# it would be preferable to make this more flexible to work with multiple data sets instead of only 2!!
# make it more cluster compatible
join_tidy_lm <- function(model_a, model_b){
  model_a <- subset(model_a, select=c(model_number:lm))
  model_b <- subset(model_b,select=c(model_number:lm))
  model_a_vars <- model_a %>% dplyr::select(starts_with("var_")) %>% length()
  model_b_vars <- model_b %>% dplyr::select(starts_with("var_")) %>% length()

  if(model_a_vars != model_b_vars){
    if (model_a_vars > model_b_vars){
      n_add <- model_a_vars - model_b_vars # number to add # names(results)[3:(2+l)]
      newcols <- as.data.frame(matrix(NA_real_, nrow = nrow(model_b), ncol = n_add))
      names(newcols)<- paste("var", (model_b_vars + 1):(model_b_vars +  n_add), sep = "_")

      # NOTE this is implying clusters right now based on -2 instead -1, and -1 in
      output <- cbind(model_b[,1:(ncol(model_b)-2)],
                      newcols, model_b[,(ncol(model_b)-1):ncol(model_b)])
      output <- rbind(model_a, output)
    } else if (model_a_vars < model_b_vars){
      n_add <- model_b_vars - model_a_vars # number to add
      newcols <- as.data.frame(matrix(NA_real_, nrow = nrow(model_a), ncol = n_add))
      names(newcols)<- paste("var", (model_a_vars + 1):(model_a_vars +  n_add), sep = "_")
      # NOTE this is implying clusters right now based on -2 instead -1, and -1 in
      output <- cbind(model_a[,1:(ncol(model_a)-2)],
                      newcols, model_a[,(ncol(model_a)-1):ncol(model_a)])
      output <- rbind(output, model_b)
    }
  } else if(model_a_vars == model_b_vars){
    output <- rbind(model_a_vars, model_b_vars)
  }
  output
}
