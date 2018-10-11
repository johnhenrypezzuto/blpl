#' Star Ready
#' Prepares output from `tidy_lm` for use with the package stargazer
#'
#' @param tidy_lm_df A `tibble` from the `tidy_lm` function. No columns can be missing or out of the original order. Rows can be appended so long as they use the same data.
#' @param data Dataframe with corresponding data that was used in the tidy_lm function.
#' @export
#' @return Returns a tibble with all lms that can be directly used with stargazer.

star_ready <- function(tidy_lm_df, data = .){

  # define grouped cluster_fun
  cluster_fun <- function(model, cluster) {
    vcovCL <- multiwayvcov::cluster.vcov(model, cluster) # return variance covariance matrix under clustered SEs
    coef <- lmtest::coeftest(model, vcovCL) # the clustered SEs by indicating the correct var-covar matrix
    return(coef)   # return coef test
  }

  ## shrink tidy df
  tidy_lm_short <- tidy_lm_df %>%
    dplyr::select(1:"dv") %>%
    dplyr::select(dv, dplyr::starts_with("var_"))

  ## Create Empty Tibble
  results <- matrix(0, nrow(tidy_lm_df), 1)
  results <- dplyr::as_tibble(results)
  colnames(results) <- "lm"
  results$lm <- as.list(results$lm)

  ## fill with lm
  z = 1
  for (z in 1:nrow(results)){
    j <- tidy_lm_short$dv[z]
    terms <- tidy_lm_short[z,2:ncol(tidy_lm_short)]
    i <- paste(terms,collapse=" + ")
    i <- stringr::str_remove_all(i, " \\+ NA")
    results[z,1] <- data %>% do(y = lm(paste(j, "~", i), data = data))
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
