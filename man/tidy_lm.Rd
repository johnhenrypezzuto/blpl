% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_lm.R
\name{tidy_lm}
\alias{tidy_lm}
\title{tidy_lm}
\usage{
tidy_lm(data, dv, terms, style = "default", treatment = NULL,
  clusters = NULL, robust_se = FALSE, alpha = 0.05,
  print_summary = FALSE)
}
\arguments{
\item{data}{A \code{data.frame}}

\item{dv}{Vector of one or more dependent variables. The function will regress all the terms on each dv separately}

\item{terms}{Vector of terms to regress on dv.}

\item{style}{Character string either "default", "incremental", "chord", or "bivariate". In what way should the terms regress on dv.
\itemize{
\item "default" uses all terms at once on all the DVs.
\item "incremental" starts off with one term and adds one more term each model for each dv.
\item "chord" begins by running a bivariate regression the first term. Then runs a trivariate regression, holding the dv, first is term constant looping through the remaining terms.
Lastly, all terms are ran together.
\item "bivariate" uses all terms individually on each dv.
}}

\item{treatment}{Vector for terms to include in the output dataset. Returns coefficent, lower confidence interval,
upper confidence interval, standard error, t-value, and p-value.}

\item{clusters}{Clusters for grouping standard error.}

\item{robust_se}{TRUE/FALSE. Whether to use to use robust standard errors ("HC2"). Equivalent to default standard error from lm_robust(). Set to FALSE by default.}

\item{alpha}{Numeric p-value for the standard errors. Default .05.}

\item{print_summary}{TRUE/FALSE. Whether to print model summaries or not. Default is FALSE.}
}
\value{
A \code{tibble} containing all linear models, and data on what variables are included in each model.
}
\description{
A function to streamline the process of running regressions in experimental psychology
}
