# blpl <img src="/sticker/imgfile.png" width="10%" height="10%" align="right">
Betsy Levy Paluck Lab R Package


This package is a collection of functions I wrote during my time as lab manager to facilitate transparent social science. This is a place where lab members can share code and functions they have written together, and learn how to contribute to open-source software.


There are loosely two groups of functions that lab members will find most useful. The first is the workhorse `tidy_lm` function with that runs hierarchical regressions with support for clustered standard errors.

`tidy_lm` can supports multiple "styles" of models. These are:

*  `default` uses all terms at once on all the DVs.
```
y_1 ~ x_1 + x_2 + x_3
y_2 ~ x_1 + x_2 + x_3
y_3 ~ x_1 + x_2 + x_3
```
* `incremental` starts off with one term and adds one more term each model for each dv.
```
y_1 ~ x_1
y_1 ~ x_1 + x_2 
y_1 ~ x_1 + x_2 + x_3
y_2 ~ x_1
y_2 ~ x_1 + x_2 
y_2 ~ x_1 + x_2 + x_3
```

* `chord` begins by running a bivariate regression the first term. Then runs a trivariate regression, holding the dv, first is term constant looping through the remaining terms. Lastly, all terms are ran together.

```
y_1 ~ x_1
y_1 ~ x_1 + x_2 
y_1 ~ x_1 + x_3
y_1 ~ x_1 + x_2 + x_3
y_2 ~ x_1
y_2 ~ x_1 + x_2 
y_2 ~ x_1 + x_3
y_2 ~ x_1 + x_2 + x_3
```

* `bivariate` uses all terms individually on each dv.
```
y_1 ~ x_1
y_1 ~ x_2 
y_1 ~ x_3
y_2 ~ x_1
y_2 ~ x_2 
y_2 ~ x_3
```


The second are custom hacks for creating publication quality tables. This is useful for editing tables created by `stargazer`, `knitr::kable` or `kableExtra`. These are numerous and mostly self-explanatory.

* `table_label` Adds dynamic labels to that can be references elsewhere in your .tex file or RMarkdown document via `\label{tab:your_label_here}`

* `add_endnote`  Improvement to footnotes from `stargazer` function with an additional `size_in_inches` argument to allow for custom sizes

* `stargazer_pvalues` the p-values in `stargazer` are clunky. This reformats the p-values to be 'brackets', 'parentheses' or 'none' to replace the `stargazer` default.

* `stargazer_rowname` add custom text to the first column in `stargazer`

* `print_table` A cleaner way to print LaTeX tables than simply using the `cat()`. Equivalent to `cat(paste(latex_table, collapse = "\n"))`

* `kable_resize` An option for when `kable_styling(full_width = F, latex_options = "scale_down")` isn't enough.

* `table_numbers` Adds custom numbers to a table. Useful for adding secondary counts to tables (e.g., Table 2a, Table 2b). Can also be used to switch primary counts to alternative styles like `alph` or `roman`. Requires `header needs \newcounter{foo}` to be added in document YAML to work under header-includes:

* `two_digits` Rounds numbers to the second digit, and returns them as a `character` in two digits. Useful for number consistency during visualization.

* `add_top_row` 

* `add_bottom_row` 

* `add_parentheses`

* `add_brackets`
 


To install:
```
install.packages("devtools")
devtools::install_github("johnhenrypezzuto/blpl")
```



