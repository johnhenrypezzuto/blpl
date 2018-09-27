#' Add Parentheses
#'
#' Adds parenthesis to input
#' @param vector
#'
#' @examples
#' add_parentheses(45)
#' (45)
#'
add_parentheses <- function(vector){
  out <- as.vector(paste0("(", vector, ")"))
  out
}

#' Add Brackets
#'
#' Adds brackets to input
#' @param vector
#'
#' @return
#' @export
#'
#' @examples
#' add_brackets(45)
#'  [45]
add_brackets <- function(vector){
  out <- as.vector(paste0("[", vector, "]"))
  out
}

#' Two Digits
#'
#' Rounds numbers two the second digit, and returns them as a `character` in two digits. Useful for number consistency during visualization.
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#' two_digits(4.5664)
#' "4.57"
#' two_digits(0.1)
#' "0.10"
two_digits <- function(data){
  out <- as.vector(trimws(format(round(data, 2), nsmall = 2)))
  out
}

#' Add First Row
#'
#' Add an empty row on top of a
#' @param data
#'
#' @return
#' @export
#'
add_first_row <- function(data){
  out <- rbind(NA_real_, data)
  out
}


#' Add Last Row
#'
#' Adds an empty row to the bottom of a `dataframe`
#' @param data
#'
#' @return
#' @export
#'
add_last_row <- function(data){
  out <-rbind(data, NA_real_)
  out
}

#' Nothing
#'
#' Does nothing. Useful as an anchor on the bottom row while editing using `%>%`.
#' @param data
#'
#' @return
#' @export
#'
nothing <- function(data) data


#' Table Label
#'
#' Adds labels to LaTeX tables that have are stored as a vector (e.g. output from `kable()` or `stargazer()` which is stored by `capture.output()`). Labels cannot be seen within the table,
#' but rather can be referenced within a rmarkdown document via `\label{tab:your_label_here}`
#' @param latex_table Table from `capture.output()`
#' @param caption
#'
#' @return
#' @export
#'
#' @examples
#'
#' my_table_rough <- capture.output(stargazer(mtcars))
#' my_table <- table_label(my_table_rough, "demographics")
#' print_table(table)
#'
#' # Within Markdown Document
#' `\label{tab:demographics}` # this will print as Table 1, and update depending on the order of the tables.
table_label <- function(latex_table, caption){
  # if label exists
  labelnum <- which(stringr::str_detect(latex_table, "\\\\label\\{\\w{3}:\\w*\\}") == TRUE)[1]
  if(!is.na(labelnum)){
    latex_table <- stringr::str_replace(latex_table, "(\\\\label\\{\\w{3}:)\\w*\\}", stringr::str_c("\\1", caption, "\\}"))
    # if caption exists
  } else if (is.na(labelnum)){
    stringr::str_replace(latex_table, "(\\caption\\{.*\\})", stringr::str_c(" \\1", "\\\\label\\{tab:", caption, "\\}"))
  } else {
    stop("Could not find existing caption or label")
  }
  latex_table
}

#' Undo Closure
#'
#' Removes one character from each side of a `string`. Useful for undoing functions like `add_parentheses` or `add_brackets`.
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#' undo_closure("(45)")
#' "45"
undo_closure <- function(data){
  out <- stringr::str_sub(data, 2, stringr::str_length(data)-1)
  out
}



#' Add Endnote
#'
#' Improvement to footnotes from `stargazer` function. Results vary with output from `kable`, although the kable endnotes by default tend to be better formatted.
#' Used with `capture.output()`. When using, notes made with the original function should not be used.
#'
#' @param table
#' @param note Endnote you want to add to the bottom of the table
#' @param size_in_inches  How many inches long the endnote should be
#' @param rm.stargazer.stars TRUE/FALSE. Whether to remove the default stargazer Note: that includes the starvalues.
#'
#' @return
#' @export
#'
#' @examples
#'
#' my_table_rough <- capture.output(stargazer(mtcars))
#' my_table <- add_endnote(table = my_table_rough, note = "my endnote is very long", size_in_inches = 6, rm.stargazer.stars = TRUE)
#' print_table(table)
add_endnote <- function(table,  note, size_in_inches = 6, rm.stargazer.stars = TRUE){
  # erase first note
  if (rm.stargazer.stars == TRUE){
    original_note <- which(stringr::str_detect(table, "\\\\textit\\{Note:\\}"))
    table[original_note] <- " "
  }
  # add new note
  placement <- which(stringr::str_detect(table, "\\\\end\\{tabular\\}"))
  table[placement] <- stringr::str_c(table[placement],"\\begin{tabular}{  p{", size_in_inches, "in} }",
                                     "\\footnotesize{", note, "}", "\\end{tabular}")
  if (length(placement) == 0){
    placement <- which(stringr::str_detect(table, "\\\\end\\{table\\}"))
    table[placement] <- stringr::str_c("\\begin{tabular}{  p{", size_in_inches, "in} }",
                                       "\\footnotesize{", note, "}", "\\end{tabular}", table[placement])
  }


  table
}


#' Stargazer P Values
#'
#'The default way for stargazer to report P-Values is by "P = X". This function offers several ways to change this.
#' @param table Table from `stargazer` using the `capture.output()` function
#' @param format String either 'brackets', 'parentheses' or 'none' to replace the stargazer default P = x
#'
#' @return
#' @export
#'
#' @examples
#' model <- lm(hp ~ wt, mtcars)
#' table_rough <- stargazer(model, report = "csp")
#' table <- stargazer_pvalues(table_rough, "brackets")
#' print_table(table)
stargazer_pvalues <- function(table, format){
  if (format == "brackets"){
    left = "\\["
    right = "]"
  } else if (format == "parentheses"){
    left = "\\("
    right = ")"
  } else if (format == "none"){
    left = ""
    right = ""
  } else {
    stop("Format does not match 'brackets', 'parantheses', or 'none' ")
  }

  table <- stringr::str_replace_all(table, "p = (\\d+(\\.\\d{1,2})?)\\d* &", stringr::str_c(left, "\\1", right, " &"))
  table <- stringr::str_replace_all(table, "p = (\\d+(\\.\\d{1,2})?)\\d* \\\\", stringr::str_c(left, "\\1", right, "\\\\"))
  table
}


#' Stargazer Rowname
#'
#' Manually adds rownames to in first column
#'
#' @param table
#' @param row_name Name for row
#' @param row_number Which row number to add the name to. Best determined by looking at the object created by `capture.output()`
#'
#' @return
#' @export
#'
stargazer_rowname <- function(table, row_name, row_number){
  table[rownumber] <- stringr::str_c(row_name, table[row_number])
}

#' Print Table
#'
#' A cleaner way to print LaTeX tables than simply using the `cat()`.
#' Equivalent to `cat(paste(latex_table, collapse = "\n"))`
#'
#' @param latex_table
#'
#' @return
#' @export
#'
#' @examples
#' table <- capture.output(kable(mtcars, format = "latex"))
#' print_table(table)
print_table <- function(latex_table){
  cat(paste(latex_table, collapse = "\n"))
}


#' Kable Resize
#'
#'An option for when `kable_styling(full_width = F, latex_options = "scale_down")` isn't enough. This function depends on you already attempting to resize using the aforementioned function.
#' When using with endnotes, I recommend to use this function with `kableExtra`'s built in, so that the resize corrosponds with the table
#'footnote function
#' @param table
#' @param resizebox
#' @param minipage
#'
#' @return
#' @export
#'
#' @examples
kable_resize <- function(table, resizebox = .8, minipage = 1){
  placement <- stringr::str_detect(table,"\\\\resizebox\\{")
  table[placement] <- stringr::str_c("\\resizebox{", resizebox, "\\linewidth}{!}{ \\begin{minipage}{", minipage, "\\linewidth}")

  # close minipage
  ending <- which(stringr::str_detect(table, "\\\\end\\{tabular\\}"))
  table[ending] <- stringr::str_remove(table5_rough[ending], "\\}") # take out resize curly bracket
  table[ending] <- stringr::str_c(table[ending], "\\end{minipage}}") # close minipage, and reinstate curly bracket

  table
}



