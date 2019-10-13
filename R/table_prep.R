#' Add Parentheses
#'
#' Adds parenthesis to input
#' @param vector
#'
#' @return
#' @export
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

#' Add Top Row
#'
#' Add an empty row on top of a `dataframe`
#' @param data
#'
#' @return
#' @export
#'
add_top_row <- function(data, value = NA_real_){
  out <- rbind(value, data)
  out
}


#' Add Bottom Row
#'
#' Adds an empty row to the bottom of a `dataframe`
#' @param data
#'
#' @return
#' @export
#'
add_bottom_row <- function(data, value = NA_real_){
  out <-rbind(data, value)
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
  labelnum <- which(stringr::str_detect(latex_table, "\\label\\{.*\\}"))[1]
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
  # most of string
  table <- stringr::str_replace_all(table, "p = (\\d+(\\.\\d{1,2})?)\\d* &", stringr::str_c(left, "\\1", right, " &"))
  # end of string
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
#' When using with endnotes, I recommend to use this function with `kableExtra`'s built in, so that the resize corresponds with the table
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


## Perhaps renumber can be improved with some variation of
# usepackage_latex <- function(name, options = NULL) {
#   invisible(knit_meta_add(list(latex_dependency(name, options))))
# }


#' Renumber LaTeX Table in a document
#'
#' Requires `header needs \newcounter{foo}` to be added in document YAML to work under header-includes:. Useful for adding secondary
#' counts to tables (e.g., Table 2a, Table 2b). Can also be used to switch primary counts to alternative styles like `alph` or `roman`.
#'
#' @param latex_table
#' @param hold_primary_count Stops the first counter from ascending
#' @param rm_primary_count Does not show primary count
#' @param rm_secondary_count Removes secondary count
#' @param reset_secondary_counter Resets the seconary counter back to 1
#' @param primary_count_style `Character` vector that should be equal to `alph`, `Alph`, `arabic`, `roman`, or `Roman`.
#' @param secondary_count_style `Character` vector that should be equal to `alph`, `Alph`, `arabic`, `roman`, or `Roman`.
#'
#' @return
#' @export
#'
#' @examples
table_numbers <- function(latex_table, hold_primary_count = TRUE, rm_primary_count = FALSE,
                          rm_secondary_count = FALSE, reset_secondary_counter = FALSE,
                          primary_count_style = "arabic", secondary_count_style = "alph"){
  # find beginning of table
  labelnum <- which(stringr::str_detect(latex_table, "\\\\begin\\{table\\}") == TRUE)
  if(length(labelnum) == 0){
    stop("Could not find beginning of table")
  }

  # define standard counter
  counter = "\\stepcounter{foo}"

  # check if primary_count_style is correct
  if (primary_count_style == "alph" | primary_count_style == "Alph" |
      primary_count_style == "arabic" |primary_count_style ==  "roman" |
      primary_count_style == "Roman"){
  } else {stop("primary_count_style must be 'alph', 'Alpha', 'arabic', 'roman', or 'Roman'")
  }

  # check if secondary_count_style is correct
  if (secondary_count_style == "alph" | secondary_count_style == "Alpha" |
      secondary_count_style == "arabic" |  secondary_count_style == "roman" |
      secondary_count_style == "Roman"){
    secondary_count_style <- stringr::str_c(secondary_count_style, "{foo}")
  } else {
    stop("secondary_count_style must be 'alph', 'Alpha', 'arabic', 'roman', or 'Roman'")
  }


  if (reset_secondary_counter == TRUE){
    counter = "\\setcounter{foo}{0}"
  }

  if (hold_primary_count == TRUE){
    tnum <- "\\addtocounter{table}{-1}"
  } else if (hold_primary_count == FALSE){
    tnum <- " "
  }

  if (rm_primary_count == TRUE){
    table_number <- " "
  } else if (rm_primary_count == FALSE){
    table_number <- stringr::str_c("\\", primary_count_style, "{table}\\")
  }
  new_number <- stringr::str_c(tnum, counter, "\\renewcommand{\\thetable}{", table_number, secondary_count_style,"}")
  latex_table[labelnum] <- stringr::str_c(latex_table[labelnum], new_number)
  latex_table
}



#' Italicize a Word in LaTeX Tables.
#'
#' Currently set up to be used in papaja documents.
#'
#' @param latex_table
#' @param word `string` of word to italicize
#'
#' @return
#' @export
#'
#' @examples
word_italicize <- function(latex_table, word){
  labelnum <- which(stringr::str_detect(latex_table, word) == TRUE)
  if(length(labelnum) == 0){
    stop("Could not find string match in table")
  }
  latex_table[labelnum] <- stringr::str_replace(latex_table[labelnum], word, stringr::str_c("\\textit{", word, "}"))
}


partial_header_line <- function(latex_output){
  number_line <- which(str_detect(latex_output, "\\((\\d?\\d\\))\\\\\\\\"))[1]
  n_models <- as.numeric(str_extract(str_extract(latex_output[number_line], "\\((\\d?\\d\\))\\\\\\\\"), "(\\d?\\d)"))
  latex_output[number_line] <- str_c("\\cline{2-",n_models+1,"} ", latex_output[number_line]) # OR \\cmidrule(lr){2-
  latex_output
}
