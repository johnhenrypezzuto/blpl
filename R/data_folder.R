#' Data Folder
#'
#' A function so that users don't need to change where data is loaded from when multiple users are working on a single project. Most likely nested inside the function read.csv() or read_csv().
#' @param data The name of the dataset to load as string.
#' @param folder The name where the dataset is stored one directory back. Directories "Data", "data", "dataset", "datasets" are checked automatically.
#' @param ws Should there be white space is preceding or trailing the name of the datafolder? Defaults to FALSE.
#' @export
#' @examples
#' read.csv(folder("mydataset.csv"))
data_folder <- function(data = "", folder = "",  ws = FALSE){
  # get root directory
  wd <- getwd()
  slashes <- stringr::str_locate_all(getwd(), "/")
  last_slash <- slashes[[1]][nrow(slashes[[1]]), 1]
  root_folder <- substr(wd, 1, last_slash)
  new_folder <- stringr::str_c(root_folder, folder)

    # handle white space
  if (stringr::str_length(folder) != 0){
    if (ws == TRUE) {
      folder <- as.character(folder)
    } else {
      folder <- trimws(as.character(folder))
    }
  }

  # handle dataset direct input
  if (stringr::str_length(data) > 0){
    if (stringr::str_detect(data, "\\.") == FALSE){
      stop("The dataset inputted does not contain .csv, .dta, or other data extensions")
    }
  }

  # handle empty input
  if (stringr::str_length(folder) == 0){
    for (folder in c("Data", "data", "dataset", "datasets")){
      folder <- folder

      #append folder name
      new_folder <- stringr::str_c(root_folder, folder)

      # test if empty
      if (length(list.files(new_folder)) >= 1){
        break
      } else {
        folder <- ""
        new_folder <- stringr::str_c(root_folder, folder)
      }
    }
  }

  #append folder name
  new_folder_data <- stringr::str_c(new_folder, data, sep = "/")
  # warning if empty directory
  if ((length(list.files(new_folder)) < 1) & (stringr::str_length(data) == 0)){
    warning("Data folder may be empty or misnamed. Proceed Carefully \n")
  }
  new_folder_data
}
