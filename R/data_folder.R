#' Data Folder
#'
#' A function so that users don't need to change where data is loaded from when multiple users are working on a single project. Most likely nested inside the function read.csv() or read_csv().
#' @param dataset The name of the dataset to load as string.
#' @param datafolder The name where the dataset is stored one directory back. Directories "Data", "data", "dataset", "datasets" are checked automatically.
#' @param ws Should there be white space is preceding or trailing the name of the datafolder? Defaults to FALSE.
#' @examples
#' read.csv(data_folder("mydataset.csv"))
data_folder <- function(dataset = "", data_folder = "",  ws = FALSE){
  # get root directory
  wd <- getwd()
  slashes <- stringr::str_locate_all(getwd(), "/")
  last_slash <- slashes[[1]][nrow(slashes[[1]]), 1]
  root_folder <- substr(wd, 1, last_slash)

    # handle white space
  if (ws == TRUE){
    data_folder <- as.character(data_folder)
  } else {
    data_folder <- trimws(as.character(data_folder))
  }

  # handle dataset direct input
  if (stringr::str_length(dataset) > 0){
    if (stringr::str_detect(dataset, "\\.") == FALSE){
      stop("The dataset inputted does not contain .csv, .dta, or other data extensions")
    }
  }

  # handle empty input
  if (stringr::str_length(data_folder) == 0){
    for (folder in c("Data", "data", "dataset", "datasets")){
      data_folder <- folder

      #append folder name
      new_data_folder <- str_c(root_folder, data_folder)

      # test if empty
      if (length(list.files(new_data_folder)) >= 1){
        return(new_data_folder)
      } else {
        data_folder <- ""
        new_data_folder <- stringr::str_c(root_folder, data_folder)
      }
    }
  }

  #append folder name
  new_data_folder <- stringr::str_c(root_folder, data_folder, dataset)

  # warning if empty directory
  if ((length(list.files(new_data_folder)) < 1) & (stringr::str_length(dataset) == 0)){
    warning("Data folder may be empty or misnamed. Proceed Carefully")
  }
  return(new_data_folder)
}
