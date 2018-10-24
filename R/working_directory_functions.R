#' Data Folder
#'
#' A function so that users don't need to change where data is loaded from when multiple users are working on a single project. Assumes the data is going to be one folder behind the current directory. Most likely nested inside the function read.csv() or read_csv().
#' @param file The name of the dataset to load as string.
#' @param folder The name where the dataset is stored one directory back. Directories "Data", "data", "dataset", "datasets" are checked automatically.
#' @param steps_back An integer labeling how many steps back from your current wd is the data folder located? Set to 1 by default.
#' @param ws Should there be white space is preceding or trailing the name of the datafolder? Defaults to FALSE.
#' @export
#' @examples
#' read.csv(data_folder("mydataset.csv"))

data_folder <- function(file = "", folder = "", steps_back = 1, ws = FALSE){

  # prepare steps_back
  if (!is.numeric(steps_back)){
    stop("steps_back needs to be an integer!")

  } else if (steps_back < 0){
    steps_back <- abs(steps_back)
      }
  steps_back <- steps_back - 1


  # get root directory
  wd <- getwd()
  if (steps_back == -1){
    root_folder = wd
  } else {
  slashes <- stringr::str_locate_all(getwd(), "/")
      if (steps_back > nrow(slashes[[1]])){
        stop("Can't take that many steps back!")
       }
  last_slash <- slashes[[1]][nrow(slashes[[1]]) - steps_back, 1]
  root_folder <- substr(wd, 1, last_slash)
    }
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
  # if (stringr::str_length(data) > 0){
  #   if (stringr::str_detect(data, "\\.") == FALSE){
  #     stop("The dataset inputted does not contain .csv, .dta, or other data extensions")
  #   }
  # }

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

  # append folder name
  if (stringr::str_length(file) == 0){
  new_folder_data <- new_folder
  } else {
    new_folder_data <- stringr::str_c(new_folder, file, sep = "/")
  }

  # warning if empty directory
  if ((length(list.files(new_folder)) < 1) & (stringr::str_length(file) == 0)){
    warning("Data folder may be empty or misnamed. Proceed Carefully \n")
  }
  new_folder_data
}


simple_wd <- function(wd){


}
