#' @title extract_acoustics_from_file
#'
#' @description Function to extract scientific name and path of each mp3 recording and path of every CSV output.
#'
#' @param file the mp3 file where we want to extract the scientific name and path
#' @param csv_dir the folder where the file is
#'
#' @return run the extrac_acoustics function
#' @export
#' 

extract_acoustics_from_file <- function(file, csv_dir) {
  basename <- path_file(file)
  name <- strsplit(basename, ".mp3")
  csv_file <- paste(csv_dir, "/", name, '.csv', sep="")
  # Extracting species scientific name
  scientific_name <- unlist(strsplit(as.character(name), '_-_'))[3]
  extract_acoustics(file, scientific_name, csv_file)
}
