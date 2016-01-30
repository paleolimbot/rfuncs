#' Extract files matching a pattern
#' 
#' Extracts files matching a pattern from a directory, copying and/or removing
#' duplicate files (based on a user-supplied function) if specified. If a
#' destination directory is specified, files will be copied (and not
#' overwritten, even if filenames are identical).
#' 
#' @param source_dir the directory to look for the files
#' @param pattern the pattern of files (NULL for all files) to extract
#' @param destination_dir the directory to which files will be copied (NULL if
#'   files should not be copied)
#' @param remove_duplicates TRUE if duplicate files (based on the function supplied in unique_fun)
#'   should be removed from the list (they will not be deleted). If destination_dir is specified,
#'   the duplicate files will not be copied. If a file to be copied already exists in destination_dir
#'   (as determined by unique_fun), the file will not be copied but the name of the existing file will
#'   be included in the list of files returned.
#' @param unique_fun the function that will return a unique value for unique files. the most
#'   reliable is probably tools::md5sum() (the default), but for large files this is slow
#'   and file.size() may be adequate. In some cases basename() may also be sufficient.
#' @param verbose TRUE if status messages should be printed.
#'   
#' @return A list of file paths. If destination_dir is specified, the list of file paths will be
#'   to the copied files. If not, the list of file paths will be relative to the working directory.
#' 
#' @export
#' 
#' @examples
#' extract_files(getwd(), "*.*", remove_duplicates=TRUE)

extract_files <- function(source_dir, pattern, destination_dir=NULL,
                             remove_duplicates=FALSE, unique_fun=NULL,
                             verbose=TRUE) {
  if(is.null(unique_fun)) {
    unique_fun <- tools::md5sum
  }
  files <- NULL
  for(eachpattern in pattern) {
    files <- c(files, list.files(path=source_dir, pattern=eachpattern, 
                        recursive=TRUE, full.names = TRUE))
  }

  if(verbose) message("Found ", length(files), " files in ", source_dir, " matching ",
                      paste(pattern, collapse=", "))
  hashes <- NULL
  if(remove_duplicates) {
    if(verbose) message("Calculating duplicates...")
    hashes <- unique_fun(files)
    numdup <- sum(duplicated(hashes))
    if(verbose) message("Removing duplicates from list: found ", numdup, " duplicated and ",
                        length(hashes)-numdup, " unique files.")
    
    files <- files[!duplicated(hashes)]
    hashes <- hashes[!duplicated(hashes)]
  }
  
  if(!is.null(destination_dir)) {
    if(verbose) message("Copying to ", destination_dir)
    existinghashes <- NULL
    existingfiles <- NULL
    if(dir.exists(destination_dir)) {
      existingfiles <- list.files(destination_dir, full.names = TRUE)
      if(length(existingfiles) > 0) {
        if(remove_duplicates) {
          if(verbose) message("Non-empty destination directory specified: calculating hashes of ",
                              "files in destination directory so duplicates are not copied...")
          existinghashes <- unique_fun(existingfiles)
        } else {
          warning("Non-empty destination directory specified: files will not be overwritten but ",
                  "the directory should be cleaned before running this function")
        }
      }
    } else {
      dir.create(destination_dir, recursive=TRUE)
    }
    outfiles <- NULL
    for(i in 1:length(files)) {
      file <- files[i]
      if(!is.null(hashes) && !is.null(existinghashes) && !is.null(existingfiles)) {
        matchingfile <- existingfiles[which(hashes[i]==existinghashes)]

        if(length(matchingfile)>0) {
          if(verbose) message(file, " (", hashes[i], ") already in destination folder as ", basename(matchingfile),
                                   ", not copying")
          outfiles <- c(outfiles, matchingfile)
          next
        }
      }
      destination <- file.path(destination_dir, basename(file))
      prepend <- 2
      while(file.exists(destination)) {
        destination <- file.path(destination_dir, paste0(prepend, basename(file)))
        prepend <- prepend+1
      }
      file.copy(file, destination)
      outfiles <- c(outfiles, destination)
    }
    
    files <- outfiles
  }
  

  files
}
#test:
#extract_files("APPGISFiles", c(".JPG", "*.jpg"), remove_duplicates = TRUE, destination_dir = "testout")
