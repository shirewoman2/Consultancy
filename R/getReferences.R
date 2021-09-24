#' Pull references from a "DataRec" folder
#'
#' Get a list of all the pdf, Word, and Excel files in, e.g., the "DataRec"
#' folder and subfolder. Can be useful for the bibliography section of a report.
#'
#' @param main_directory the main directory from which you want a list of all
#'   the files
#'
#' @return A data.frame containing columns "Dir" for the directory and, where
#'   applicable, the subdirectory of a file, and "File" for the file name.
#'
#' @export
#'
#' @examples
#'
#' main_directory <- "\\\\s08sharepoint.certara.com@SSL/DavWWWRoot/sites/consult/dndi-1a/DataRec"
#' getReferences(main_directory)
#'
#'
getReferences <- function(main_directory){

      RefDirs <- list.dirs(main_directory)
      RefDirs <- RefDirs[!str_detect(RefDirs, "DataRec/Forms")]

      Refs <- list()

      for(i in RefDirs){

            MyFiles <- list.files(i, pattern = "pdf$|doc$|docx$|xls",
                                  include.dirs = FALSE,
                                  recursive = FALSE,
                                  full.names = FALSE)

            Refs[[i]] <- data.frame(Dir = i,
                                    File = as.character(unlist(MyFiles)) )

            rm(MyFiles)
      }

      Refs <- bind_rows(Refs) %>%
            mutate(Dir = sub("\\.\\./DataRec", "DataRec", Dir)) %>%
            arrange(Dir, File)

      return(Refs)
}


