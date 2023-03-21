#' Convert a page from a pdf file to a csv file
#'
#' \code{pdf_to_csv} takes as input a pdf file and a page number and converts
#' that to a data.frame and, optionally, saves that to a csv file. This can be
#' useful for getting client-supplied data into a useful format. NOTE: This
#' requires the package pdftools, which you may have to download and install
#' with \code{install.packages("pdftools")}. You only have to do that once.
#'
#' @param pdf_file the pdf file you want to read, in quotes, e.g.,
#'   \code{pdf_file = "Table data that I want to graph but is in pdf form.pdf"}
#' @param page page number(s) of the pdf you want to read as a numeric vector.
#'   For example, to read page 10, use \code{page = 10}. To read pages 1, 3, and
#'   5 and put them into a single csv file: \code{page = c(1, 3, 5)}. To read
#'   pages 9 through 12: \code{page = 9:12}. If you use multiple pages, keep in
#'   mind that, if the columns don't line up well in the pdf, they also won't
#'   line up well in the output csv file.
#' @param save_csv optionally specify a file name for saving the output. If left
#'   as "csv", the file name will be the pdf file name plus the 1st page number
#'   requested. If set to NA, no file will be saved.
#'
#' @return a data.frame of pdf table content
#' @export
#'
#' @examples
#' # No examples yet.
#' 
pdf_to_csv <- function(pdf_file, 
                       page, 
                       save_csv = "csv"){
    
    # Error catching ----------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
    }
    
    # Check for appropriate input for arguments
    if(length(pdf_file) != 1){
        stop("You must supply a single file name for the argument `pdf_file` for the pdf_to_csv function to work.", 
             call. = FALSE)
    }
    
    if(str_detect(pdf_file, "\\.pdf") == FALSE){
        pdf_file <- paste0(pdf_file, ".pdf")
    }
    
    # if(length(page) > 1){
    #     warning("The function pdf_to_csv can only accommodate one page at a time (at least for now), and you have supplied more than one value. We'll only use the first value.", 
    #             call. = FALSE)
    #     page <- page[1]
    # }
    
    # Ignore any NA values in the page numbers.
    page <- page[complete.cases(page)]
    
    if(class(page) != "integer" | all(is.na(page))){
        stop("You must supply a numeric value for the page.", 
             call. = FALSE)
        page <- as.numeric(page)
    }
    
    # Main body of function -------------------------------------------------
    readPage <- function(singlepage){
        Out <- pdftools::pdf_data(pdf_file)[[singlepage]] %>% 
            arrange(y, x) %>%                        #sort in reading order
            mutate(group = cumsum(!lag(space, default = 0))) %>%  #identify text with spaces and paste
            group_by(group) %>% 
            summarise(x = first(x),
                      y = first(y),
                      text = paste(text, collapse = " ")) %>% 
            group_by(y) %>% 
            mutate(colno = row_number()) %>%         #add column numbers for table data 
            ungroup() %>% 
            select(text, colno, y) %>% 
            pivot_wider(names_from = colno, values_from = text) %>% #pivot into table format
            select(-y)
        names(Out) <- paste0("Col", 1:ncol(Out))
        return(Out)
    }
    
    Pages <- list()
    for(i in page){
        Pages[[i]] <- readPage(i)
    }
    
    Out <- bind_rows(Pages)
    
    # Check whether they just supplied "csv" for output file name
    if(str_detect(sub("\\.", "", save_csv), "^csv$")){
        save_csv <- sub("\\.?pdf", paste0(" - pg ", page[1], ".csv"), pdf_file)
        
    } else {
        # If they supplied something other than just "csv", then check whether
        # that file name is formatted appropriately.
        if(str_detect(basename(save_csv), "\\..*")){
            if(str_detect(basename(save_csv), "\\.csv") == FALSE){
                # If they specified a file extension that wasn't csv, make that
                # file extension be .csv
                save_csv <- sub("\\..*", ".csv", save_csv)
            }
        } else {
            # If they didn't specify a file extension at all, make it .csv. 
            save_csv <- paste0(save_csv, ".csv")
        }
    }
    
    if(complete.cases(save_csv)){
        write.csv(Out, file = save_csv, row.names = FALSE)
    }
    
    return(Out)
    
}

