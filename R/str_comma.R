
#' Collapse a vector of character strings into one readable string, separated by
#' commas and by "and"
#'
#' \code{str_comma} combines multiple character strings into one and makes them
#' more readable by separating those strings with commas and "and" as
#' appropriate. The place I had in mind for using this is in RMarkdown documents
#' where you want to insert a comment in the text that calls on a character
#' vector. For example, say you want to note in a text chunk which subjects in a
#' study have some measurement that is two standard deviations above the mean.
#' Rather than using only, e.g., \code{stringr::str_c(StudyOutliers, collapse =
#' ", ")}, you could have it include the "and" in the appropriate location and
#' make your final document more polished, readable, and professional.
#'
#'
#' @param x vector to collapse
#' @param oxford TRUE or FALSE for whether to use the Oxford comma
#' @param conjunction the conjunction to use to join the character strings.
#'   Options are "and" (default) and "or".
#'
#' @return Returns a single, collapsed character string
#' @export
#' @examples
#' str_comma(LETTERS[1:2])
#' str_comma(LETTERS[1:4])
#' str_comma(LETTERS[1:4], oxford = FALSE)
#'
#' 
str_comma <- function(x, oxford = TRUE, conjunction = "and"){
    
    conjunction <- tolower(conjunction)
    
    if(conjunction %in% c("and", "or") == FALSE){
        stop("You have attempted to use a word other than `and` or `or` as the conjunction, which is not permitted. Please set the conjunction to one of those and try again.",
             call. = FALSE)
    }
    
    if(length(x) <= 2){
        Out <- stringr::str_c(x, collapse = paste0(" ", conjunction, " "))
    } else {
        Last <- x[length(x)]
        Others <- x[1:(length(x) - 1)]
        Out <- stringr::str_c(
            paste0(c(stringr::str_c(Others, collapse = ", "),
                     conjunction, Last), collapse = " "))
        if(oxford){
            Out <- sub(" and| or", paste0(", ", conjunction), Out)
        }
    }
    
    return(Out)
}

