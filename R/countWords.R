#' Count the number of words in each item in a vector or list
#'
#' This function determines how many words are in a string; it assumes that
#' words are separated by either a space or a hyphen.
#'
#' @param x A vector or list. This can be any type of data.
#' @import stringr
#' @return Returns a vector of numbers that is the number of words present
#' @export
#'
#' @examples
#'
#' countWords("My cats' names are Lightning and Theo.")
#' countWords(list("My cats' names are Lightning and Theo.",
#'               "mein hindee seekh rahi hoon", 4.5, TRUE,
#'               "Pharmacogenetic variation in drug-metabolizing enzymes"))
#'
countWords <- function(x){

      # Checking whether there are any words
      WordsFound <-  stringr::str_detect(x, " |-")
      Count <- rep(NA, length(x))
      for(i in 1:length(x)){
            if(is.na(WordsFound[i])){
                  Count[i] <- 1
            } else {
                  if(WordsFound[i]){
                        Count[i] <- sapply(stringr::str_split(x[i], " |-"), length)
                  } else {
                        Count[i] <- 1
                  }
            }
      }

      return(Count)
}


