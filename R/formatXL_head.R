#' Save an Excel file with standard formatting for the header row
#'
#' This function saves a data.frame to an Excel file with the header row in bold
#' face, centered both horizontally and vertically, and with wrapped text. It
#' also sets the column widths to a reasonable guess based on the data you're
#' writing. For more details, please see \code{\link{formatXL}}.
#'
#' @param DF input data.frame
#' @param file file name (character)
#' @param sheet sheet name (character). Defaults to the name of the supplied
#'   data.frame if no other name is supplied.
#'
#' @return This does not return any R objects; instead, it saves an Excel file.
#' @examples
#' data(iris)
#'
#' formatXL_head(DF = iris, file = "test.xlsx", sheet = "iris1")
#'
#'
#' @return
#' @export
#'
#'
formatXL_head <- function(DF, file, sheet = NA){

      # Defining pipe operator
      `%>%` <- magrittr::`%>%`

      # All the columns must be named for this to work well. Checking that.
      if(any(is.na(names(DF)))){
            stop("All the columns in your data.frame must be named.")
      }

      # Guessing at appropriate column width based on max number of characters
      # in that column. First, need to include headers as a row so that it will
      # count those.
      DFwithHead <- DF %>% dplyr::mutate_all(as.character) %>%
            rbind(names(DF))

      Nchar <- DFwithHead %>%
            dplyr::summarize_all(function(x) max(nchar(as.character(x)), na.rm = TRUE)) %>%
            as.numeric()

      # For anything where the max number of characters is > 15, look for spaces
      # or hyphens to separate words and then find the max number of characters
      # in a word. If there are a lot of words, we'll want the column to be
      # wider than if there aren't as many.

      splitWords <- function(x){
            max(as.numeric(sapply(stringr::str_split(x, " |-"), nchar)))
      }

      XWide <- which(Nchar > 15)
      Nchar_word <- Nchar

      # If the column is not included in XWide (the extra-wide columns), then
      # set NumWord to 1.
      NumWord <- rep(1, length(names(DF)))
      for(i in XWide){
            Nchar_word[i] <- mean(sapply(apply(DFwithHead[i], MARGIN = 1, splitWords),
                                         as.vector), na.rm = TRUE)

            NumWord[i] <- max(apply(DFwithHead[i], MARGIN = 1, countWords),
                              na.rm = TRUE)
            # If there are at least 3 words, you probably want to see the first
            # three, so multiplying Nchar_word by 3 to allow for that. If there
            # are fewer words, just set it to the number of characters.
            Nchar_word[i] <- ifelse(NumWord[i] >= 3,
                                    Nchar_word[i] * 3, Nchar[i])
      }

      # Check whether the column class for anything with words was originally
      # POSIXct or Date b/c those have a lot of hyphens but no actual words.
      Classes <- sapply(DF, class)
      for(i in 1:length(NumWord)){
            Nchar_word[i] <- ifelse(Classes[[i]][1] %in%
                                          c("POSIXct", "POSIXlt", "Date"),
                                    Nchar[i], Nchar_word[i])
            NumWord[i] <- ifelse(Classes[[i]][1] %in%
                                       c("POSIXct", "POSIXlt", "Date"),
                                 NA, NumWord[i])
      }

      # Words in the header should NOT be split up, so Nchar_word should
      # be at least as large as the largest word in the header.
      Header_nchar <- as.numeric(sapply(names(DF), splitWords))
      for(i in 1:length(Header_nchar)){
            Nchar_word[i] <- ifelse(Header_nchar[i] > Nchar_word[i],
                                    Header_nchar[i], Nchar_word[i])
      }

      # Using 10 pixels for values < 10, 15 for values from 10 to 15, 20 for
      # values up to 30 characters and then 30 pixels for values even larger.
      GoodWidths <- cutNumeric(as.numeric(Nchar_word),
                               breaks = c(0, 10, 15, 20, 30, 100, 1000))
      GoodWidths[which(GoodWidths > 30)] <- 30

      # However, if there were more than 5 words for that column, set the column
      # width to 25 or the original width it came up with, whichever is wider.
      # If there were more than 20 words in that column, set the column width to
      # 50.
      for(i in 1:length(NumWord[which(NumWord > 5)])){
            GoodWidths[which(NumWord > 5)][i] <-
                  ifelse(GoodWidths[which(NumWord > 5)][i] > 25,
                         GoodWidths[which(NumWord > 5)][i], 25)
      }
      GoodWidths[which(NumWord > 20)] <- 50

      # Setting column widths, applying styles, and saving.
      formatXL(DF, file = file, sheet = sheet,
               colWidth = list(colNum = NULL, width = GoodWidths),
               styles = list(list(rows = 0,
                                  font = list(bold = TRUE),
                                  textposition = list(alignment = "middle",
                                                      wrapping = TRUE))))

}




