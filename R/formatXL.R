#' Format data with font colors, font sizes, alignments, borders, etc. when you
#' write them to an Excel file
#'
#' This function takes a data.frame you want to save to an Excel file using the
#' package \code{xlsx} and applies the formatting you want for the Excel file.
#' \strong{Warning:} The arguments for this function are a lot of lists, and it
#' can be tricky to get the syntax exactly right, but I haven't yet come up with
#' a better way of doing things. If this isn't working, check that your lists
#' are laid out correctly and named correctly.
#'
#' @param DF input data.frame
#' @param file file name (character)
#' @param sheet sheet name (character). Defaults to the name of the supplied
#'   data.frame if no other name is supplied.

#' @param colWidth A named list of \code{colNum} or \code{colName} and
#'   \code{width} in pixels. Examples: \itemize{ \item \code{colWidth =
#'   list(colNum = c(1, 5, 6), width = 25)} \item \code{colWidth = list(colName
#'   = c("ColA", "ColG"), width = c(10, 30))}} If colNum and colName are set to
#'   NULL, all columns will be set to the width listed. For any columns not
#'   specifically set or if width is set to NULL, reasonable guesses for column
#'   widths will be used.

#' @param styles A list of lists (one list for every set of cells that you want
#'   to format) that contains the following named objects: \describe{
#'
#'   \item{\code{rows}}{A vector of the row indices for the cells whose
#'   formatting we're setting. Leaving \code{rows} unspecified applies the
#'   formatting to all rows in the sheet. \strong{Important note:} For
#'   consistency with row naming elsewhere in R, row 1 refers to the 1st row
#'   \emph{of data} in the data.frame, NOT the header. If you would like to
#'   apply the formatting to the header, set the row to 0. Examples:
#'   \itemize{\item \code{rows = 1:5} \item \code{rows = c(5, 23, 60)}}}
#'
#'   \item{\code{columns}}{A vector of the column indices or names for all the
#'   cells whose formatting we're setting. Leaving \code{columns} unspecified
#'   applies the formatting to all columns. Examples: \itemize{ \item
#'   \code{columns = c(1, 2, 8)} \item \code{columns = c("ColB", "ColH",
#'   "ColQ")}}}
#'
#'   \item{\code{numberFormat}}{The format for displaying any numbers in the
#'   cells. Options are "date", "general" (doesn't format anything), or
#'   "currency" (dollar sign included, negative numbers are red, and 2 digits
#'   included). Example: \itemize{\item \code{numberFormat = "currency"}}}
#'
#'   \item{\code{font}}{A named list of "color", "size", "bold", "italics",
#'   and/or "underline" to set the font style.
#'
#'   \describe{ \item{\code{color}}{Color options are standard color names s/a
#'   "red", "blue", "purple" or any of the possible named colors available in R.
#'   For a list of indexed color names, with the \code{xlsx} package loaded,
#'   type "INDEXED_COLORS_" into the console.}
#'
#'   \item{\code{size}}{Size is the font size, with typical values being 10 or
#'   11. Setting it to NULL means it will not change from the default.}
#'
#'   \item{\code{bold}, \code{italics}, \code{underline}}{The "bold", "italics",
#'   and "underline" values are logical and default to FALSE.}}
#'
#'   Examples: \itemize{ \item \code{font = list(color = "blue", bold = TRUE,
#'   italics = FALSE, underline = FALSE)} \item \code{font = list(color = "red",
#'   size = 16)}}}
#'
#'   \item{\code{textposition}}{A named list of "alignment" and "wrapping".
#'   \describe{ \item{\code{alignment}}{Alignment can be left, right, center, or
#'   general (all are vertically aligned to the bottom), or "middle" for
#'   vertical and horizontal alignment to the center.}
#'
#'   \item{\code{wrapping}}{Wrapping is TRUE or FALSE for whether the text
#'   should be wrapped.}} Examples: \itemize{ \item \code{textposition =
#'   list(alignment = "center", wrapping = TRUE)} \item \code{textposition =
#'   list(alignment = "right")}}}
#'
#'   \item{\code{border}}{A named list of color, position, and pen. \describe{
#'
#'   \item{\code{color}}{The same color options that worked for assigning the
#'   font color work for coloring the border.}
#'
#'   \item{\code{position}}{The position  can be "bottom", "left", "top", or
#'   "right".}
#'
#'   \item{\code{pen}}{The options for the pen setting, which is optional and
#'   defaults to a thin border, are "solid" ("BORDER_THIN" behind the scenes),
#'   "hair" for really thin hairline, "medium" for a medium-thickness line,
#'   "thick" for a thick line, "dashed", "dotted", and "none".}} Examples:
#'   \itemize{\item \code{border = list(color = "black", position = c("top",
#'   "bottom"))} \item \code{border = list(color = "red", position = "left", pen
#'   = "thick")}}}
#'
#'   \item{\code{fill}}{A color s/a "red", "blue", "purple" or any of the
#'   possible named colors available in R.}}
#'
#' @details If this will create a new Excel file or if your current Excel file
#'   has no other sheets but this one, this will generate a message "Workbook
#'   has no sheets!" that I \emph{cannot} seem to get rid of.
#'
#'   \strong{COMMON ERRORS or ERROR MESSAGES} since this is a tad (ok, a LOT)
#'   glitchy: \code{styles} is meant to accommodate multiple sets of formatting,
#'   so you have to have \code{styles} be a list but then also each set of cells
#'   that you're formatting must also be a list. This means that styles should
#'   probably look like this in your code: \code{styles = list(list(...))}
#'
#'   If you set a style for a cell and then set another style for that same
#'   cell, the final style will be the 2nd one, not a combination of the two.
#'   For example, say you select row 1 and make it blue and bold, and then you
#'   add a border on the left side of all cells in column 4. The cell in row 1,
#'   column 4 will be the default Excel style of text plus having a border on
#'   the left side - not blue, bold, \emph{and} border on left.
#'
#' @return This does not return any R objects; it saves an Excel file.
#' @examples
#' data(iris)
#' iris$Date <- as.Date("2020-11-03")
#' iris$Money <- rnorm(nrow(iris), 20, 20)
#'
#' formatXL(DF = iris, file = "test.xlsx", sheet = "iris1",
#'          colWidth = list(width = 30),
#'          styles = list(list(columns = 6,
#'                             numberFormat = "date"),
#'                        list(columns = 7,
#'                             numberFormat = "currency"),
#'                        list(rows = 0,
#'                             font = list(bold = TRUE, size = 18),
#'                             textposition = list(alignment = "middle",
#'                                                 wrapping = TRUE))))
#'
#' formatXL(DF = iris, file = "test.xlsx", sheet = "iris2",
#'          colWidth = list(colNum = c(1, 5, 6), width = c(25, 10, 30)),
#'         styles = list(list(rows = 4,
#'                             font = list(color = "blue", bold = TRUE)),
#'                        list(rows = 8:9, columns = 3,
#'                             font = list(color = "red", italics = TRUE),
#'                             textposition = list(alignment = "center",
#'                                                 wrapping = TRUE),
#'                             fill = "dodgerblue3"),
#'                        list(rows = 0, columns = 1,
#'                             font = list(color = "purple", underline = TRUE),
#'                             textposition = list(alignment = "right",
#'                                                 wrapping = TRUE))))
#'
#'
#' @return
#' @export
#'
#'
formatXL <- function(DF, file, sheet = NA,
                     colWidth = list(colNum = NULL,
                                     colName = NULL,
                                     width = NULL),
                     styles){

      # Defining pipe operator
      `%>%` <- magrittr::`%>%`

      ### Error catching input argument syntax problems
      # All the columns must be named for this to work well. Checking that.
      if(any(is.na(names(DF)))){
            stop("All the columns in your data.frame must be named.")
      }

      # Number format must by "general", "date", or "currency" only.
      if(!is.null(styles$numberFormat)){
            if(any(styles$numberFormat %in%
                   c("general", "date", "currency") == FALSE)){
                  stop("Options for number format are only 'general', 'date', and 'currency'. Please select one of those and make sure that you have spelled everything correctly.")
            }
      }

      # Font doesn't do anything if the items are anything other than "color",
      # "size", "bold", "italics", or "underline".
      if(!is.null(styles$font)){
            if(any(names(styles$font) %in% c("color", "size", "bold", "italics",
                                             "underline") == FALSE)){
                  stop("Options for the font are only 'color', 'size', 'bold', 'italics', and/or 'underline'. Please make sure that you're using those options and that you have spelled everything correctly.")
            }
      }


      ### Loading the file
      # See whether that file already exists. If it does, load the workbook.
      # Otherwise, create it.
      AllFiles <- list.files(pattern = ".xlsx")
      if(length(AllFiles) == 0){
            WB <- xlsx::createWorkbook(type = "xlsx")
      } else {
            if(file %in% AllFiles){
                  WB <- xlsx::loadWorkbook(file)
            } else {
                  WB <- xlsx::createWorkbook(type = "xlsx")
            }
      }

      rm(AllFiles)

      # If nothing is supplied for the "sheet" argument, the sheet name will
      # default to the name of the DF.
      if(is.na(sheet)){
            sheet <- as.character(substitute(DF))
      }

      # Check whether the sheet already exists. If it does, remove it.
      AnySheets <- xlsx::getSheets(WB)
      if(!is.null(AnySheets)){
            if(sheet %in% names(xlsx::getSheets(WB))){
                  xlsx::removeSheet(WB, sheetName = sheet)
            }
      }

      NewSheet <- xlsx::createSheet(WB, sheetName = sheet)
      SheetRows <- xlsx::createRow(NewSheet, rowIndex = 1:(nrow(DF) + 1))
      xlsx::addDataFrame(DF %>% as.data.frame(),
                         sheet = NewSheet, row.names = FALSE)

      # Getting all the cells in that object and then their names
      AllCells <- xlsx::getCells(SheetRows)
      AllCellNames <- data.frame(AllNames = names(AllCells),
                                 stringsAsFactors = FALSE) %>%
            tidyr::separate(AllNames, c("Row", "Column"), "\\.", remove = FALSE)

      StylesToApply <- list()

      for(i in 1:length(styles)){

            ### Selecting the cells

            # If they haven't specified rows or columns, apply to all cells.
            if(is.null(styles[[i]]$rows) &
               is.null(styles[[i]]$columns)){
                  MyCells <- AllCellNames %>%  dplyr::pull(AllNames)
            } else {
                  # If they've specified both rows and columns, apply to those
                  # cells.
                  if(!is.null(styles[[i]]$rows) &
                     !is.null(styles[[i]]$columns)){
                        MyCells <- AllCellNames %>%
                              dplyr::filter(Row %in% (styles[[i]]$rows + 1) &
                                                  Column %in% styles[[i]]$columns) %>%
                              dplyr::pull(AllNames)
                  } else {
                        # If they've specified the columns but not the rows,
                        # apply to all rows in that column.
                        if(is.null(styles[[i]]$rows)){
                              MyCells <- AllCellNames %>%
                                    dplyr::filter(Column %in% styles[[i]]$columns) %>%
                                    dplyr::pull(AllNames)
                        } else {
                              # If they've specified the rows but not the
                              # columns, apply to all columns in that row.
                              MyCells <- AllCellNames %>%
                                    dplyr::filter(Row %in% (styles[[i]]$rows + 1)) %>%
                                    dplyr::pull(AllNames)
                        }
                  }
            }

            ### Setting number format
            if(is.null(styles[[i]]$numberFormat)){
                  NumFormatArgs <- NULL
            } else {
                  if(styles[[i]]$numberFormat == "general"){
                        styles[[i]]$numberFormat <- NULL
                        NumFormatArgs <- NULL
                  } else {

                        NumFormatOptions <-
                              data.frame(Input = c("date", "currency"),
                                         Output = c("m/d/yyy",
                                                    "$#,##0.00_);[Red]($#,##0.00)"))

                        NumFormatArgs <-
                              xlsx::DataFormat(NumFormatOptions %>%
                                                     dplyr::filter(Input == styles[[i]]$numberFormat) %>%
                                                     dplyr::pull(Output))
                        # See
                        # https://www.excelhowto.com/macros/formatting-a-range-of-cells-in-excel-vba/
                        # for more examples of number formats.
                  }
            }

            ### Setting font preferences
            # Filling in defaults for anything unspecified for font.
            if(is.null(styles[[i]]$font)){
                  styles[[i]]$font <- list(color = "#030303",
                                           bold = FALSE,
                                           italics = FALSE,
                                           underline = FALSE)
            }

            styles[[i]]$font$color <- ifelse(is.null(styles[[i]]$font$color),
                                             "#030303", styles[[i]]$font$color)
            styles[[i]]$font$color <- ifelse(styles[[i]]$font$color == "black",
                                             "#030303", styles[[i]]$font$color)
            # Note of explanation for the above: There is a glitch somewhere
            # behind the scenes such that, when you set the font color to black,
            # it's actually getting set to white. NO IDEA how to fix this, and
            # it took me a bit of frustration to even figure out that that was
            # what was happening. My workaround: Set the font color to the
            # darkest possible gray.

            styles[[i]]$font$bold <- ifelse(is.null(styles[[i]]$font$bold),
                                            FALSE, styles[[i]]$font$bold)

            styles[[i]]$font$italics <- ifelse(is.null(styles[[i]]$font$italics),
                                               FALSE, styles[[i]]$font$italics)

            styles[[i]]$font$underline <- ifelse(is.null(styles[[i]]$font$underline),
                                                 FALSE, styles[[i]]$font$underline)


            ### Setting alignment
            # Filling in defaults for anything unspecified for alignment
            if(is.null(styles[[i]]$textposition)){
                  styles[[i]]$textposition <- list(alignment = "general",
                                                   wrapping = FALSE)
            }

            styles[[i]]$textposition$alignment <-
                  ifelse(is.null(styles[[i]]$textposition$alignment),
                         "general",
                         styles[[i]]$textposition$alignment)

            styles[[i]]$textposition$wrapping <-
                  ifelse(is.null(styles[[i]]$textposition$wrapping),
                         FALSE,
                         styles[[i]]$textposition$wrapping)

            if(styles[[i]]$textposition$alignment == "middle"){
                  AlignArg <- xlsx::Alignment(horizontal = "ALIGN_CENTER",
                                              vertical = "VERTICAL_CENTER",
                                              wrapText = styles[[i]]$textposition$wrapping)
            } else {
                  AlignArg <-
                        xlsx::Alignment(horizontal =
                                              paste0("ALIGN_",
                                                     toupper(styles[[i]]$textposition$alignment)),
                                        wrapText = styles[[i]]$textposition$wrapping)
            }

            ### Setting the fill
            if(is.null(styles[[i]]$fill)){
                  FillArg <- xlsx::Fill(pattern = "NO_FILL")
            } else {
                  FillArg <- xlsx::Fill(foregroundColor = styles[[i]]$fill)
            }

            ### Setting the border
            if(is.null(styles[[i]]$border)){
                  BorderArg <- xlsx::Border(pen = "BORDER_NONE")
            } else {
                  BorderArg <-
                        xlsx::Border(color = ifelse(is.null(styles[[i]]$border$color),
                                                    "black", styles[[i]]$border$color),
                                     position = toupper(styles[[i]]$border$position),
                                     pen = paste0("BORDER_",
                                                  toupper(
                                                        ifelse(styles[[i]]$border$pen ==
                                                                     "solid",
                                                               "thin", styles[[i]]$border$pen))))
            }


            ### Applying the styles the user has set

            StylesToApply[[i]] <-
                  xlsx::CellStyle(WB,
                                  font = xlsx::Font(WB, color = styles[[i]]$font$color,
                                                    isItalic = styles[[i]]$font$italics,
                                                    underline = ifelse(styles[[i]]$font$underline,
                                                                       1, 0),
                                                    heightInPoints = styles[[i]]$font$size,
                                                    isBold = styles[[i]]$font$bold),

                                  alignment = AlignArg,

                                  fill = FillArg,

                                  border = BorderArg,

                                  dataFormat = NumFormatArgs)


            for(j in MyCells){
                  xlsx::setCellStyle(cell = AllCells[[j]],
                                     cellStyle = StylesToApply[[i]])
            }

            rm(MyCells, FillArg, BorderArg)
      }

      # Setting column widths

      # If they've specified column width, then check which columns to apply
      # that to.
      if(!(is.null(colWidth$width))){
            # If width contains some info but neither colName nor colNum are
            # specified, then set colNum to 1:ncol(DF) so that we can then apply
            # the specified width to ALL columns.
            if(is.null(colWidth$colName) & is.null(colWidth$colNum)){
                  colWidth$colNum <- 1:ncol(DF)
            } else {
                  # If the column name is specified, then apply the width to
                  # that column.
                  if(!is.null(colWidth$colName)){
                        for(k in colWidth$colName){
                              colWidth$colNum <- which(names(DF) == k)
                        }
                        rm(k)
                  }
            }
      }


      # error catching
      if(length(colWidth$width) != 1 &
         length(colWidth$width) != length(colWidth$colNum)){
            stop("There must be either only 1 value for the column width or there must be the same number of values for column width as there are numbers of values for column number or name.",
                 call. = TRUE)
      }

      # For any columns whose width is *not* set explicitly, guess at a
      # reasonable width and set the column width to that.

      # Noting which columns are already set and their widths.
      if(length(colWidth$colNum) > 0){
            ColAlreadySet <- data.frame(colIndex = colWidth$colNum,
                                        colWidth = colWidth$width)
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

      # Replacing GoodWidths for the previously set column widths with those
      # widths.
      if(exists("ColAlreadySet")){
            for(i in ColAlreadySet$colIndex){
                  GoodWidths[i] <- ColAlreadySet$colWidth[
                        ColAlreadySet$colIndex == i]
            }
      }

      for(k in 1:length(GoodWidths)){
            xlsx::setColumnWidth(NewSheet, colIndex = k,
                                 colWidth = GoodWidths[k])
      }
      # Finally, saving.
      xlsx::saveWorkbook(WB, file)

}




