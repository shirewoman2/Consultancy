#' Format tables according to Simcyp Consultancy Team specifications
#'
#' \code{formatTable_Simcyp} makes a nicely formatted table from a data.frame or
#' tibble. It was primarily designed to work with output from
#' \code{\link{pksummary_table}} and \code{\link{pksummary_mult}}, so, by
#' default, it formats tables so that the column headings and the first column
#' are bold, and the second through the last columns are centered. Column
#' headings with, e.g., "AUCinf" or "Cmax" will have the "inf" or the "max"
#' subscripted, and the table will automatically expand to fit the contents. You
#' can save the output to a Word file with the argument \code{save_table}.
#'
#' @param DF a data.frame or a flextable, usually output from
#'   \code{\link{pksummary_table}} or \code{\link{pksummary_mult}}
#' @param fontsize the numeric font size for the output table. Default is 11
#'   point.
#' @param shading_column If you would like to alternate the shading of the rows
#'   in the output table, supply here the unquoted name of the column to check
#'   for when to change the shading; every time that column's value changes, the
#'   shading will alternate between white and light gray. For example, if you
#'   have a table with PK values for multiple files and you have more than one
#'   row per file (an example of this would be the output from the function
#'   \code{\link{pksummary_mult}}), setting \code{shading_column = File} will
#'   cause the shading of the rows to alternate between white and light gray
#'   whenever the file changes. Please see the examples at the bottom of this
#'   help file.
#' @param merge_shaded_cells TRUE (default) or FALSE for whether to merge the
#'   cells that have the same shade. This only applies when one of the columns
#'   in the input data.frame is used for deciding when to alternate shading,
#'   that is, \code{shading_column} has a value.
#' @param merge_columns a vector of quoted column names or of numeric column
#'   positions that should be merged vertically whenever the values are the
#'   same. For example, \code{merge_columns = c("File", "Tissue")} will cause
#'   the cells in the columns "File" and "Tissue" to merge vertically whenever
#'   the same value shows up in consecutive rows. Similarly, \code{merge_columns
#'   = c(1, 3, 5)} will merge vertically the 1st, 3rd, and 5th columns whenever
#'   the values are the same. Note: This is different from most other functions
#'   in the SimcypConsultancy package, which require unquoted column names.
#'   Honestly, we just don't know how code things for you to supply a variable
#'   number of unquoted column names for a single argument; we've just hit a
#'   coding knowledge limitation here!
#' @param bold_cells optionally specify cells in the table to be in bold-face
#'   text with a numeric vector where the 1st number is the row number and the
#'   2nd number is the column number (just like regular row and column
#'   specifications in R). For example, \code{bold_cells = c(1, 2)} will make
#'   the cell in row 1 and column 2 bold face. Use "0" for the row number if you
#'   want to use bold face for something in the header row, and use NA in place
#'   of a row or column number to make everything in that row or column bold
#'   face. If you want to specify multiple places to use bold face, use a list
#'   of numeric vectors. By default, the header row and the 1st column will be
#'   bold. Set \code{bold_cells = NA} to make \emph{nothing} bold. Please see
#'   the examples at the bottom of the help file.
#' @param center_1st_column TRUE or FALSE (default) for whether to make the
#'   alignment of the first column centered
#' @param prettify_columns TRUE (default) or FALSE for whether to make easily
#'   human-readable column names for any columns with PK parameters. TRUE makes
#'   pretty column names such as "Dose 1 AUCinf (h*ng/mL)" whereas FALSE leaves
#'   the column with the R-friendly name from \code{\link{extractPK}}, e.g.,
#'   "AUCinf_dose1".
#' @param sort_column optionally specify a column to sort by. If none are
#'   supplied, the table will not be sorted. If you would like to sort by more
#'   than one column, we recommend sorting \emph{before} using this function,
#'   e.g., \code{MyPKTable <- MyPKTable \%>\% arrange(Study, Dose)} to sort by
#'   the column "Study" and then by the column "Dose" and \emph{then} supply
#'   "MyPKTable" to \code{formatTable_Simcyp}. (This is just an example; your
#'   table must include those two columns for that to work.)
#' @param highlight_so_cutoffs optionally specify cutoffs for highlighting any
#'   simulated-to-observed ratios. Anything that is above those values or below
#'   the inverse of those values will be highlighted. To figure out what cells
#'   to highlight, this looks for a column titled "Statistic" or "Stat", then
#'   looks for what row contains "S/O" or "simulated (something something)
#'   observed" (as in, we'll use some wildcards to try to match your specific
#'   text). Next, it looks for any values in that same row that are above those
#'   cutoffs. This overrides anything else you specified for highlighting. The
#'   default is NA, for \emph{not} highlighting based on S/O value. Acceptable
#'   input for, say, highlighting values that are > 125\% or < 80\% of the
#'   observed and also, with a second color, values that are > 150\% or < 66\%
#'   would be: \code{highlight_so_cutoffs = c(1.25, 1.5)}. If you would like the
#'   middle range of values to be highlighted, include 1 in your cutoffs. For
#'   example, say you would like everything that's < 80\% or > 125\% to be
#'   highlighted red but you'd like the "good" values from 80\% to 125\% to be
#'   green, you can get that by specifying
#'   \code{highlight_so_cutoffs = c(1, 1.25)} and \code{highlight_so_colors =
#'   c("green", "red")}
#' @param highlight_gmr_colors optionally specify a set of colors to use for
#'   highlighting geometric mean ratios for DDIs. Options are "yellow to red",
#'   "green to red", "traffic" (a more vivid version of "green to red"), or a
#'   vector of 4 colors of your choosing. If left as NA, no highlighting for GMR
#'   level will be done.
#' @param highlight_so_colors optionally specify a set of colors to use for
#'   highlighting S/O values outside the limits you specified with
#'   \code{highlight_so_cutoffs}. Options: \describe{
#'
#'   \item{"yellow to red" (default)}{A range of light yellow to light orange to
#'   light red. If you have included 1 in your cutoffs and you leave
#'   \code{highlight_so_colors} with the default setting, values in the middle,
#'   "good" range of S/O values will be highlighted a light green.}
#'
#'   \item{"traffic"}{light green, yellow, and red designed to display values
#'   outside 1.25, 1.5, and 2 fold of unity, respectively. If you include 1 in
#'   \code{highlight_so_cutoffs}, you'll get a darker green for "good" S/O
#'   values. This color scheme was borrowed from Lisa, so if you've seen her
#'   slides, these will look familiar.}
#'
#'   \item{a character vector of specific colors}{Any R-acceptable colors, will
#'   work here, e.g., \code{highlight_so_colors = c("yellow", "orange", "red")}}.
#'   If you do specify your own bespoke colors, you'll need to make sure that
#'   you supply one color for every value in \code{highlight_so_cutoffs}.}
#' @param highlight_cells optionally specify cells in the table to be
#'   highlighted with a numeric vector where the 1st number is the row number
#'   and the 2nd number is the column number (just like regular row and column
#'   specifications in R). For example, \code{highlight_cells = c(1, 2)} will
#'   make the cell in row 1 and column 2 highlighted. Use "0" for the row number
#'   if you want to highlight something in the header row, and use NA in place
#'   of a row or column number to highlight everything in that row or column. If
#'   you want to specify multiple places to highlight, use a list of numeric
#'   vectors. Please see the examples at the bottom of the help file.
#' @param highlight_color color to use for highlighting; default is yellow.
#'   Color can be specified using any R-friendly color name or hex code, e.g.,
#'   "red" or "#D8212D".
#' @param save_table optionally save the output table by supplying a file name
#'   in quotes here, e.g., "My nicely formatted table.docx".  Do not include any
#'   slashes, dollar signs, or periods in the file name. If you leave off the
#'   file extension, we'll assume you want it to be ".docx". If there is a
#'   column titled "File" in your table, we'll add a caption listing which files
#'   were included.
#' @param title_document optionally specify a title for the Word document
#'   output. If you don't save the table, this will be ignored.
#' @param table_caption optionally add some text for a table caption. If the
#'   table you supply contains a column titled "File", there will already be a
#'   caption listing the source files; this would add some additional text
#'   before that.
#' @param add_header_for_DDI TRUE (default) or FALSE for whether to add an extra
#'   header row to the top of your table denoting when the PK are for baseline,
#'   with a perpetrator, or are the geometric mean ratios.
#' @param perpetrator_name the name of any perpetrator that is included in a PK
#'   table. This is only used when \code{add_header_for_DDI} is TRUE. It looks
#'   for the name of the perpetrator you specify and uses that in the top, extra
#'   row where the table is labeled as being the PK at baseline, with your
#'   perpetrator, or GMRs. If we don't know what the perpetrator drug name is,
#'   it's really hard to do that just right.
#' @param page_orientation set the page orientation for the Word file output to
#'   "portrait" (default) or "landscape" 
#'
#' @return a formatted table
#' @export
#'
#' @examples
#' MyPKTable <- tibble(Statistic = c("Simulated", "CV%", "Observed", "S/O"),
#'                         AUCinf = c(2756, 32.5, 1801, 1.53),
#'                         Cmax = c(852, 45.8, 775, 1.1),
#'                         `Half life` = c(7.75, 5.7, 6.05, 1.28))
#' formatTable_Simcyp(MyPKTable)
#' formatTable_Simcyp(MyPKTable, center_1st_column = TRUE)
#' formatTable_Simcyp(MyPKTable, fontsize = 18)
#' formatTable_Simcyp(MyPKTable, shading_column = Statistic)
#'
#' # Highlighting examples
#' ## Highlighting S/O values outside bioequivalence of 125%.
#' formatTable_Simcyp(MyPKTable,
#'                    highlight_so_cutoffs = 1.25)
#'
#' ## Highlighting S/O values with a few more colors based on the S/O.
#' formatTable_Simcyp(MyPKTable,
#'                    highlight_so_cutoffs = c(1.25, 1.5))
#'
#' ## Highlighting S/O values and shading the "good" values green.
#' formatTable_Simcyp(MyPKTable,
#'                    highlight_so_cutoffs = c(1, 1.25, 1.5))
#'
#' ## Highlight exactly the cells you want, e.g., row 1, column 2
#' formatTable_Simcyp(MyPKTable, highlight_cells = c(1, 2))
#'
#' ## Highlight all of column 2
#' formatTable_Simcyp(MyPKTable, highlight_cells = c(NA, 2))
#'
#' ## Highlight all of row 1
#' formatTable_Simcyp(MyPKTable, highlight_cells = c(1, NA))
#'
#' ## Highlight the 2nd column in the header
#' formatTable_Simcyp(MyPKTable, highlight_cells = c(0, 2))
#'
#' ## Set the highlight color to light blue instead of yellow
#' formatTable_Simcyp(MyPKTable, highlight_cells = c(1, NA),
#'                    highlight_color = "lightblue")
#'
#' ## Highlighting multiple cells
#' formatTable_Simcyp(MyPKTable, highlight_cells = list(c(1, 2), c(3,3), c(4, 2)),
#'                    highlight_color = "lightblue")
#'
#' # Bold-face examples
#' ## Make only the cell in row 4 and column 2 be bold face. This will
#' ## override the default of having the header row and the 1st column in bold.
#' formatTable_Simcyp(MyPKTable, bold_cells = c(4, 2))
#'
#' ## Make the cell in row 4 and column 2 be bold face AND include the original
#' ## defaults of having the header row and the 1st column be in bold.
#' formatTable_Simcyp(MyPKTable, bold_cells = list(c(0, NA), c(NA, 1), c(4, 2)))
#'
#' # Saving
#' ## Adding a column called "File" so that there will be a caption in the Word
#' ## document listing which files were included in the table. Also setting
#' ## the document title.
#' MyPKTable$File <- "abc-1a.xlsx"
#' formatTable_Simcyp(MyPKTable,
#'                    highlight_so_cutoffs = c(1, 1.25, 1.5),
#'                    save_table = "My data.docx",
#'                    title_document = "PK data")
#'
#' 


formatTable_Simcyp <- function(DF, 
                               fontsize = 11, 
                               shading_column, 
                               merge_shaded_cells = TRUE,
                               merge_columns = NA, 
                               sort_column, 
                               bold_cells = list(c(0, NA), c(NA, 1)),
                               center_1st_column = FALSE,
                               add_header_for_DDI = TRUE, 
                               perpetrator_name = "perpetrator", 
                               prettify_columns = FALSE, 
                               highlight_gmr_colors = NA, 
                               highlight_so_cutoffs = NA, 
                               highlight_so_colors = "yellow to red",
                               highlight_cells = NA, 
                               highlight_color = "yellow",
                               save_table = NA, 
                               page_orientation = "portrait", 
                               title_document = NA, 
                               table_caption = NA){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run\nlibrary(tidyverse)\n    ...and then try again.", 
           call. = FALSE)
   }
   
   page_orientation <- tolower(page_orientation)[1]
   if(page_orientation %in% c("portrait", "landscape") == FALSE){
      warning("You requested something other than `portrait` or `landscape` for the page orientation in the Word output, and those are the only options. We'll use the default of `portrait`.\n", 
              call. = FALSE)
      page_orientation <- "portrait"
   }
   
   TemplatePath <- switch(page_orientation, 
                          "landscape" = system.file("Word/landscape_report_template.dotx",
                                                    package="SimcypConsultancy"), 
                          "portrait" = system.file("Word/report_template.dotx",
                                                   package="SimcypConsultancy"))
   
   if("flextable" %in% class(DF)){
      
      FT <- DF
      
      # Format the file name appropriately, including making the extension be
      # docx, even if they specified something else.
      save_table <- ifelse(str_detect(save_table, "\\..*$"), 
                           sub("\\..*", ".docx", save_table), 
                           paste0(save_table, ".docx"))
      
      # Now that the file should have an appropriate extension, check what
      # the path and basename should be.
      OutPath <- dirname(save_table)
      save_table <- basename(save_table)
      
      # May need to change the working directory temporarily, so
      # determining what it is now
      CurrDir <- getwd()
      
      OutPath <- dirname(save_table)
      if(OutPath == "."){
         OutPath <- getwd()
      }
      
      FileName <- basename(save_table)
      
      rmarkdown::render(system.file("rmarkdown/templates/savetablesimcyp/skeleton/skeleton.Rmd",
                                    package="SimcypConsultancy"), 
                        output_format = rmarkdown::word_document(reference_docx = TemplatePath), 
                        output_dir = OutPath, 
                        output_file = FileName, 
                        quiet = TRUE)
      # Note: The "system.file" part of the call means "go to where the
      # package is installed, search for the file listed, and return its
      # full path.
      
      return(FT)
      
   }
   
   if("data.frame" %in% class(DF) == FALSE){
      stop("Please check your input. The `formatTable_Simcyp` function only works with data.frames or flextables, and it looks like you have supplied some other type of data.", 
           call. = FALSE)
   }
   
   
   if(nrow(DF) == 0){
      stop("Please check your input. The data.frame you supplied doesn't have any rows.", 
           call. = FALSE)
   }
   
   if(any(complete.cases(highlight_cells))){
      if(class(highlight_cells) == "numeric"){
         highlight_cells <- list(highlight_cells[1:2])
      }
      
      if(any(sapply(highlight_cells, length) < 2)){
         warning("For highlighting cells, you must specify a row and a column for everything you want to be highlighted, and you have only specified one number for at least one of the items you asked to be highlighted. We don't know which rows or columns to highlight without that second number, so nothing will be highlighted.\n", 
                 call. = FALSE)
         highlight_cells <- NA
      }
   }
   
   if(any(complete.cases(highlight_so_cutoffs)) & 
      "Statistic" %in% names(DF) == FALSE){
      warning("You requested highlighting by the S/O ratio, but we look for which row contains that ratio in a column titled `Statistic` and couldn't find that column. We thus don't know which row is for S/O ratios and thus cannot highlight by those values.\n", 
              call. = FALSE)
      highlight_so_cutoffs <- NA
   }
   
   if(any(complete.cases(bold_cells))){
      if(class(bold_cells) == "numeric"){
         bold_cells <- list(bold_cells[1:2])
      }
      
      if(any(sapply(bold_cells, length) < 2)){
         warning("For making cells bold, you must specify a row and a column for everything you want to have bold-face text, and you have only specified one number for at least one of the items you asked to be bold face. We don't know which rows or columns to make bold face without that second number, so we'll use the default settings and make the 1st column and the header row bold.\n", 
                 call. = FALSE)
         bold_cells <- list(c(0, NA), c(NA, 1))
      }
   }
   
   if(complete.cases(highlight_gmr_colors) && 
      tolower(highlight_gmr_colors[1]) == "lisa"){highlight_gmr_colors = "traffic"}
   if(complete.cases(highlight_so_colors) &&
      tolower(highlight_so_colors[1]) == "lisa"){highlight_so_colors = "traffic"}
   
   if(any(complete.cases(highlight_gmr_colors)) &&
      highlight_gmr_colors[1] %in% c("yellow to red", "green to red", "traffic") == FALSE){
      if(length(highlight_gmr_colors) != 4){
         warning("We need 4 colors for highlighting geometric mean ratios, one each for negligible, weak, moderate, and strong interactions, and you have provided a different number of colors. We'll use yellow to red values for highlighting these.\n", 
                 call. = FALSE)
         highlight_gmr_colors <- "yellow to red"
      } else if(is.matrix(col2rgb(highlight_gmr_colors)) == FALSE){
         warning("The values you used for highlighting geometric mean ratios are not all valid colors in R. We'll used the default colors instead.\n", 
                 call. = FALSE)
         highlight_gmr_colors <- "yellow to red"
      } 
   }
   
   if(any(complete.cases(highlight_so_colors)) &&
      highlight_so_colors[1] %in% c("yellow to red", "green to red", "traffic") == FALSE &&
      is.matrix(col2rgb(highlight_so_colors)) == FALSE){
      warning("The values you used for highlighting S/O values are not all valid colors in R. We'll used the default colors instead.\n", 
              call. = FALSE)
      highlight_so_colors <- "yellow to red"
   } 
   
   
   if(class(merge_columns) %in% "numeric"){
      if(all(merge_columns %in% 1:ncol(DF)) == FALSE){
         warning(paste0("You requested that we vertically merge more columns that are present in your data. Specifically, there is/are no column(s) ", 
                        str_comma(setdiff(merge_columns, 1:ncol(DF)), conjunction = "or"), 
                        ". These will be ignored.\n"), 
                 call. = FALSE)
         merge_columns <- merge_columns[merge_columns %in% 1:ncol(DF)]   
      }
      
      merge_columns <- names(DF)[merge_columns]
   }
   
   if(class(merge_columns) %in% "character"){
      BadCols <- setdiff(merge_columns, names(DF))
      if(length(BadCols) > 0){
         warning(paste0("You requested that we vertically merge some columns that are not present in your data. Specifically, the column(s) ", 
                        str_comma(paste0("`", BadCols, "`")), 
                        " is/are not present. These will be ignored. If you believe that's an error, please carefully check that what you specified for `merge_columns` perfectly matches the spelling of each column name.\n"), 
                 call. = FALSE)
         
         merge_columns <- merge_columns[merge_columns %in% names(DF)]
      }
   }
   
   
   # Setting things up for nonstandard evaluation ----------------------------
   shading_column <- rlang::enquo(shading_column)
   sort_column <- rlang::enquo(sort_column)
   
   if(as_label(sort_column) != "<empty>"){
      DF <- DF %>% group_by(across(.cols = any_of("File"))) %>% 
         arrange(!!sort_column)
   }
   
   # Main body of function -------------------------------------------------
   
   # Figuring out which columns contain PK data
   PKRegex <- c(AllPKParameters$PrettifiedNames, 
                AllPKParameters$PrettifiedNames_nodosenum)
   PKRegex <- unique(str_trim(sub("\\(.*\\)", "", PKRegex)))
   PKRegex <- str_c(PKRegex, collapse = "|")
   PKCols <- which(sapply(names(DF), 
                          FUN = function(x){str_detect(x, PKRegex)}))
   
   if(prettify_columns){
      DF <- prettify_column_names(DF)
   }
   
   # Check for whether there are any DDI columns b/c will add extra header row
   # if so later. I have not set this up to replace a specific drug w/that
   # specific drug name, so will need to return to this.
   # FIXME 
   AnyDDI <- any(str_detect(names(DF)[PKCols], " with | ratio")) & 
      add_header_for_DDI
   OrigNames <- names(DF)
   
   FT <- flextable::flextable(DF)
   
   ## Adding DDI header row --------------------------------------------------
   
   if(AnyDDI){
      
      PerpRegex <- paste0(" with perpetrator| ratio|_withInhib|_ratio| with ", 
                          perpetrator_name)
      
      DDIRegex <- c(AllPKParameters$PrettifiedNames[
         AllPKParameters$AppliesOnlyWhenPerpPresent == TRUE], 
         AllPKParameters$PrettifiedNames_nodosenum[
            AllPKParameters$AppliesOnlyWhenPerpPresent == TRUE])
      DDIRegex <- unique(str_trim(sub("\\(.*\\)", "", DDIRegex)))
      DDIRegex <- sub("perpetrator", perpetrator_name, DDIRegex)
      DDIRegex <- str_c(DDIRegex, collapse = "|")
      
      DDIcols <- which(sapply(names(DF), 
                              FUN = function(x){str_detect(x, DDIRegex)}))
      BLcols <- setdiff(PKCols, DDIcols)
      
      RatioCols <- intersect(which(str_detect(OrigNames, "ratio")), 
                             PKCols)
      DDIcols <- setdiff(DDIcols, RatioCols)
      
      TopRowValues <- OrigNames
      TopRowValues[BLcols] <- "Baseline"
      TopRowValues[DDIcols] <- paste("With", perpetrator_name)
      TopRowValues[RatioCols] <- "GMR"
      
      FT <- FT %>% 
         flextable::delete_part(part = "header") %>% 
         flextable::add_header_row(values = sub(PerpRegex, "", OrigNames)) %>% 
         flextable::add_header_row(values = TopRowValues) %>%  
         flextable::merge_h(part = "header") %>% 
         flextable::merge_v(part = "header")
      
   } else {
      PerpRegex <- ""
   }
   
   # Optionally making things bold face ---------------------------------------
   if(any(sapply(bold_cells, complete.cases))){
      for(cells in 1:length(bold_cells)){
         
         if(complete.cases(bold_cells[[cells]][1]) &&
            bold_cells[[cells]][1] == 0){
            # This is when the header should be bold. NB: User does not have the
            # ability to make one but not the other header row -- when there
            # even are two header rows -- bold. Just too complicated to code and
            # unnecessary.
            FT <- FT %>% 
               flextable::bold(part = "header", 
                               j = switch(as.character(is.na(bold_cells[[cells]][2])), 
                                          "TRUE" = NULL, 
                                          "FALSE" = bold_cells[[cells]][2]))
         } else {
            # This is when the bold settings apply to the body. Row can be NA or
            # can be a specific row.
            BoldRows <- bold_cells[[cells]][1]
            if(is.na(BoldRows)){BoldRows <- NULL}
            
            BoldCols <- bold_cells[[cells]][2]
            if(is.na(BoldCols)){BoldCols <- NULL}
            
            FT <- FT %>% 
               flextable::bold(part = "body", 
                               i = BoldRows, 
                               j = BoldCols) 
            
            rm(BoldRows, BoldCols)
         }
      }
   }
   
   # center the header row, bg white -----------------------------------------
   FT <- FT %>% 
      flextable::align(align = "center", part = "header") %>% 
      
      # make everything have a white background (we'll fill in other shading later)
      flextable::bg(part = "all", bg = "white")
   
   # center the columns that contain numbers, i.e., the 2nd column through the
   # penultimate column and optionally center the 1st column
   if(center_1st_column == FALSE & ncol(DF) == 1){
      FT <- FT %>% flextable::align(align = "left")
   } else {
      FT <- FT %>% 
         flextable::align(align = "center", 
                          j = switch(paste(center_1st_column, ncol(DF) > 1),
                                     "TRUE TRUE" = 1:ncol(DF),
                                     "TRUE FALSE" = 1:ncol(DF), 
                                     "FALSE TRUE" = 2:ncol(DF)))
   }
   
   # Optionally including shading whenever the shading column changes ------------
   if(as_label(shading_column) != "<empty>"){
      
      ShadeCol <- DF %>% pull(!!shading_column)
      
      ShadeChange <- which(ShadeCol[1:(length(ShadeCol) - 1)] != 
                              ShadeCol[2:nrow(DF)]) + 1
      if(length(ShadeChange) == 0){
         DF$Shade <- FALSE
      } else {
         ShadeRows <- ShadeChange[seq(1, length(ShadeChange), by = 2)]
         if(length(ShadeChange) > 1){
            NoShadeRows <- ShadeChange[seq(2, length(ShadeChange), by = 2)]
         } else {
            NoShadeRows <- 1
         }
         DF$Shade <- as.logical(NA)
         DF$Shade[ShadeRows] <- TRUE
         DF$Shade[NoShadeRows] <- FALSE
         DF <- DF %>% fill(Shade, .direction = "down") %>% 
            mutate(Shade = ifelse(is.na(Shade), FALSE, Shade))
         
         ShadeRows <- which(DF$Shade)
         FT <- FT %>% 
            flextable::bg(i = ShadeRows, bg = "#F2F2F2") %>% 
            flextable::bg(i = NoShadeRows, bg = "white") %>% 
            flextable::bg(part = "header", bg = "white")
         
         if(merge_shaded_cells){
            FT <- FT %>% 
               flextable::merge_v(j = which(names(DF) == as_label(shading_column)))
         }
      }
   } else {
      ShadeRows <- c()
   }
   
   ## Merge other columns, too -----------------------------------------------
   
   if(any(complete.cases(merge_columns))){
      for(mc in merge_columns){
         FT <- FT %>% 
            flextable::merge_v(j = mc)
      }
   }
   
   ## Highlight GMRs ---------------------------------------------------------
   
   if(any(complete.cases(highlight_gmr_colors))){
      if(highlight_gmr_colors[1] %in% c("yellow to red", "green to red", "traffic")){
         
         highlight_gmr_colors <- switch(tolower(highlight_gmr_colors), 
                                        "green to red" = c("negligible" = "#C7FEAC", 
                                                           "weak" = "#FFFF95",
                                                           "moderate" = "#FFDA95",
                                                           "strong" = "#FF9595"),
                                        "yellow to red" = c("negligible" = "white", 
                                                            "weak" = "#FFFF95",
                                                            "moderate" = "#FFDA95",
                                                            "strong" = "#FF9595"), 
                                        "traffic" = c("negligible" = "#00B050", 
                                                      "weak" = "#92D050",
                                                      "moderate" = "#FFC000",
                                                      "strong" = "#FF0000"))
         
      } else {
         
         # If the user did not properly name the vector, fix that.
         if(any(names(highlight_gmr_colors) != c("negligible", "weak", "moderate", 
                                                 "strong"))){
            names(highlight_gmr_colors) <- c("negligible", "weak", "moderate", 
                                             "strong")
         }
      }
      
      # Columns must be numeric for this to work correctly. 
      suppressWarnings(
         DF <- DF %>% 
            mutate(across(.cols = matches("ratio"), .fns = as.numeric))
      )
      
      # Finding each cell that should be colored according to each level of interaction
      for(j in which(str_detect(tolower(names(DF)), "ratio"))){
         
         RowsToShade <- which(str_detect(DF$Statistic,"CV%") == FALSE)
         
         FT <- FT %>% 
            # Negligible
            flextable::bg(i = intersect(
               which(DF[, j] >= 0.8 & DF[, j] <= 1.25), 
               RowsToShade),
               j = j, 
               bg = highlight_gmr_colors["negligible"]) %>% 
            # Weak
            flextable::bg(i = intersect(
               c(which(DF[, j] >= 0.5 & DF[, j] < 0.8), 
                 which(DF[, j] > 1.25 & DF[, j] <= 2)),
               RowsToShade),
               j = j, 
               bg = highlight_gmr_colors["weak"]) %>% 
            # Moderate 
            flextable::bg(i = intersect(
               c(which(DF[, j] >= 0.2 & DF[, j] < 0.5), 
                 which(DF[, j] > 2 & DF[, j] <= 5)),
               RowsToShade),
               j = j, 
               bg = highlight_gmr_colors["moderate"]) %>% 
            # Strong
            flextable::bg(i = intersect(
               which(DF[, j] < 0.2 | DF[, j] > 5), 
               RowsToShade),
               j = j, 
               bg = highlight_gmr_colors["strong"])
         
         # If they want white to be used when the interaction is negligible and
         # they also have asked for shading, adjust so that we don't have one
         # column that's white where everything else in the row is a light gray.
         if(all(col2rgb(highlight_gmr_colors["negligible"]) == 
                col2rgb("white"))){
            RowsToMakeGray <- intersect(
               intersect(which(DF[, j] >= 0.8 & DF[, j] <= 1.25), 
                         RowsToShade), 
               ShadeRows)
            
            if(length(RowsToMakeGray) > 0){
               FT <- FT %>% 
                  flextable::bg(i = RowsToMakeGray, 
                                j = j, 
                                bg = "#F2F2F2") 
            }
         }
      }
   }
   
   ## Optionally highlighting poor fidelity S/O values -----------------------
   
   if(any(complete.cases(highlight_so_cutoffs))){
      
      # Tidying inputs
      HighlightMiddle <- 1 %in% highlight_so_cutoffs
      
      if(any(highlight_so_cutoffs < 1)){
         warning("At least one of the numbers you specified for highlight_so_cutoffs was < 1. We will automatically use both the original number you specified and its inverse for highlighting, so we'll ignore any values < 1 here.", 
                 call. = FALSE)
         highlight_so_cutoffs <- highlight_so_cutoffs[which(highlight_so_cutoffs >= 1)]
      }
      
      if(length(highlight_so_cutoffs) != length(highlight_so_colors) &
         highlight_so_colors[1] %in% c("yellow to red", "traffic") == FALSE){
         warning("You have specified one number of colors for highlighting S/O values and a different number of cutoff values, so we don't know what colors you want. We'll use the default colors for highlighting.", 
                 call. = FALSE)
         highlight_so_colors <- "yellow to red"
      }
      
      highlight_so_colors <- tolower(highlight_so_colors)
      highlight_so_cutoffs <- sort(unique(highlight_so_cutoffs))
      
      if(highlight_so_colors[1] %in% c("yellow to red", "traffic") == FALSE && 
         is.matrix(col2rgb(highlight_so_colors)) == FALSE){
         warning("The values you used for highlighting problematic S/O ratios are not all valid colors in R. We'll used the default colors instead.", 
                 call. = FALSE)
         highlight_so_colors <- "yellow to red"
      } 
      
      if(highlight_so_colors[1] %in% c("yellow to red", "traffic")){
         
         ColorChoices <- paste(
            highlight_so_colors,
            HighlightMiddle,
            cut(length(highlight_so_cutoffs), breaks = c(0:4, Inf)))
         
         highlight_so_colors <-
            switch(ColorChoices,
                   ## yellow to red
                   
                   # no middle, 1 cutoff
                   "yellow to red FALSE (0,1]" = "#FF9595",
                   
                   # no middle, 2 cutoffs
                   "yellow to red FALSE (1,2]" = c("#FFFF95", "#FF9595"),
                   
                   # no middle, 3 cutoffs
                   "yellow to red FALSE (2,3]" = c("#FFFF95", "#FFDA95", "#FF9595"),
                   
                   # no middle, >3 cutoffs
                   "yellow to red FALSE (3,4]" = colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                      length(highlight_so_cutoffs)),
                   # This is the same as the above on purpose.
                   "FALSE (4,Inf]" = colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                      length(highlight_so_cutoffs)),
                   
                   # Just highlight everything green. This would be weird and
                   # probably not what the user wants, but is among the possible
                   # choices for inputs.
                   "yellow to red TRUE (0,1]" = c("#C7FEAC"),
                   
                   # highlight middle, 1 cutoff other than middle
                   "yellow to red TRUE (1,2]" = c("#C7FEAC", "#FF9595"),
                   
                   # highlight middle, 2 cutoffs other than middle
                   "yellow to red TRUE (2,3]" = c("#C7FEAC", "#FFFF95", "#FF9595"),
                   
                   # highlight middle, 3 cutoffs other than middle
                   "yellow to red TRUE (3,4]" = c("#C7FEAC", "#FFFF95", "#FFDA95", "#FF9595"),
                   
                   # highlight middle, >3 cutoffs other than middle
                   "yellow to red TRUE (4,Inf]" =
                      c("#C7FEAC",
                        colorRampPalette(c("#FFFF95", "#FFDA95", "#FF9595"))(
                           length(highlight_so_cutoffs))),
                   
                   ## traffic
                   
                   # no middle, 1 cutoff
                   "traffic FALSE (0,1]" = "#FF0000",
                   
                   # no middle, 2 cutoffs
                   "traffic FALSE (1,2]" = c("#FFC000", "#FF0000"),
                   
                   # no middle, 3 cutoffs
                   "traffic FALSE (2,3]" = colorRampPalette(c("#FFC000", "#FF0000"))(
                      length(highlight_so_cutoffs)),
                   
                   # no middle, >3 cutoffs
                   "traffic FALSE (3,4]" = colorRampPalette(c("#FFC000", "#FF0000"))(
                      length(highlight_so_cutoffs)),
                   # This is the same as the above on purpose.
                   
                   "FALSE (4,Inf]" = colorRampPalette(c("#FFC000", "#FF0000"))(
                      length(highlight_so_cutoffs)),
                   # This is the same as the above on purpose.
                   
                   # Just highlight everything green. This would be weird and
                   # probably not what the user wants, but is among the possible
                   # choices for inputs.
                   "traffic TRUE (0,1]" = c("#00B050"),
                   
                   # highlight middle, 1 cutoff other than middle
                   "traffic TRUE (1,2]" = c("#00B050", "#FF0000"),
                   
                   # highlight middle, 2 cutoffs other than middle
                   "traffic TRUE (2,3]" = c("#00B050", "#92D050", "#FF0000"),
                   
                   # highlight middle, 3 cutoffs other than middle
                   "traffic TRUE (3,4]" = c("#00B050", "#92D050", "#FFC000", "#FF0000"),
                   
                   # highlight middle, >3 cutoffs other than middle
                   "traffic TRUE (4,Inf]" =
                      c("#00B050",
                        colorRampPalette(c("#FFC000", "#FF0000"))(
                           length(highlight_so_cutoffs)))
            )
      }
      
      StatCol <- which(str_detect(names(DF), "[Ss]tat$|[Ss]tatistic"))
      SOrows <- which(str_detect(t(DF[, StatCol[1]]),
                                 "S/O|[Ss]simulated.*[Oo]bserved"))
      
      # Columns must be numeric for this to work correctly. 
      suppressWarnings(
         DF <- DF %>% 
            mutate(across(.cols = matches("S/O|[Ss]simulated.*[Oo]bserved"), .fns = as.numeric))
      )
      
      for(i in SOrows){
         for(j in 1:length(highlight_so_cutoffs)){
            suppressWarnings(
               SO_col <- which(
                  as.numeric(t(DF[i, ])) >= highlight_so_cutoffs[j] | 
                     as.numeric(t(DF[i, ])) <= 1/highlight_so_cutoffs[j])
            )
            SO_col <- intersect(SO_col, PKCols)
            
            if(length(SO_col) > 0){
               FT <- FT %>% 
                  flextable::bg(i = i, 
                                j = SO_col, 
                                bg = highlight_so_colors[j])
            }
            
            rm(SO_col)
         }
      }
   }
   
   
   ## Optionally highlight specific cells -----------------------------------
   if(any(sapply(highlight_cells, complete.cases))){
      for(i in 1:length(highlight_cells)){
         
         FT <- FT %>% 
            flextable::bg(i = switch(paste(is.na(highlight_cells[[i]][1]), 
                                           highlight_cells[[i]][1] == 0), 
                                     "TRUE NA" = NULL, 
                                     "FALSE TRUE" = 1, # this is when the row should be the only row in the header
                                     "FALSE FALSE" = highlight_cells[[i]][1]), 
                          j = switch(as.character(is.na(highlight_cells[[i]][2])), 
                                     "TRUE" = NULL, 
                                     "FALSE" = highlight_cells[[i]][2]), 
                          bg = highlight_color, 
                          part = ifelse(complete.cases(highlight_cells[[i]][1]) & 
                                           highlight_cells[[i]][1] == 0, 
                                        "header", "body"))   
      }
   }
   
   
   ## Applying other aesthetics -------------------------------------------
   
   FT <- FT %>% 
      
      # Set the font size
      flextable::fontsize(part = "all", size = fontsize) %>% 
      
      # setting up which borderlines to show
      flextable::border_remove() %>% 
      flextable::border_inner_v(part = "all", 
                                border = officer::fp_border(width = 0.5)) %>% 
      flextable::border_inner_h(part = "header", 
                                border = officer::fp_border(width = 0.5)) %>% 
      flextable::border_outer(border = officer::fp_border(width = 0.5)) %>% 
      flextable::hline_bottom(part = "body", 
                              border = officer::fp_border(width = 0.5)) %>% 
      flextable::fix_border_issues() %>% 
      
      # making the width autofitted to contents
      flextable::set_table_properties(width = 1, layout = "autofit")
   
   # Dealing with subscripts
   ColNames <- sub(PerpRegex, "", OrigNames)
   ColNames <- sub("AUCt( |$)", "AUC~t~ ", ColNames)
   ColNames <- sub("AUCinf( |$)", "AUC~inf~ ", ColNames)
   ColNames <- sub("AUCt$", "AUC~t~", ColNames)
   ColNames <- sub("AUCtau", "AUC~tau~", ColNames)
   ColNames <- sub("Cmax", "C~max~", ColNames)
   ColNames <- sub("Cmin", "C~min~", ColNames)
   ColNames <- sub("tmax", "t~max~", ColNames)
   ColNames <- sub("tlag", "t~lag~", ColNames)
   ColNames <- sub(" ka ", " k~a~ ", ColNames)
   ColNames <- sub("^ka ", "k~a~ ", ColNames)
   ColNames <- sub(" fa ", " f~a~ ", ColNames)
   ColNames <- sub("^fa ", "f~a~ ", ColNames)
   ColNames <- sub(" fh ", " f~h~ ", ColNames)
   ColNames <- sub("^fh ", "f~h~ ", ColNames)
   ColNames <- sub(" fg ", " f~g~ ", ColNames)
   ColNames <- sub("^fg ", "f~g~ ", ColNames)
   ColNames <- sub("Indmax", "Ind~max~", ColNames)
   ColNames <- sub("Emax", "E~max~", ColNames)
   ColNames <- sub("IndC50", "IndC~50~", ColNames)
   ColNames <- sub("EC50", "EC~50~", ColNames)
   
   ColNames <- str_split(ColNames, pattern = "~", simplify = T)
   
   if(ncol(ColNames) == 3){
      for(cols in which(ColNames[,2] != "")){
         FT <- FT %>% 
            flextable::compose(part = "header",
                               i = ifelse(AnyDDI, 2, 1),
                               j = cols,
                               value = flextable::as_paragraph(
                                  ColNames[cols, 1],
                                  flextable::as_sub(ColNames[cols, 2]), 
                                  ColNames[cols, 3]))
      }
   }
   
   # Saving --------------------------------------------------------------
   if(complete.cases(save_table)){
      
      # Format the file name appropriately, including making the extension be
      # docx, even if they specified something else.
      save_table <- ifelse(str_detect(save_table, "\\..*$"), 
                           sub("\\..*", ".docx", save_table), 
                           paste0(save_table, ".docx"))
      
      # Now that the file should have an appropriate extension, check what
      # the path and basename should be.
      OutPath <- dirname(save_table)
      save_table <- basename(save_table)
      
      # May need to change the working directory temporarily, so
      # determining what it is now
      CurrDir <- getwd()
      
      OutPath <- dirname(save_table)
      if(OutPath == "."){
         OutPath <- getwd()
      }
      
      FileName <- basename(save_table)
      
      rmarkdown::render(system.file("rmarkdown/templates/savetablesimcyp/skeleton/skeleton.Rmd",
                                    package="SimcypConsultancy"), 
                        output_format = rmarkdown::word_document(reference_docx = TemplatePath), 
                        output_dir = OutPath, 
                        output_file = FileName, 
                        quiet = TRUE,
                        params = list(template_path = TemplatePath))
      # Note: The "system.file" part of the call means "go to where the
      # package is installed, search for the file listed, and return its
      # full path.
      
   }
   
   return(FT)
   
}


