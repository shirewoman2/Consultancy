#' Format a table to look nice in a Word file or pdf - UNDER CONSTRUCTION
#'
#' \code{format_table_simple} is meant to for use with writing compound summary
#' pdfs. It includes minimal error catching at this point.
#'
#' @param DF a data.frame or a flextable
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
#' @param font font to use. Default is "Palatino Linotype" and any fonts
#'   available on your machine in either Word or PowerPoint should be
#'   acceptable. If you get Times New Roman in your table when you asked for
#'   something else, it means that that font isn't available or maybe wasn't
#'   spelled the way R is expecting it. For example, "Calibri" works but
#'   "Calibri (Body)" doesn't even though the latter is listed in PowerPoint and
#'   Word.
#' @param fontsize the numeric font size for the output table. Default is 11
#'   point.
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
#' @param page_orientation set the page orientation for the Word file output to
#'   "portrait" (default) or "landscape"
#'
#' @return a formatted table
#' @export
#'
#' @examples
#' # None yet
#' 
format_table_simple <- function(DF, 
                                shading_column, 
                                merge_shaded_cells = TRUE, 
                                merge_columns = NA, 
                                font = "Palatino Linotype", 
                                fontsize = 11, 
                                save_table = NA,
                                page_orientation = "portrait", 
                                title_document = NA, 
                                table_caption = NA){
   
   # flextable function w/the specs I want. Note: I had a lot of trouble when I
   # tried to use a modified version of formatTable_Simcyp here. I kept getting
   # latex errors about extra carriage returns. Could not figure out the problem!
   
   # Catching instances where the font name isn't *exactly* the same as what's
   # in Word or PowerPoint. Will have to slowly gather examples of this.
   font <- case_when(
      # "Calibri (Body)" dosen't work; just "Calibri" does.
      str_detect(font, "Calibri") ~ "Calibri", 
      .default = font)
   
   
   # Setting things up for nonstandard evaluation ----------------------------
   shading_column <- rlang::enquo(shading_column)
   
   # Main body of function ---------------------------------------------------
   if("Parameter" %in% names(DF)){
      FT <- format_scripts(DF, parameter_column = Parameter)
   } else {
      FT <- format_scripts(DF)
   }
   
   # Setting up shading ----------------------------------------------------
   
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
   
   if(any(complete.cases(merge_columns))){
      for(mc in merge_columns){
         FT <- FT %>% 
            flextable::merge_v(j = mc)
      }
   }
   
   FT <- FT %>% 
      # Set the font size
      flextable::fontsize(part = "all", size = fontsize) %>% 
      
      # Set the font
      flextable::font(part = "all",
                      # fontname = "fourier") %>% # doesn't work. I think the font has to be something available in Word b/c I think the folder loation is not the same as for latex.
                      fontname = font) %>%
      flextable::bold(part = "header") %>% 
      flextable::width(width = (7 / ncol(DF))) %>%
      flextable::fix_border_issues()
   
   # Saving --------------------------------------------------------------
   if(complete.cases(save_table)){
      
      formatTable_Simcyp(DF = FT, 
                         save_table = save_table, 
                         page_orientation = page_orientation, 
                         title_document = title_document, 
                         table_caption = table_caption)
   }
   
   return(FT)
}
