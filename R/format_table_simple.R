#' Format a table rather simply to look nice in a Word file
#'
#' \code{format_table_simple} is meant to for use with writing compound summary
#' files. It is a wrapper function for formatTable_Simcyp that creates a
#' somewhat simpler style of table.
#'
#' @param DF a data.frame
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
#' @param font font to use. Default is "Palatino Linotype" and any fonts
#'   available on your machine in either Word or PowerPoint should be
#'   acceptable. If you get Times New Roman in your table when you asked for
#'   something else, it means that that font isn't available or maybe wasn't
#'   spelled the way R is expecting it. For example, "Calibri" works but
#'   "Calibri (Body)" doesn't even though the latter is listed in PowerPoint and
#'   Word.
#' @param fontsize the numeric font size for the output table. Default is 11
#'   point.
#' @param column_widths optionally specify what the widths of the columns should
#'   be with a numeric vector of the widths in inches, e.g., \code{column_widths
#'   = c(1.5, 2, 0.5, 3)}
#' @param alignment alignment of text throughout table. Options are "left"
#'   (default), "right", "center", or "justify".
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
                                highlight_gmr_colors = NA, 
                                highlight_so_cutoffs = NA, 
                                highlight_so_colors = "yellow to red",
                                font = "Palatino Linotype", 
                                fontsize = 11, 
                                column_widths = NA, 
                                alignment = "left", 
                                save_table = NA,
                                page_orientation = "portrait", 
                                title_document = NA, 
                                table_caption = NA){
   
   # Error catching --------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
   }
   
   alignment <- tolower(alignment)[1]
   alignment <- ifelse(str_detect(alignment, "just"), "justify", alignment)
   if(is.na(alignment) | 
      alignment %in% c("left", "right", "center", "justify") == FALSE){
      warning(wrapn("You have specified something for the text alignment that is not among the available options. We'll use the default text alignment of 'left'."), 
              call. = FALSE)
      alignment <- "left"
   }
   
   if("flextable" %in% class(DF)){
      warning(wrapn("Just a note: You have provided a flextable object to 'format_table_simple'. When 'format_table_simple' received a flextable as input, it does not apply any formatting to that table and instead just passes it through to possibly save the flextable but nothing else. Just wanted to clarify that, if you want the simple style of 'format_table_simple', you'll need to supply a data.frame or tibble instead."), 
              call. = FALSE)
   }
   
   # Setting things up for nonstandard evaluation ----------------------------
   shading_column <- rlang::enquo(shading_column)
   
   
   # Main body of function ---------------------------------------------------
   
   # Make the flextable 1st, then apply text alignment, then save. Doing this
   # this way b/c format_table_simple applies alignment across all of the table
   # whereas formatTable_Simcyp applies it very specifically. This is easier.
   FT <- formatTable_Simcyp(DF, 
                            shading_column = !!shading_column, 
                            merge_shaded_cells = merge_shaded_cells,
                            merge_columns = merge_columns, 
                            bold_cells = list(c(0, NA)), 
                            add_header_for_DDI = FALSE, 
                            prettify_columns = FALSE, 
                            borders = FALSE, 
                            highlight_gmr_colors = highlight_gmr_colors, 
                            highlight_so_colors = highlight_so_colors, 
                            highlight_so_cutoffs = highlight_so_cutoffs, 
                            font = font, 
                            fontsize = fontsize, 
                            column_widths = column_widths, 
                            save_table = NA) %>% 
      flextable::align(part = "all", 
                       align = alignment)
   
   if(any(complete.cases(save_table))){
      formatTable_Simcyp(FT, 
                         highlight_gmr_colors = highlight_gmr_colors, 
                         highlight_so_colors = highlight_so_colors, 
                         highlight_so_cutoffs = highlight_so_cutoffs, 
                         save_table = save_table, 
                         page_orientation = page_orientation, 
                         title_document = title_document, 
                         table_caption = table_caption)
   }
   
   return(FT)
}

