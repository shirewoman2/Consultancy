#' Save a bespoke PK table to Word using the pksummary_mult rmarkdown template
#'
#' \code{save_table_to_Word} will save a DIY PK table to Word using the same
#' template file as is used for the function pksummary_mult. UNDER CONSTRUCTION.
#' I haven't done almost any error catching here yet. -LSh
#'
#' @param MyPKResults PK table that includes the columsn File, Statistic,
#'   Tissue, CompoundID, and then also columns named in the standard way
#'   (prettified or un-prettified) for PK tables from the SimcypConsultancy
#'   package.
#' @param PKpulled PK that are included in the table. All must be included in
#'   AllPKParameters$PKparameter.
#' @param save_table optionally save the output table by supplying a file name
#'   in quotes here, e.g., "My nicely formatted table.docx".  Do not include any slashes, dollar signs, or periods in the file name.
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} to get all the details from the "Input
#'   Sheet" (e.g., when you ran extractExpDetails you said \code{exp_details =
#'   "Input Sheet"} or \code{exp_details = "all"}), you can save some processing
#'   time by supplying that object here, unquoted. If left as NA, this function
#'   will run \code{extractExpDetails} behind the scenes to figure out some
#'   information about your experimental set up.
#' @param mean_type What kind of means and CVs do you want listed in the output
#'   table? Options are "arithmetic" or "geometric" (default).
#' @param single_table TRUE (default) or FALSE for whether to save all the PK
#'   data in a single table or break the data up by tissue, compound ID, and
#'   file into multiple tables. This only applies to the Word output.
#' @param include_dose_num NA (default), TRUE, or FALSE on whether to include
#'   the dose number when listing the PK parameter. By default, the parameter
#'   will be labeled, e.g., "Dose 1 Cmax ratio" or "Last dose AUCtau ratio", if
#'   you have PK data for both the first dose and the last dose. Also by
#'   default, if you have data only for the first dose or only for the last
#'   dose, the dose number will be omitted and it will be labeled, e.g., "AUCtau
#'   ratio" or "Cmax ratio". Set this to TRUE or FALSE as desired to override
#'   the default behavior and get exactly what you want.
#' @param fontsize the numeric font size for Word output. Default is 11 point.
#'   This only applies when you save the table as a Word file.
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
#'   c("green", "red")}. This only applies when you save the table as a Word file.
#' @param highlight_so_colors optionally specify a set of colors to use in the
#'   Word file output for highlighting S/O values outside the limits you
#'   specified with \code{highlight_so_cutoffs}. Options: \describe{
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
#'   work here, e.g., \code{highlight_so_colors = c("yellow", "orange", "red")}}
#'   If you do specify your own bespoke colors, you'll need to make sure that
#'   you supply one color for every value in \code{highlight_so_cutoffs}.}
#'
#' @return saves a table to a Word file
#' @export
#'
#' @examples
#' # None yet
#' 
save_table_to_Word <- function(
      MyPKResults, 
      PKpulled, # make this automatically determined later
      save_table,
      existing_exp_details,
      mean_type = "geometric", 
      single_table = FALSE, 
      include_dose_num = NA,
      fontsize = 11, 
      highlight_so_cutoffs = NA, 
      highlight_so_colors = NA){
   
   # MyPKResults needs to have the following columns:
   # File
   # Statistic -- must contain "S/O" if you want "observed" to show up in table headings
   # Tissue
   # CompoundID
   # PK parameters
   
   tissues = sort(unique(MyPKResults$Tissue)) 
   compoundsToExtract <- sort(unique(MyPKResults$CompoundID))
   # PKpulled <- intersect(names(MyPKResults), 
   #                       unique(c(AllPKParameters$PKparameter, 
   #                                AllPKParameters$BasePKparameter) # FIXME 
   
   PKToPull <- PKpulled
   PKpulled <- expand.grid(PKpulled = PKpulled, 
                           File = unique(MyPKResults$File), 
                           Tissue = unique(MyPKResults$Tissue), 
                           CompoundID = compoundsToExtract)
   
   FromCalcPKRatios = FALSE
   paired = FALSE # Probably don't need this
   
   OutPath <- dirname(save_table)
   
   if(OutPath == "."){
      OutPath <- getwd()
   }
   
   FileName <- basename(save_table)
   
   rmarkdown::render(
      system.file("rmarkdown/templates/pksummarymult/skeleton/skeleton.Rmd", 
                  package="SimcypConsultancy"),
      output_dir = OutPath, 
      output_file = FileName, 
      quiet = TRUE)
   
}