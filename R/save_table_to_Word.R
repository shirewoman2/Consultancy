#' Save a bespoke PK table to Word using the pksummary_mult rmarkdown template
#'
#' \code{save_table_to_Word} will save a DIY PK table to Word using the same
#' template file as is used for the function pksummary_mult. UNDER CONSTRUCTION.
#' I haven't done almost any error catching here yet. -LSh
#'
#' @param PKtable PK table (really, a data.frame or tibble) that includes the
#'   columns File, Statistic, Tissue, CompoundID, and then also columns named in
#'   the standard way (prettified or un-prettified) for PK parameters from the
#'   SimcypConsultancy package. Well, that is, your input for PKtable must meet
#'   those criteria if you want it to be formatted in any way or if you want to
#'   split it into multiple tables using the argument \code{single_table} or
#'   have highlighting applied with \code{highlight_so_cutoffs}. If you
#'   \emph{don't} care about that, supply any old data.frame or tibble, and this
#'   will save it to Word. If you supply a flextable (see the
#'   \href{https://davidgohel.github.io/flextable/}{eponymous package} if you
#'   don't know what we're talking about), we also won't touch the formatting
#'   and will return your flextable as is in the Word document.
#' @param save_table save the output table by supplying a file name in quotes
#'   here, e.g., "My nicely formatted table.docx".  Do not include any slashes,
#'   dollar signs, or periods in the file name.
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} to get all the details from the "Input
#'   Sheet" (e.g., when you ran extractExpDetails you said \code{exp_details =
#'   "Input Sheet"} or \code{exp_details = "all"}), we can use this information
#'   to make nicely annotated table headings and captions. Not strictly
#'   required, though.
#' @param mean_type What kind of means and CVs do you want listed in the output
#'   table? Options are "arithmetic" or "geometric" (default).
#' @param single_table TRUE (default) or FALSE for whether to save all the PK
#'   data in a single table or break the data up by tissue, compound ID, and
#'   file into multiple tables. This only applies to the Word output. If you
#'   want to save the file with multiple tables, we do need your PK table to be
#'   structures in a specific way so that we know where to separate things and
#'   how to format the resulting individual tables. Your entry for
#'   \code{PKtable} must contain the columns "File", "CompoundID", and "Tissue"
#'   for this to work well and must contain the column "File" to work at all.
#' @param prettify_columns TRUE (default) or FALSE for whether to make easily
#'   human-readable column names for any columns with PK parameters. TRUE makes
#'   pretty column names such as "Dose 1 AUCinf (h*ng/mL)" whereas FALSE leaves
#'   the column with the R-friendly name from \code{\link{extractPK}}, e.g.,
#'   "AUCinf_dose1".
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
#' MyPKTable <- tibble(Statistic = c("Simulated", "CV%", "Observed", "S/O"),
#'                         AUCinf = c(2756, 32.5, 1801, 1.53), 
#'                         Cmax = c(852, 45.8, 775, 1.1), 
#'                         `Half life` = c(7.75, 5.7, 6.05, 1.28))
#' 
#' \dontrun{
#' save_table_to_Word(MyPKTable, save_table = "My PK table.docx")
#' }
#' 
save_table_to_Word <- function(
      PKtable, 
      save_table,
      existing_exp_details = NA,
      mean_type = "geometric", 
      single_table = FALSE, 
      include_dose_num = NA,
      prettify_columns = TRUE, # want to add prettify compound names but haven't yet
      fontsize = 11, 
      # shading_column, 
      merge_columns = NA, 
      # sort_column, 
      bold_cells = list(c(0, NA), c(NA, 1)),
      center_1st_column = FALSE,
      merge_shaded_cells = TRUE, 
      highlight_so_cutoffs = NA, 
      highlight_so_colors = NA, 
      highlight_gmr_colors = NA){
   
   # PKtable needs to have the following columns to get saved w/pksummarymult Rmd file:
   # File
   # Statistic -- must contain "S/O" if you want "observed" to show up in table headings
   # Tissue
   # CompoundID
   # PK parameters
   
   OutPath <- dirname(save_table)
   
   if(OutPath == "."){
      OutPath <- getwd()
   }
   
   FileName <- basename(save_table)
   
   if("flextable" %in% class(PKtable) |
      "File" %in% names(PKtable) == FALSE |
      (all(c("Tissue", "CompoundID") %in% names(PKtable)) == FALSE &
       single_table == TRUE)){
      
      if("File" %in% names(PKtable) == FALSE & single_table == FALSE){
         warning("You requested that we break up the table you provided in to multiple tables in the Word file, but your table doesn't include a column called `File`, which is what this function uses to break up the single table into multiples. We'll save your table as is.\n", 
                 call. = FALSE)
      }
      
      if("flextable" %in% class(PKtable) == FALSE){
         PKtable <- flextable::flextable(PKtable)
      }
      
      rmarkdown::render(
         system.file("rmarkdown/templates/savetable/skeleton/skeleton.Rmd", 
                     package="SimcypConsultancy"),
         output_dir = OutPath, 
         output_file = FileName, 
         quiet = TRUE)
      
   } else {
      
      if("Tissue" %in% names(PKtable) == FALSE){
         warning("You'll get better-looking output if you include a column in PKtable titled `Tissue` that indicates what tissue the PK are for, e.g., blood or plasma.\n", 
                 call. = FALSE)
         PKtable$Tissue <- "NOT SPECIFIED"
      }
      
      if("CompoundID" %in% names(PKtable) == FALSE){
         warning("We need the column `CompoundID` to appear in PKtable to make things work correctly. We'll assume that your table contained PK for the substrate for now, but you might want to include a column in PKtable titled `CompoundID` that indicates what compound ID the PK are for, e.g., substrate, inhibitor 1, etc.\n", 
                 call. = FALSE)
         PKtable$CompoundID <- "substrate"
      }
      
      tissues = sort(unique(PKtable$Tissue)) 
      compoundsToExtract <- sort(unique(PKtable$CompoundID))
      
      AllPKParameters_mod <- 
         AllPKParameters %>% select(PKparameter, PrettifiedNames) %>% 
         mutate(PKparameter = sub("_dose1|_last", "", PKparameter), 
                PrettifiedNames = str_trim(sub("Last dose|Dose 1", "", 
                                               PrettifiedNames))) %>% 
         unique()
      
      TableNames <-
         data.frame(PKparameter = names(PKtable)) %>% 
         mutate(IsPretty = PKparameter %in% c(AllPKParameters$PrettifiedNames, 
                                              AllPKParameters_mod$PrettifiedNames), 
                IsNotPretty = PKparameter %in% c(AllPKParameters_mod$PKparameter, 
                                                 AllPKParameters$PKparameter),
                IsPKParam = IsPretty | IsNotPretty, 
                NeedsPrettifying = IsPKParam & IsNotPretty)
      
      TableNames <- split(TableNames, f = TableNames$NeedsPrettifying)
      
      if(is.null(nrow(TableNames[["TRUE"]])) == FALSE){
         suppressMessages(
            TableNames[["TRUE"]] <- TableNames[["TRUE"]] %>% 
               left_join(AllPKParameters %>% 
                            select(PKparameter, PrettifiedNames) %>% 
                            mutate(PKparameter_unpretty = PKparameter)) %>% 
               unique() %>% 
               mutate(PrettifiedNames = ifelse(is.na(PrettifiedNames), 
                                               PKparameter, PrettifiedNames))
         )
      }
      
      if(is.null(nrow(TableNames[["FALSE"]])) == FALSE){
         suppressMessages(
         TableNames[["FALSE"]] <- TableNames[["FALSE"]] %>% 
            rename(PrettifiedNames = PKparameter) %>% 
            left_join(AllPKParameters %>% bind_rows(AllPKParameters_mod) %>% 
                         select(PKparameter, PrettifiedNames)) %>% 
            unique() %>% 
            mutate(PKparameter = ifelse(is.na(PKparameter), 
                                        PrettifiedNames, PKparameter), 
                   PKparameter_unpretty = PKparameter)
         )
      }
      
      TableNames <- bind_rows(TableNames)
      
      PKpulled <- TableNames$PKparameter_unpretty[TableNames$IsPKParam == TRUE]
      
      PKToPull <- PKpulled
      PKpulled <- expand_grid(PKpulled = PKpulled, 
                              File = unique(PKtable$File), 
                              Tissue = unique(PKtable$Tissue), 
                              CompoundID = compoundsToExtract)
      
      FromCalcPKRatios = FALSE
      paired = FALSE # Probably don't need this
      
      if(prettify_columns){
         MyPKResults <- prettify_column_names(PKtable)  
      } else {
         MyPKResults <- PKtable
      }
      
      rmarkdown::render(
         system.file("rmarkdown/templates/pksummarymult/skeleton/skeleton.Rmd", 
                     package="SimcypConsultancy"),
         output_dir = OutPath, 
         output_file = FileName, 
         quiet = TRUE)
      
   }
}
