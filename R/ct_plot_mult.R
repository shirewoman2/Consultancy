#' Make graphs for multiple Simulator output files at once
#'
#' This function was designed for making nicely arranged concentration-time
#' graphs from several Simcyp Simulator output files all together \emph{or} for
#' making multiple files -- one for each Simulator file -- all at once. Behind
#' the scenes, it uses the function \code{\link{ct_plot}} to make these graphs,
#' so it will automatically break up your supplied concentration-time data into
#' datasets by a) file, b) compound ID, c) tissue, and d) subsection of any
#' ADAM-model data. If you have more than one dataset per file, the graph titles
#' won't necessarily be clear, so please pay attention to what data you're
#' including. If you get unexpected or unclear output, try using
#' \code{\link{ct_plot_overlay}} to graph your data; it might work better for
#' what you want to show.
#'
#' \strong{A note on the order of the graphs:} This function arranges graphs
#' first by file, then by compound ID, and then by tissue, and all sorting is
#' alphabetical. However, since sorting alphabetically might not be the optimal
#' graph arrangement for \emph{your} scenario, you can specify the order of the
#' graphs using either the \code{graph_titles} argument or, if you're
#' comfortable with setting factors in R, by making any of File, CompoundID,
#' Tissue, and subsection_ADAM factor rather than character data and setting the
#' levels how you wish. If you're unfamiliar with setting factor levels in R and
#' setting \code{graph_titles} isn't achieving what you want, please ask a
#' member of the R Working Group for assistance.
#'
#' @param ct_dataframe the data.frame with multiple sets of concentration-time
#'   data
#' @param obs_to_sim_assignment optionally specify which observed files should
#'   be compared to which simulator files. If left as NA and what you supplied
#'   for \code{ct_dataframe} doesn't already specify which observed data go with
#'   which simulated file, this will assume that \emph{all} observed data goes
#'   with \emph{all} simulated data. To specify, use a named character vector
#'   like this: \code{obs_to_sim_assignment = c("obs data 1.xlsx" =
#'   "mdz-5mg-qd.xlsx", "obs data 2.xlsx" = "mdz-5mg-qd-cancer.xlsx")} If one
#'   observed file needs to match more than one simulated file but not
#'   \emph{all} the simulated files, you can do that by separating the simulated
#'   files with commas, e.g., \code{obs_to_sim_assignment = c("obs data 1.xlsx"
#'   = "mdz-5mg-qd.xlsx, mdz-5mg-qd-fa08.xlsx", "obs data 2.xlsx" =
#'   "mdz-5mg-qd-cancer.xlsx, mdz-5mg-qd-cancer-fa08.xlsx")}. Pay close
#'   attention to the position of commas and quotes there!
#' @param graph_arrangement set how to arrange the graphs. Options are
#'   \describe{\item{"all together"}{(default) for all graphs being nicely
#'   arranged and aligned together,}
#'
#'   \item{"separate files"}{to make one output file per simulator file, or}
#'
#'   \item{"numrows x numcols"}{where you replace "numrows" with the number of
#'   rows of graphs you'd like and "numcols" with the number of columns. The
#'   result will be a single, nicely arranged graph. For example, "2 x 4" will
#'   make a set of graphs with 2 rows and 4 columns. This is the same as the
#'   option "all together" except with more control.}}
#'
#'   If you choose "separate files", each Simulator output file will have its
#'   own graph file, named to match the Simulator output file name and you don't
#'   need to specify anything for \code{save_graph}. (In fact, anything you
#'   specify for \code{save_graph} will be ignored.)
#'
#' @param figure_type type of figure to plot. Options are:
#'
#'   \describe{
#'
#'   \item{"trial means"}{plots an opaque line for the mean data, lighter lines
#'   for the mean of each trial of simulated data, and open circles for the
#'   observed data. If an effector were present, lighter dashed lines indicate
#'   the mean of each trial of simulated data in the presence of the effector.}
#'
#'   \item{"percentiles"}{(default) plots an opaque line for the mean data,
#'   lighter lines for the 5th and 95th percentiles of the simulated data, and
#'   open circles for the observed data. If an effecter were present, the
#'   default is dashed lines for the data in the presence of an effector.}
#'
#'   \item{"percentile ribbon"}{plots an opaque line for the mean data,
#'   transparent shading for the 5th to 95th percentiles of the simulated data,
#'   and open circles for the observed data. If an effector were present, the
#'   default is to show the data without the effector in blue and the data in
#'   the presence of the effector in red. Note: You may sometimes see some
#'   artifacts -- especially for semi-log plots -- where the ribbon gets partly
#'   cut off. For arcane reasons we don't want to bore you with here, we can't
#'   easily prevent this. However, a possible fix is to set your y axis limits
#'   for the semi-log plot to be wider using \code{y_axis_limits_log}.}
#'
#'   \item{"means only"}{plots a black line for the mean data and, if an
#'   effector was modeled, a dashed line for the concentration-time data with
#'   Inhibitor 1.}
#'
#'   \item{"Freddy"}{Freddy's favorite style of plot with trial means in light
#'   gray, the overall mean in thicker black, the 5th and 95th percentiles in
#'   dashed lines, and the observed data in semi-transparent purple-blue points.
#'   Graphs with an effector present lose the trial means, and the percentiles
#'   switch to solid, gray lines.}}
#'
#' @param mean_type graph "arithmetic" (default) or "geometric" means or
#'   "median" for median concentrations
#' @param linear_or_log the type of graph to be returned. Options: "semi-log"
#'   (default), "linear", "both vertical" (graphs are stacked vertically), or
#'   "both horizontal" (graphs are side by side).
#' @param time_range time range to show relative to the start of the simulation.
#'   Options: \describe{
#'
#'   \item{NA}{(default) entire time range of data}
#'
#'   \item{a start time and end time in hours}{only data in that time range,
#'   e.g. \code{c(24, 48)}. Note that there are no quotes around numeric data.}
#'
#'   \item{"first dose"}{only the time range of the first dose}
#'
#'   \item{"last dose"}{only the time range of the last dose}
#'
#'   \item{"penultimate dose"}{only the time range of the 2nd-to-last dose,
#'   which can be useful for BID data where the end of the simulation extended
#'   past the dosing interval or data when the substrate was dosed BID and the
#'   effector was dosed QD}
#'
#'   \item{a specific dose number with "dose" or "doses" as the prefix}{the time
#'   range encompassing the requested doses, e.g., \code{time_range = "dose 3"}
#'   for the 3rd dose or \code{time_range = "doses 1 to 4"} for doses 1 to 4}
#'
#'   \item{"all obs" or "all observed" if you feel like spelling it out}{Time
#'   range will be limited to only times when observed data are present.}
#'
#'   \item{"last dose to last observed" or "last obs" for short}{Time range will
#'   be limited to the start of the last dose until the last observed data
#'   point.}
#'
#'   }
#'
#' @param pad_x_axis optionally add a smidge of padding to the the x axis
#'   (default is TRUE, which includes some generally reasonable padding). If
#'   changed to FALSE, the y axis will be placed right at the beginning of your
#'   time range and all data will end \emph{exactly} at the end of the time
#'   range specified. If you want a \emph{specific} amount of x-axis padding,
#'   set this to a number; the default is \code{c(0.02, 0.04)}, which adds 2\%
#'   more space to the left side and 4\% more to the right side of the x axis.
#'   If you only specify one number, we'll assume that's the percent you want
#'   added to the left side.
#' @param pad_y_axis optionally add a smidge of padding to the y axis (default
#'   is TRUE, which includes some generally reasonable padding). As with
#'   \code{pad_x_axis}, if changed to FALSE, the x axis will be placed right at
#'   the bottom of your data, possible cutting a point in half. If you want a
#'   \emph{specific} amount of y-axis padding, set this to a number; the default
#'   is \code{c(0.02, 0)}, which adds 2\% more space to the bottom and nothing
#'   to the top of the y axis. If you only specify one number, we'll assume
#'   that's the percent you want added to the bottom.
#' @param x_axis_interval optionally set the x-axis major tick-mark interval.
#'   Acceptable input: any number or leave as NA to accept default values, which
#'   are generally reasonable guesses as to aesthetically pleasing and
#'   PK-relevant intervals.
#' @param x_axis_label optionally supply a character vector or an expression to
#'   use for the x axis label
#' @param y_axis_limits_lin optionally set the Y axis limits for the linear
#'   plot, e.g., \code{c(10, 1000)}. If left as the default NA, the Y axis
#'   limits for the linear plot will be automatically selected.
#' @param y_axis_limits_log optionally set the Y axis limits for the semi-log
#'   plot, e.g., \code{c(10, 1000)}. Values will be rounded down and up,
#'   respectively, to a round number. If left as the default NA, the Y axis
#'   limits for the semi-log plot will be automatically selected.
#' @param y_axis_label optionally supply a character vector or an expression to
#'   use for the y axis label
#' @param legend_position Specify where you want the legend to be. Options are
#'   "left", "right", "bottom", "top", or "none" (default) if you don't want one
#'   at all. If you include the legend but then some graphs do have a legend and
#'   some graphs do not (e.g., some have effectors and some do not so there's
#'   nothing to put in a legend), the alignment between sets of graphs will be a
#'   bit off.
#' @param legend_label optionally indicate on the legend whether the effector is
#'   an inhibitor, inducer, activator, or suppressor. Input will be used as the
#'   label in the legend for the line style and the shape. If left as the
#'   default NA when a legend is included and an effector is present, the label
#'   in the legend will be "Inhibitor".
#' @param graph_titles optionally specify titles to be used in the graphs and
#'   specify the order in which the files are graphed or use "none" to have no
#'   titles on your graphs. Input should be a named character vector of the
#'   files in the order you would like and what you want to use for the title.
#'   The file name must \emph{perfectly} match the file name listed in
#'   ct_dataframe or it won't be used. An example of how this might be
#'   specified: \code{graph_titles = c("My file 1.xlsx" = "Healthy volunteers",
#'   "My file 2.xlsx" = "Mild hepatic impairment")}  If you get an order that
#'   you didn't think you specified, please double check that you have specified
#'   the file names \emph{exactly} as they appear in \code{ct_dataframe}.
#'   \strong{CAVEAT:} If you have more than one dataset per file, this is
#'   trickier. However, you can specify titles using the name of the simulator
#'   output file, the compound ID, the tissue, and then the ADAM-model
#'   subsection (use "none" if that doesn't apply here), each separated with a
#'   ".". An example: \code{graph_titles = c("my sim
#'   file.xlsx.substrate.plasma.none" = "Midazolam", "my sim file.xlsx.inhibitor
#'   1.plasma.none" = "Ketoconazole")} Please see the "Examples" section for an
#'   example with the dataset MDZ_Keto.
#' @param graph_title_size the font size for the graph title if it's included;
#'   default is 14
#' @param graph_labels TRUE (default) or FALSE for whether to include labels (A,
#'   B, C, etc.) for each of the small graphs.
#' @param ... arguments that pass through to \code{\link{ct_plot}}
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png"or "My conc time graph.docx".
#'   If you leave off ".png" or ".docx", it will be saved as a png file, but if
#'   you specify a different graphical file extension, it will be saved as that
#'   file format. Acceptable graphical file extensions are "eps", "ps", "jpeg",
#'   "jpg", "tiff", "png", "bmp", or "svg". Leaving this as NA means the file
#'   will not be automatically saved to disk, except when \code{graph_arrangment
#'   = "separate files"}, when anything you specify here will be ignored and it
#'   will be saved by file name anyway. \strong{WARNING:} SAVING TO WORD DOES
#'   NOT WORK ON SHAREPOINT OR THE LARGE FILE STORE. This is a Microsoft
#'   permissions issue, not an R issue. If you temporarily change your working
#'   directory to a local folder, it will work fine and you can copy those files
#'   later back to SharePoint or the Large File Store. We wish we had a better
#'   solution for this!
#' @param file_suffix optionally add a file suffix to explain what each graph
#'   it. For example, you might run this function once and with
#'   \code{figure_type = "means only"} and once with \code{figure_type =
#'   "percentiles"}, so you could set file_suffix to, e.g., "means only" for the
#'   former and "percentiles" for the latter, and the graph file names would be
#'   something like "abc-5mg-sd - means only.png" and "abc-5mg-sd -
#'   percentiles.png". This is only used when \code{graph_arrangement =
#'   "separate files"}.
#' @param fig_height figure height in inches; default is 8
#' @param fig_width figure width in inches; default is 8
#'
#' @return
#' @export
#'
#' @examples
#'
#' data(MDZct)
#' ct_plot_mult(ct_dataframe = MDZct)
#'
#' ct_plot_mult(ct_dataframe = MDZct,
#'    graph_titles = c("mdz-5mg-sd-fa1.xlsx" = "fa = 1",
#'                     "mdz-5mg-sd-fa0_8.xlsx" = "fa = 0.8",
#'                     "mdz-5mg-sd-fa0_6.xlsx" = "fa = 0.6",
#'                     "mdz-5mg-sd-fa0_4.xlsx" = "fa = 0.4"))
#'
#'
#' # Graph titles when you have the tricky situation of more than one
#' # dataset per file
#' ct_plot_mult(
#'     ct_dataframe = MDZ_Keto,
#'     graph_titles = c("mdz-qd-keto-qd.xlsx.substrate.plasma.none" = "Midazolam in plasma",
#'                      "mdz-qd-keto-qd.xlsx.substrate.blood.none" = "Midazolam in blood",
#'                      "mdz-qd-keto-qd.xlsx.inhibitor 1.plasma.none" = "Ketoconazole in plasma",
#'                      "mdz-qd-keto-qd.xlsx.inhibitor 1.blood.none" = "Ketoconazole in blood"))
#' 


ct_plot_mult <- function(ct_dataframe, 
                         obs_to_sim_assignment = NA,
                         graph_arrangement = "all together", 
                         figure_type = "percentiles",
                         mean_type = "arithmetic",
                         linear_or_log = "semi-log",
                         time_range = NA, 
                         x_axis_interval = NA, 
                         x_axis_label = NA,
                         pad_x_axis = TRUE, 
                         pad_y_axis = TRUE,
                         y_axis_limits_lin = NA, 
                         y_axis_limits_log = NA, 
                         y_axis_label = NA,
                         legend_position = "none",
                         legend_label = NA, 
                         graph_titles = NA,
                         graph_title_size = 14,
                         graph_labels = TRUE,
                         save_graph = NA,
                         file_suffix = NA,
                         fig_height = 8,
                         fig_width = 8,
                         ...){
    
    # error catching -------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.", 
             call. = FALSE)
    }
    
    if(nrow(ct_dataframe) == 0){
        stop("Please check your input. The data.frame you supplied for ct_dataframe doesn't have any rows.", 
             call. = FALSE)
    }
    
    # main body of function -----------------------------------------------
    
    # Setting up ct_dataframe
    ct_dataframe <- ct_dataframe %>% 
        mutate(subsection_ADAM = ifelse(is.na(subsection_ADAM),
                                        "none", subsection_ADAM),
               GraphLabs = paste(File, CompoundID, Tissue, subsection_ADAM, sep = "."))
    
    # Checking for situations where they'll get the same file name for more than
    # one set of data
    DatasetCheck <- ct_dataframe %>% filter(Simulated == TRUE) %>% 
        select(File, Tissue, CompoundID, subsection_ADAM, GraphLabs) %>% 
        unique()
    
    if(any(duplicated(DatasetCheck$File)) == FALSE){
        ct_dataframe <- ct_dataframe %>% 
            mutate(GraphLabs = File)
        DatasetCheck$GraphLabs <- DatasetCheck$File
    }
    
    # Setting up graph titles and order
    if(length(graph_titles) > 1 && any(complete.cases(graph_titles))){
        
        # If they have named some graph_titles but not all, fix that.
        graph_titles <- c(
            graph_titles, 
            setdiff(sort(unique(DatasetCheck$GraphLabs)), 
                    names(graph_titles)))
        
        # If graph_titles isn't named, make the names match the files themselves.
        if(is.null(names(graph_titles))){
            names(graph_titles) <- graph_titles
        }
        
        # If they named some but not all the values in graph_titles, name the
        # missing ones, too.
        if(any(is.na(names(graph_titles)) | names(graph_titles) == "")){
            names(graph_titles)[is.na(names(graph_titles))] <-
                DatasetCheck$GraphLabs[is.na(names(graph_titles))]
            
            names(graph_titles)[names(graph_titles) == ""] <-
                DatasetCheck$GraphLabs[names(graph_titles) == ""]
        }
        
        # Convert labels to file base names (this doesn't do anything to the
        # label if the user specified that. Well, as long as they didn't use
        # graph_titles that were the entire file path, which seems highly
        # unlikely.)
        file_label_names <- basename(names(graph_titles))
        graph_titles <- basename(graph_titles)
        names(graph_titles) <- file_label_names
        
        # If the user omitted any files that are included in ct_dataframe, grab
        # those now and tack them onto the end of graph_titles. This will allow
        # them to set the order of the files they DID specify but not omit files
        # that they forgot. The forgotten files just won't have pretty titles.
        graph_titles_all <- unique(c(names(graph_titles), 
                                     DatasetCheck$GraphLabs))
        
        # Name items in graph_titles_all according to graph_titles.
        names(graph_titles_all) <- graph_titles_all
        graph_titles_all[names(graph_titles)] <- as.character(graph_titles)
        
    } else {
        
        # Even if user didn't specify file order, we need the levels of that
        # factor later. Setting them here. 
        graph_titles_all <- sort(unique(DatasetCheck$GraphLabs))
        names(graph_titles_all) <- graph_titles_all
    }
    
    # Dealing with observed data. This is the scenario when any observed data
    # exist AND *either* any of the observed data are missing a value for File
    # *or* there are any values for obs_to_sim_assignment.
    if(any(ct_dataframe$Simulated == FALSE) & 
       (any(is.na(ct_dataframe$File[ct_dataframe$Simulated == FALSE])) |
        any(complete.cases(obs_to_sim_assignment)))){
        
        ObsCT <- ct_dataframe %>% filter(Simulated == FALSE)
        ct_dataframe <- ct_dataframe %>% filter(Simulated == TRUE)
        
        if(all(is.na(obs_to_sim_assignment))){
            # If there are no values assigned for File and the user did not
            # specify anything for obs_to_sim_assignment, then make all the
            # observed data go with all the simulated data.
            FileAssign <- expand_grid(ObsFile = ObsCT %>% pull(ObsFile) %>% unique(), 
                                      File = unique(ct_dataframe$File)) %>% 
                filter(complete.cases(File))
            suppressMessages(
                ObsCT <- FileAssign %>% full_join(ObsCT %>% select(-File))
            )
        } else {
            # If the user *did* specify values for obs_to_sim_assignment, then use
            # those for File.
            
            # Making sure that the split pattern will work in case the user omitted
            # spaces.
            obs_to_sim_assignment <- gsub(",[^ ]", ", ", obs_to_sim_assignment)
            ObsAssign <- str_split(obs_to_sim_assignment, pattern = ", ")
            
            if(all(sapply(ObsAssign, length) == 1)){
                ObsCT <- ObsCT %>% mutate(File = obs_to_sim_assignment[ObsFile])
            } else {
                ObsCT <- split(as.data.frame(ObsCT), f = ObsCT$ObsFile)
                
                for(j in 1:length(ObsAssign)){
                    FileAssign <- expand_grid(ObsFile = names(obs_to_sim_assignment)[j], 
                                              File = ObsAssign[[j]])
                    suppressMessages(
                        ObsCT[[j]] <- FileAssign %>% full_join(ObsCT[[j]] %>% select(-File))
                    )
                    rm(FileAssign)
                }
                ObsCT <- bind_rows(ObsCT)
            }
        }
        
        ct_dataframe <- ct_dataframe %>% bind_rows(ObsCT)
    }
    
    # Much easier to deal with things if we've got base file names in
    # ct_dataframe. Taking care of that here.
    ct_dataframe <- ct_dataframe %>% 
        mutate(File_bn = basename(File), 
               # Set the sort order in the data
               GraphLabs = factor(GraphLabs, levels = names(graph_titles_all)))
    
    AllGraphs <- list()
    
    if(is.na(graph_titles[1])){
        
        # Splitting the data, which we're about to do, messes up the order b/c you
        # have to split on character data rather than factor. Getting the order of
        # the factors here.
        getOrder <- function(x){
            Out <- levels(x)
            if(is.null(Out)){
                Out <- sort(unique(x))
            }
            return(Out)
        }
        
        if(any(duplicated(DatasetCheck$File))){
            
            Order <- expand.grid(list("File" = getOrder(ct_dataframe$File), 
                                      "CompoundID" = getOrder(ct_dataframe$CompoundID), 
                                      "Tissue" = getOrder(ct_dataframe$Tissue), 
                                      "subsection_ADAM" = getOrder(ct_dataframe$subsection_ADAM))) %>% 
                mutate(Order = paste(File, CompoundID, Tissue, subsection_ADAM, sep = ".")) %>% 
                pull(Order)
        } else {
            Order <- getOrder(ct_dataframe$File)
        }
        
    } else {
        Order <- names(graph_titles_all)
    }    
    
    if(any(duplicated(DatasetCheck$File))){
        ct_dataframe <- split(ct_dataframe, 
                              f = list(as.character(ct_dataframe$File),
                                       as.character(ct_dataframe$CompoundID), 
                                       as.character(ct_dataframe$Tissue), 
                                       as.character(ct_dataframe$subsection_ADAM)))
    } else {
        ct_dataframe <- split(ct_dataframe, f = as.character(ct_dataframe$File))
    }
    
    for(i in Order){
        if(nrow(ct_dataframe[[i]]) == 0){
            next
        }
        
        if(any(complete.cases(graph_titles)) && graph_titles[1] == "none"){
            Title_i <- NA
        } else if(any(duplicated(DatasetCheck$File))){
            if(any(names(graph_titles_all) != graph_titles_all)){
                Title_i <- graph_titles_all[i]
            } else {
                y <- str_split(i, pattern = "\\.")[[1]]
                Title_i <- paste(str_trim(
                    paste(paste(y[1], y[2], sep = "."), 
                          y[3], y[4], 
                          ifelse(y[5] == "none", "", y[5]))))
            }
        } else {
            Title_i <- graph_titles_all[i]
        }
        
        ct_dataframe[[i]] <- ct_dataframe[[i]] %>% 
            # need to convert subsection_ADAM back to NA if it was
            # changed above in order for this to work with ct_plot
            mutate(subsection_ADAM = ifelse(subsection_ADAM == "none",
                                            NA, subsection_ADAM))
        # print(i)
        # print(head(ct_dataframe[[i]]))
        
        AllGraphs[[i]] <- 
            ct_plot(ct_dataframe = ct_dataframe[[i]], 
                    figure_type = figure_type,
                    mean_type = mean_type,
                    linear_or_log = linear_or_log,
                    time_range = time_range, 
                    x_axis_interval = x_axis_interval, 
                    x_axis_label = x_axis_label,
                    pad_x_axis = pad_x_axis, 
                    pad_y_axis = pad_y_axis,
                    y_axis_limits_lin = y_axis_limits_lin, 
                    y_axis_limits_log = y_axis_limits_log, 
                    y_axis_label = y_axis_label,
                    legend_position = legend_position,
                    legend_label = legend_label, 
                    graph_labels = FALSE, 
                    graph_title = Title_i,
                    ..., # comment this when developing
                    graph_title_size = graph_title_size)
        
        rm(Title_i)
    }
    
    if(graph_labels){
        labels <- "AUTO"
    } else {
        labels <- NULL
    }
    
    if(graph_arrangement != "separate files"){
        
        # Checking on number of columns and rows requested
        if(graph_arrangement == "all together"){
            nrow <- NULL
            ncol <- NULL
        } else {
            graph_arrangement <- gsub(" ", "", tolower(graph_arrangement))
            graph_arrangement <- as.numeric(
                str_split(graph_arrangement, pattern = "x", simplify = TRUE))
            nrow <- graph_arrangement[1]
            ncol <- graph_arrangement[2]
            
            # Checking that these are sensible numbers. If there are blank spots
            # where there aren't enough graphs -- for example, there are only 3
            # graphs and they asked for 2 rows and 2 columns -- that's ok, but
            # this gives unexpected output if there are not enough slots to fit
            # all the graphs.
            if(nrow * ncol < length(AllGraphs)){
                warning(paste0("You requested ", nrow, " row(s) and ", 
                               ncol, " column(s) of graphs, which allows space for up to ", 
                               nrow * ncol, " graphs. However, you have ", 
                               length(AllGraphs), " graphs. We're going to guess at more reasonable numbers of rows and columns for you."),
                        call. = FALSE)
                nrow <- NULL
                ncol <- NULL
            }
        }
        
        if(legend_position == "none"){
            Out <- suppressWarnings(
                ggpubr::ggarrange(plotlist = AllGraphs, 
                                  nrow = nrow, 
                                  ncol = ncol, 
                                  labels = labels, align = "hv"))
            
        } else {
            Out <- suppressWarnings(
                ggpubr::ggarrange(plotlist = AllGraphs, 
                                  nrow = nrow, 
                                  ncol = ncol, 
                                  common.legend = TRUE,
                                  legend = legend_position,
                                  labels = labels, align = "hv"))
        }
        
        if(complete.cases(save_graph)){
            FileName <- save_graph
            if(str_detect(FileName, "\\.")){
                # Making sure they've got a good extension
                Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
                FileName <- sub(paste0(".", Ext), "", FileName)
                Ext <- ifelse(Ext %in% c("eps", "ps", "jpeg", "tiff",
                                         "png", "bmp", "svg", "jpg", "docx"), 
                              Ext, "png")
                FileName <- paste0(FileName, ".", Ext)
            } else {
                FileName <- paste0(FileName, ".png")
                Ext <- "png"
            }
            
            if(Ext == "docx"){
                
                # This is when they want a Word file as output
                OutPath <- dirname(FileName)
                if(OutPath == "."){
                    OutPath <- getwd()
                }
                
                # Check for whether they're trying to save on SharePoint, which DOES
                # NOT WORK. If they're trying to save to SharePoint, instead, save
                # to their Documents folder.
                
                # Side regex note: The myriad \ in the "sub" call are necessary b/c
                # \ is an escape character, and often the SharePoint and Large File
                # Store directory paths start with \\\\.
                if(str_detect(sub("\\\\\\\\", "//", OutPath), SimcypDir$SharePtDir)){
                    
                    OutPath <- paste0("C:/Users/", Sys.info()[["user"]], 
                                      "/Documents")
                    warning(paste0("You have attempted to use this function to save a Word file to SharePoint, and Microsoft permissions do not allow this. We will attempt to save the ouptut to your Documents folder, which we think should be ", 
                                   OutPath,
                                   ". Please copy the output to the folder you originally requested or try saving locally or on the Large File Store."), 
                            call. = FALSE)
                }
                
                LFSPath <- str_detect(sub("\\\\\\\\", "//", OutPath), SimcypDir$LgFileDir)
                
                if(LFSPath){
                    # Create a temporary directory in the user's AppData/Local/Temp
                    # folder.
                    TempDir <- tempdir()
                    
                    # Upon exiting this function, delete that temporary directory.
                    on.exit(unlink(TempDir))
                    
                }
                
                FileName <- basename(FileName)
                
                rmarkdown::render(system.file("rmarkdown/templates/multctplot/skeleton/skeleton.Rmd",
                                              package="SimcypConsultancy"), 
                                  output_dir = switch(as.character(LFSPath), 
                                                      "TRUE" = TempDir,
                                                      "FALSE" = OutPath),
                                  output_file = FileName, 
                                  quiet = TRUE)
                # Note: The "system.file" part of the call means "go to where the
                # package is installed, search for the file listed, and return its
                # full path.
                
                if(LFSPath){
                    file.copy(file.path(TempDir, FileName), OutPath, overwrite = TRUE)
                }
                
            } else {
                # This is when they want any kind of graphical file format.
                ggsave(FileName, height = fig_height, width = fig_width, dpi = 600, 
                       plot = Out)
                
            }
        }
        
        return(Out)
        
    } else {
        
        for(i in names(AllGraphs)){
            
            Filename <- paste0(gsub("\\.xlsx.*", "", basename(i)), 
                               ifelse(complete.cases(file_suffix),
                                      paste0(" - ", file_suffix), ""), ".png")
            
            if(any(duplicated(DatasetCheck$File))){
                Split_i <- str_split(i, pattern = "\\.")[[1]]
                Filename <- paste0(Split_i[3], " ", Split_i[4], " ",
                                   ifelse(Split_i[5] == "none", "", 
                                          paste0(" subsection ADAM ", Split_i[5])),
                                   Filename)
            } 
            
            ggsave(Filename, 
                   height = fig_height, width = fig_width, dpi = 600, 
                   plot = AllGraphs[[i]])
        }
    }
    
}

