#' Make graphs for multiple Simulator output files at once
#'
#' This function was designed for making nicely arranged concentration-time
#' graphs from several Simcyp Simulator output files all together or for making
#' multiple files -- one for each Simulator file -- all at once.
#'
#' \strong{A note on the order of the graphs:} This function arranges graphs
#' first by file, then by compound ID, and then by tissue, and all sorting is
#' alphabetical. However, since sorting alphabetically might not be the optimal
#' graph arrangement for your scenario, you \emph{can} specify the order of the
#' graphs using either the \code{file_order} argument or, if you're comfortable
#' with setting factors in R, by making any of File, CompoundID, Tissue, and
#' subsection_ADAM factor rather than character data and setting the levels how
#' you wish. If you're unfamiliar with setting factor levels in R and setting
#' \code{file_order} isn't achieving what you want, please ask a member of the R
#' Working Group for assistance.
#'
#' @param ct_dataframe the data.frame with multiple sets of concentration-time
#'   data
#' @param single_out_file TRUE (default) or FALSE for whether to make multiple
#'   output files. TRUE means that there will be a single file with all of the
#'   graphs nicely arranged together. FALSE means that each Simulator output
#'   file will have its own output file, and that graph's output file will be
#'   named to match the Simulator output file name.
#' @param file_order optionally specify the order in which the files are graphed
#'   with a character vector of the files in the order you would like. (Not
#'   applicable if \code{single_out_file = FALSE}.) If you would like to include
#'   graph titles to make it clear which graph is which, you can also specify
#'   what the title should be for that file. Example of acceptable input:
#'   \code{c("simfile3.xlsx" = "10 mg SD", "simfile1.xlsx" = "50 mg SD",
#'   "simfile1.xlsx" = "100 mg SD")}. If you get an order that you didn't think
#'   you specified, please double check that you have specified the file names
#'   \emph{exactly} as they appear in \code{ct_dataframe}.
#' @param graph_labels TRUE or FALSE for whether to include labels (A, B, C,
#'   etc.) for each of the small graphs. (Not applicable if
#'   \code{single_out_file = FALSE}.)
#' @param nrow number of rows of small graphs in the arrangement. If left as NA,
#'   a reasonable guess will be made. (Not applicable if \code{single_out_file =
#'   FALSE}.)
#' @param ncol number of columns of small graphs in the arrangement. If left as
#'   NA, a reasonable guess will be made. (Not applicable if
#'   \code{single_out_file = FALSE}.)
#' @param linear_or_log the type of graph to be returned. Options: "semi-log",
#'   "linear", "both vertical" (default, graphs are stacked vertically), or
#'   "both horizontal" (graphs are side by side).
#' @param ... arguments that pass through to \code{\link{ct_plot}}
#' @param include_title TRUE or FALSE on whether to include a title for each
#'   small graph. NOTE: This is not currently working well when you choose to
#'   get both linear and semi-log plots; the title overlaps the graph. UNDER
#'   CONSTRUCTION.
#' @param legend_position Specify where you want the legend to be. Options are
#'   "left", "right", "bottom", "top", or "none" (default) if you don't want one
#'   at all.
#' @param save_graph optionally save the output graph by supplying a file name
#'   in quotes here, e.g., "My conc time graph.png". If you leave off ".png", it
#'   will be saved as a png file, but if you specify a different file extension,
#'   it will be saved as that file format. Acceptable extensions are "eps",
#'   "ps", "jpeg", "jpg", "tiff", "png", "bmp", or "svg". Leaving this as NA
#'   means the file will not be automatically saved to disk. (Not applicable if
#'   \code{single_out_file = FALSE} because each graph will be named according
#'   to its Simulator output file.)
#' @param fig_height figure height in inches; default is 8
#' @param fig_width figure width in inches; default is 8
#'
#' @return
#' @export
#'
#' @examples
#' # No examples yet
#' 
ct_plot_mult <- function(ct_dataframe, 
                         single_out_file = TRUE, 
                         file_order = NA,
                         linear_or_log = "semi-log",
                         include_title = TRUE,
                         graph_labels = TRUE,
                         legend_position = "none",
                         nrow = NULL, 
                         ncol = NULL,
                         save_graph = NA,
                         fig_height = 8,
                         fig_width = 8,
                         ...){
    
    # Much easier to deal with things if we've got base file names in
    # ct_dataframe. Taking care of that here.
    ct_dataframe <- ct_dataframe %>% 
        mutate(File_bn = basename(File))
    
    if(length(file_order) > 1 && complete.cases(file_order[1])){
        
        file_order <- basename(file_order)
        
        # If file_order isn't named, make the names match the files themselves.
        if(is.null(names(file_order))){
            names(file_order) <- file_order
        }
        
        # If they named some but not all the files, name the missing ones, too. 
        if(any(is.na(names(file_order)))){
            names(file_order)[is.na(names(file_order))] <- 
                file_order[is.na(names(file_order))]
        }
        
        # If the user omitted any files that are included in ct_dataframe,
        # grab those now and tack them onto the end of file_order. This will
        # allow them to set the order of the files they DID specify but not omit
        # files that they forgot. The forgotten files just won't have pretty
        # titles.
        file_order_all <- unique(c(names(file_order), 
                                   sort(unique(as.character(ct_dataframe$File_bn)))))
        
        # Name items in file_order_all according to file_order.
        names(file_order_all) <- file_order_all
        file_order_all[names(file_order)] <- as.character(file_order)
        
    } else {
        
        # Even if user didn't specify file order, we need the levels of that
        # factor later. Setting them here. 
        file_order_all <- sort(unique(ct_dataframe$File_bn))
        names(file_order_all) <- file_order_all
        
    }
    
    # Set the sort order in the data
    ct_dataframe <- ct_dataframe %>% 
        mutate(File_bn = factor(File_bn, levels = names(file_order_all)))
    
    AllGraphs <- list()
    AllData <- ct_dataframe %>% 
        mutate(subsection_ADAM = ifelse(is.na(subsection_ADAM),
                                        "none", subsection_ADAM))
    
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
    
    Order <- expand.grid(list("File" = getOrder(AllData$File_bn), 
                              "CompoundID" = getOrder(AllData$CompoundID), 
                              "Tissue" = getOrder(AllData$Tissue), 
                              "subsection_ADAM" = getOrder(AllData$subsection_ADAM))) %>% 
        mutate(Order = paste(File, CompoundID, Tissue, subsection_ADAM, sep = ".")) %>% 
        pull(Order)
    
    
    # Titles <- c("File" = length(sort(unique(AllData$File))), 
    #             "CompoundID" = length(sort(unique(AllData$CompoundID))),
    #             "Tissue" = length(sort(unique(AllData$Tissue))),
    #             "subsection_ADAM" = length(sort(unique(AllData$subsection_ADAM))))
    # Titles <- Titles[which(Titles != 1)]
    
    AllData <- split(AllData, 
                     f = list(as.character(AllData$File_bn),
                              as.character(AllData$CompoundID), 
                              as.character(AllData$Tissue), 
                              as.character(AllData$subsection_ADAM)))
    
    # TitlesDF <- data.frame(AllData = names(AllData), 
    #                        TitleType = names(Titles)) %>% 
    #     mutate(Title = ifelse(TitleType == "File"))
    
    for(i in Order){
        Title_i <- file_order_all[as.character(unique(AllData[[i]]$File_bn))]
        
        AllData[[i]] <- AllData[[i]] %>% 
            # need to convert subsection_ADAM back to NA if it was
            # changed above in order for this to work with ct_plot
            mutate(subsection_ADAM = ifelse(subsection_ADAM == "none",
                                            NA, subsection_ADAM))
        # print(i)
        # print(head(AllData[[i]]))
        
        AllGraphs[[i]] <- 
            ct_plot(ct_dataframe = AllData[[i]], 
                    ...,
                    include_legend = ifelse(legend_position == "none",
                                            FALSE, TRUE),
                    linear_or_log = linear_or_log)
        
        if(linear_or_log %in% c("linear", "log", "semi-log") &
           include_title){
            AllGraphs[[i]] <- AllGraphs[[i]] + ggtitle(Title_i) +
                theme(title = element_text(size = 10))
        } else {
            if(include_title){
                AllGraphs[[i]] <- ggpubr::ggarrange(AllGraphs[[i]],
                                                    labels = Title_i)
                # Need a way to increase the viewport b/c vjust = 0 or label.y = 1.5
                # both put the title off the top of the graph. Not sure how to do
                # this.
            }
        }
        
        rm(Title_i)
    }
    
    if(graph_labels){
        labels <- "AUTO"
    } else {
        labels <- NULL
    }
    
    if(single_out_file){
        
        if(legend_position == "none"){
            Out <- ggpubr::ggarrange(plotlist = AllGraphs, 
                                     nrow = nrow, 
                                     ncol = ncol, 
                                     labels = labels)
            
        } else {
            Out <- ggpubr::ggarrange(plotlist = AllGraphs, 
                                     nrow = nrow, 
                                     ncol = ncol, 
                                     common.legend = TRUE,
                                     legend = legend_position,
                                     labels = labels)
        }
        
        if(complete.cases(save_graph)){
            FileName <- save_graph
            if(str_detect(FileName, "\\.")){
                # Making sure they've got a good extension
                Ext <- sub("\\.", "", str_extract(FileName, "\\..*"))
                FileName <- sub(paste0(".", Ext), "", FileName)
                Ext <- ifelse(Ext %in% c("eps", "ps", "jpeg", "tiff",
                                         "png", "bmp", "svg", "jpg"), 
                              Ext, "png")
                FileName <- paste0(FileName, ".", Ext)
            } else {
                FileName <- paste0(FileName, ".png")
            }
            
            ggsave(FileName, height = fig_height, width = fig_width, dpi = 600, 
                   plot = Out)
            
        }
        
    } else {
        
        for(i in names(AllGraphs)){
            ggsave(paste0(basename(gsub("\\.xlsx.*", "", i)), ".png"), 
                   height = fig_height, width = fig_width, dpi = 600, 
                   plot = AllGraphs[[i]])
        }
    }
    
    return(Out)
    
}

