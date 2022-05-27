#' Make graphs for multiple Simulator output files at once
#'
#' This function was designed for making nicely arranged concentration-time
#' graphs from several Simcyp Simulator output files all together \emph{or} for
#' making multiple files -- one for each Simulator file -- all at once.
#'
#' \strong{A note on the order of the graphs:} This function arranges graphs
#' first by file, then by compound ID, and then by tissue, and all sorting is
#' alphabetical. However, since sorting alphabetically might not be the optimal
#' graph arrangement for your scenario, you \emph{can} specify the order of the
#' graphs using either the \code{file_labels} argument or, if you're comfortable
#' with setting factors in R, by making any of File, CompoundID, Tissue, and
#' subsection_ADAM factor rather than character data and setting the levels how
#' you wish. If you're unfamiliar with setting factor levels in R and setting
#' \code{file_labels} isn't achieving what you want, please ask a member of the
#' R Working Group for assistance.
#'
#' @param ct_dataframe the data.frame with multiple sets of concentration-time
#'   data
#' @param single_out_file TRUE (default) or FALSE for whether to make multiple
#'   output files. TRUE means that there will be a single file with all of the
#'   graphs nicely arranged together. FALSE means that each Simulator output
#'   file will have its own output file, and that graph's output file will be
#'   named to match the Simulator output file name.
#' @param file_labels optionally specify a label to be used for the file name in
#'   the graphs (this applies when \code{include_title = TRUE}) and/or specify
#'   the order in which the files are graphed with a named character vector of
#'   the files in the order you would like. (Not applicable if
#'   \code{single_out_file = FALSE}.) The file name must \emph{perfectly} match
#'   the file name listed in ct_dataframe or it won't be used. An example of how
#'   this might be specified: \code{alt_title = c("My file 1.xlsx" = "Healthy
#'   volunteers", "My file 2.xlsx" = "Mild hepatic impairment")}  If you get an
#'   order that you didn't think you specified, please double check that you
#'   have specified the file names \emph{exactly} as they appear in
#'   \code{ct_dataframe}.
#' @param graph_labels TRUE or FALSE for whether to include labels (A, B, C,
#'   etc.) for each of the small graphs. (Not applicable if
#'   \code{single_out_file = FALSE}.)
#' @param nrow number of rows of small graphs in the arrangement. If left as NA,
#'   a reasonable guess will be made. (Not applicable if \code{single_out_file =
#'   FALSE}.)
#' @param ncol number of columns of small graphs in the arrangement. If left as
#'   NA, a reasonable guess will be made. (Not applicable if
#'   \code{single_out_file = FALSE}.)
#' @param linear_or_log the type of graph to be returned. Options: "semi-log"
#'   (default), "linear", "both vertical" (graphs are stacked vertically), or
#'   "both horizontal" (graphs are side by side).
#' @param ... arguments that pass through to \code{\link{ct_plot}}
#' @param include_title TRUE or FALSE (default) on whether to include a title
#'   for each small graph.
#' @param legend_position Specify where you want the legend to be. Options are
#'   "left", "right", "bottom", "top", or "none" (default) if you don't want one
#'   at all. If you include the legend but then some graphs do have a legend and
#'   some graphs do not (e.g., some have effectors and some do not so there's
#'   nothing to put in a legend), the alignment between sets of graphs will be a
#'   bit off.
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
#' 
#' data(MDZct)
#' ct_plot_mult(ct_dataframe = MDZct) 
#' 
#' ct_plot_mult(ct_dataframe = MDZct, include_title = TRUE)
#' 
#' ct_plot_mult(ct_dataframe = MDZct, include_title = TRUE, 
#'    file_labels = c("mdz-5mg-sd-fa1.xlsx" = "fa = 1", 
#'                    "mdz-5mg-sd-fa0_8.xlsx" = "fa = 0.8", 
#'                    "mdz-5mg-sd-fa0_6.xlsx" = "fa = 0.6", 
#'                    "mdz-5mg-sd-fa0_4.xlsx" = "fa = 0.4"))
#' 
ct_plot_mult <- function(ct_dataframe, 
                         single_out_file = TRUE, 
                         linear_or_log = "semi-log",
                         graph_labels = TRUE,
                         include_title = FALSE,
                         file_labels = NA,
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
    
    if(length(file_labels) > 1 && complete.cases(file_labels[1])){
        
        # If file_labels isn't named, make the names match the files themselves.
        if(is.null(names(file_labels))){
            names(file_labels) <- file_labels
        }
        
        # If they named some but not all the files, name the missing ones, too. 
        if(any(is.na(names(file_labels)))){
            names(file_labels)[is.na(names(file_labels))] <- 
                file_labels[is.na(names(file_labels))]
        }
        
        # Convert labels to file base names (this doesn't do anything to the
        # label if the user specified that. Well, as long as they didn't use
        # file_labels that were the entire file path, which seems highly
        # unlikely.)
        file_label_names <- basename(names(file_labels))
        file_labels <- basename(file_labels)
        names(file_labels) <- file_label_names
        
        # If the user omitted any files that are included in ct_dataframe, grab
        # those now and tack them onto the end of file_labels. This will allow
        # them to set the order of the files they DID specify but not omit files
        # that they forgot. The forgotten files just won't have pretty titles.
        file_labels_all <- unique(c(names(file_labels), 
                                    sort(unique(as.character(ct_dataframe$File_bn)))))
        
        # Name items in file_labels_all according to file_labels.
        names(file_labels_all) <- file_labels_all
        file_labels_all[names(file_labels)] <- as.character(file_labels)
        
    } else {
        
        # Even if user didn't specify file order, we need the levels of that
        # factor later. Setting them here. 
        file_labels_all <- sort(unique(ct_dataframe$File_bn))
        names(file_labels_all) <- file_labels_all
        
    }
    
    # Set the sort order in the data
    ct_dataframe <- ct_dataframe %>% 
        mutate(File_bn = factor(File_bn, levels = names(file_labels_all)))
    
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
    
    AllData <- split(AllData, 
                     f = list(as.character(AllData$File_bn),
                              as.character(AllData$CompoundID), 
                              as.character(AllData$Tissue), 
                              as.character(AllData$subsection_ADAM)))
    
    for(i in Order){
        Title_i <- file_labels_all[as.character(unique(AllData[[i]]$File_bn))]
        
        AllData[[i]] <- AllData[[i]] %>% 
            # need to convert subsection_ADAM back to NA if it was
            # changed above in order for this to work with ct_plot
            mutate(subsection_ADAM = ifelse(subsection_ADAM == "none",
                                            NA, subsection_ADAM))
        # print(i)
        # print(head(AllData[[i]]))
        
        if(linear_or_log %in% c("linear", "log", "semi-log") | 
           include_title == FALSE){
            # This takes care of all plots where there isn't a title, including
            # when they want both linear and semi-log, and it works fine for
            # when user wants only one of linear or log and also wants a title.
            # If they want both types of graphs AND they want a title, though,
            # the spacing just doesn't work and we need to generate those graphs
            # separately.
            
            AllGraphs[[i]] <- 
                ct_plot(ct_dataframe = AllData[[i]], 
                        ..., # comment this when developing
                        include_graph_labels = FALSE,
                        include_legend = ifelse(legend_position == "none",
                                                FALSE, TRUE),
                        linear_or_log = linear_or_log)
            
            if(include_title){
                # This is for when they only want one of linear or log.
                AllGraphs[[i]] <- AllGraphs[[i]] + ggtitle(Title_i) +
                    theme(title = element_text(size = 10))
            }
            
        } else {
            # If the user wanted both linear and log plots AND they want a
            # title, we really need to just run ct_plot function twice for each
            # graph. Otherwise, we just don't have enough control over graph
            # placement and the spacing needed for titles.
            AllGraphs[[i]] <- 
                ggpubr::ggarrange(
                    plotlist = list(
                        # linear plot on top
                        ct_plot(ct_dataframe = AllData[[i]], 
                                include_graph_labels = FALSE,
                                ..., # comment this when developing
                                linear_or_log = "linear") +
                            ggtitle(Title_i) + 
                            theme(title = element_text(size = 10), 
                                  plot.margin = unit(c(0, 0, 0, 0), "lines")), 
                        
                        # semi-log on bottom
                        ct_plot(ct_dataframe = AllData[[i]], 
                                include_graph_labels = FALSE,
                                ..., # comment this when developing
                                linear_or_log = "semi-log")),
                    
                    align = "hv", 
                    nrow = ifelse(linear_or_log %in% c("both", "both vertical"), 
                                  2, 1), 
                    common.legend = TRUE, 
                    legend = legend_position)
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
                                     labels = labels, align = "hv")
            
        } else {
            Out <- ggpubr::ggarrange(plotlist = AllGraphs, 
                                     nrow = nrow, 
                                     ncol = ncol, 
                                     common.legend = TRUE,
                                     legend = legend_position,
                                     labels = labels, align = "hv")
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
        
        return(Out)
        
    } else {
        
        for(i in names(AllGraphs)){
            ggsave(paste0(basename(gsub("\\.xlsx.*", "", i)), ".png"), 
                   height = fig_height, width = fig_width, dpi = 600, 
                   plot = AllGraphs[[i]])
        }
    }
    
}

