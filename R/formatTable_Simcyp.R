#' Format tables according to Simcyp Consultancy Team specfications
#'
#' \code{formatTable_Simcyp} was designed to work with output from
#' \code{\link{pksummary_table}} and \code{\link{pksummary_mult}}, and it
#' formats tables so that the column headings and the first column are bold, and
#' the second through the last columns are centered. Column headings with, e.g.,
#' "AUCinf" or "Cmax" will have the "inf" or the "max" subscripted, and the
#' table will automatically expand to fit the contents.
#'
#' @param DF a data.frame, usually output from \code{\link{pksummary_table}} or
#'   \code{\link{pksummary_mult}}
#' @param fontsize the numeric font size for the output. Default is 11 point.
#'
#' @return
#' @export
#'
#' @examples 
#' 
#' pksummary_table(sim_data_file = "My simulation.xlsx")$Table %>% 
#'     formatTable_Simcyp()
#' 
formatTable_Simcyp <- function(DF, fontsize = 11){
    
    FT <- DF %>% 
        flextable::flextable() %>% 
        
        # Make the header bold
        flextable::bold(part = "header") %>%
        
        # Make the 1st column bold
        flextable::bold(j = 1, part = "body") %>% 
        
        # center the header row
        flextable::align(align = "center", part = "header") %>% 
        
        # center the columns with numbers, i.e., the 2nd column through the
        # penultimate column
        flextable::align(align = "center", j = 2:ncol(DF)) %>%
        
        # Set the font size
        flextable::fontsize(part = "all", size = fontsize) %>% 
        
        # setting up which borderlines to show
        flextable::border_remove() %>% 
        flextable::border_inner_v(part = "all", 
                                  border = officer::fp_border(width = 0.5)) %>% 
        flextable::border_outer(border = officer::fp_border(width = 0.5)) %>% 
        
        # making the width autofitted to contents
        flextable::set_table_properties(width = 1, layout = "autofit")
    
    # Dealing with subscripts
    NamePattern <- " AUCt | AUCinf | AUCtau | Cmax | Cmin | tmax | fa| fg| fh| ka| tlag"
    
    ColNames <- data.frame(OrigName = names(DF)) %>% 
        mutate(Part1 = sapply(OrigName, 
                              FUN = function(x) {str_split(x, pattern = NamePattern)[[1]][1]}), 
               Part2 = sub("inf|tau|max|min|a|g|h|lag", "", str_trim(str_extract(OrigName, NamePattern))), 
               Part2 = sub("AUCt", "AUC", Part2),
               Subscript = sub(" AUC| C| t| f| k", "", str_extract(OrigName, NamePattern)), 
               Part3 = sapply(OrigName, 
                              FUN = function(x) {str_split(x, pattern = NamePattern)[[1]][2]})) 
    
    for(i in 1:nrow(ColNames)){
        
        if(complete.cases(ColNames$Subscript[i])){
            FT <- FT %>% 
                flextable::compose(part = "header",
                                   j = which(names(DF) == ColNames$OrigName[i]),
                                   value = flextable::as_paragraph(ColNames$Part1[i], " ",
                                                                   ColNames$Part2[i],
                                                                   flextable::as_sub(ColNames$Subscript[i]), 
                                                                   ColNames$Part3[i]))
        }
    }
    
    return(FT)
    
}


