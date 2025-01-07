#' Insert the standard Certara UK Limited copyright box that appears in compound
#' summary files
#'
#' @param font font to use. Default is "Fourier" and any fonts available on your
#'   machine in either Word or PowerPoint should be acceptable. If you get Times
#'   New Roman in your table when you asked for something else, it means that
#'   that font isn't available or maybe wasn't spelled the way R is expecting
#'   it. For example, "Calibri" works but "Calibri (Body)" doesn't even though
#'   the latter is listed in PowerPoint and Word.
#'
#' @return a flextable with the current copyright information in it
#' @export
#'
#' @examples
#' insert_copyright()
#' 
insert_copyright <- function(font = "Fourier"){
   
   # Error catching ---------------------------------------------------------
   # Check whether tidyverse is loaded
   if("package:tidyverse" %in% search() == FALSE){
      stop("The SimcypConsultancy R package requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run\nlibrary(tidyverse)\n    ...and then try again.", 
           call. = FALSE)
   }
   
   # Catching instances where the font name isn't *exactly* the same as what's
   # in Word or PowerPoint. Will have to slowly gather examples of this.
   font <- case_when(
      # "Calibri (Body)" dosen't work; just "Calibri" does.
      str_detect(font, "Calibri") ~ "Calibri", 
      .default = font)
   
   
   # Main body of function ----------------------------------------------------
   
   flextable::flextable(data.frame(
      Copyright = c(paste0("©Certara UK Limited 2001 - ", 
                           lubridate::year(Sys.Date())), 
                    "Copyright in this document belongs to Certara UK limited. Contents of this document may not be used, sold, licensed, transferred, copied or reproduced in whole or in any part or in any manner or form", 
                    "without the prior written consent of Certara UK Limited.", 
                    "The recipient of this material shall undertake to respect and preserve the confidentiality of such information.", 
                    "Certara UK (Simcyp Division),", 
                    "Level 2 - Acero, 1 Concourse Way,", 
                    "Sheffield, S1 2BJ, United Kingdom"))) %>% 
      flextable::bold(i = c(1, 3)) %>% 
      flextable::delete_part(part = "header") %>%
      flextable::width(width = 6, unit = "in") %>% 
      flextable::align(align = "center", part = "all") %>% 
      # Set the font
      flextable::font(part = "all", 
                      fontname = font) %>% 
      flextable::fontsize(i = 1, size = 14) %>% 
      flextable::fontsize(i = 3, size = 14) %>% 
      flextable::border_outer(border = officer::fp_border(width = 2)) %>% 
      flextable::padding(i = 1, padding.top = 12) %>% 
      flextable::padding(i = 2, padding.bottom = 0) %>% 
      flextable::padding(i = 5:7, padding = 0) %>% 
      flextable::padding(i = 7, padding.bottom = 12)
   
}


