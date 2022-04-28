#' Graph for checking accumulation over multiple doses -- UNDER CONSTRUCTION
#'
#' UNDER CONSTRUCTION -- DoseNum must be complete. mark_dosing doesn't work for
#' custom dosing intervals at present. Haven't done much to make this "nice"
#' yet. Just using it for a project I'm working on and thought I'd add it. -LSh
#'
#' @param CT data.frame w/conc time data from extractConcTime
#' @param t0 start time for compound being plotted
#' @param timepoint Options: Cmin, Cmax, C0, Clast -- Cmin would be whenever the
#'   min was, C0 would be 1st time point available for that dose, and Clast
#'   would be last time point available for that dose. Is that the best,
#'   clearest way to refer to those? Clast generally means the last for *all*
#'   times monitored, so I don't want it to be confusing.
#' @param mark_dosing "red dotted", "none", "blue dashed" etc. Any named R color
#'   and any named linetype.
#' @param diff_cutoff
#'
#' @return
#' @export
#'
#' @examples
#'
#' CT <- extractConcTime()
#'
#' 
check_accumulation <- function(CT, t0 = 0,
                               timepoint = "Clast",
                               mark_dosing = "red dotted",
                               diff_cutoff = 0.05){
    
    SScheck <- check %>% 
        group_by(DoseNum) %>% 
        # switch doesn't seem to work with summarize. Calculating each value.
        summarize(t0 = min(Time),
                  tlast = max(Time),
                  tmin = Time[which.min(Conc)],
                  tmax = Time[which.max(Conc)], 
                  Cmin = min(Conc), 
                  Cmax = max(Conc), 
                  C0 = Conc[which.min(Time)], 
                  Clast = Conc[which.max(Time)]) %>% 
        ungroup() %>% 
        mutate(Conc = switch(timepoint, 
                             "Cmin" = Cmin, 
                             "Cmax" = Cmax, 
                             "C0" = C0, 
                             "Clast" = Clast), 
               Time = switch(timepoint, 
                             "Cmin" = tmin, 
                             "Cmax" = tmax,
                             "C0" = t0, 
                             "Clast" = tlast)) %>% 
        mutate(PercDiff = c(NA, diff(Conc, lag = 1))/Conc, 
               DiffCriterion = abs(PercDiff) < diff_cutoff,
               DiffCriterion = ifelse(is.na(DiffCriterion), FALSE, DiffCriterion))
    
    LineAES <- str_split(mark_dosing, pattern = " ")[[1]]
    
    G <- ggplot(SScheck, aes(x = Time, y = Conc, color = DiffCriterion))
    
    if(mark_dosing != "none"){
        G <- G + 
            geom_vline(xintercept = SScheck$t0, 
                       color = LineAES[1], linetype = LineAES[2])
    }
    
    G <- G +
        geom_point(size = 1) + 
        labs(color = paste0("Difference from previous point < ", 100*diff_cutoff, "%")) +
        xlab("Time (h)") +
        ylab(paste(timepoint, "(ng/mL)"))  +
        scale_color_brewer(palette = "Set1") +
        theme(panel.background = element_rect(fill="white", color=NA),
              legend.key = element_rect(fill = "white"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(color = "black"),
              axis.title = element_text(color = "black", face = "bold"),
              axis.line.x.bottom = element_line(color = "black"),
              axis.line.y.left = element_line(color = "black"))
    
    return(G)
    
}


