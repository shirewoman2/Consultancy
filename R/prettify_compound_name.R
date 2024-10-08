#' Make a compound name prettier and more human readable
#'
#' @param CompoundName a compound name or vector of names to be prettified such
#'   as "Sim-Midazolam" or "Sim-Ketoconazole-400 mg QD", which will become
#'   "midazolam" and "ketoconazole".
#' @param case set the case to be "lower" (default), "title" (only the first
#'   letter in each word is capitalized), or "original" (nothing about the case
#'   changes). If you set the case to "lower" or "title", compounds with "OH-",
#'   "O-", "S-", or "R-" as a prefix will still have that capitalized, and if
#'   anyone has any other common exceptions they'd like, email Laura Shireman.
#'
#' @return a vector of prettier compound names
#' @export
#'
#' @examples
#'
#' prettify_compound_name(c("Sim-Midazolam",
#'                          "Sim-Ketoconazole-400 mg QD",
#'                          "none"))
#' 
prettify_compound_name <- function(CompoundName, case = "lower"){
    
    CompoundName <-
        gsub(
            "[sS][vV]-|[sS]im-|[wW][sS][pP]-|_[eE][cC]|_[sS][rR]|-[mM][dD]|-[sS][dD]|_[fF][oO]|-[1-9]00 mg [QqMmSsTtBbIidD]{1,3}|_fasted soln|_fed capsule|RES-",
            "", CompoundName)
    
    # Dealing with case
    CompoundName <- switch(case, 
                           "lower" = tolower(CompoundName), 
                           "title" = str_to_title(CompoundName), 
                           "original" = CompoundName)
    
    # Adjusting for compounds (metabolites) w/"OH" in name or other
    # idiosyncracies
    CompoundName <- sub("^oh ", "OH-", CompoundName)
    CompoundName <- sub("oh-", "OH-", CompoundName)
    CompoundName <- sub("o-", "O-", CompoundName)
    CompoundName <- sub("s-", "S-", CompoundName)
    CompoundName <- sub("r-", "R-", CompoundName)
    CompoundName <- sub("_fasted soln|_fed capsule", "", CompoundName)
    CompoundName <- sub("OH-itraconazole", "hydroxyitraconazole", CompoundName)
    
    # Misspelled compound files
    CompoundName <- sub("levenorgestrel", "levonorgestrel", CompoundName)
    
    return(CompoundName)
}

