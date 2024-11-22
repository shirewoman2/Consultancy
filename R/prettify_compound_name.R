#' Make a compound name prettier and more human readable
#'
#' @param compound_name a compound name or vector of names to be prettified such
#'   as "Sim-Midazolam" or "Sim-Ketoconazole-400 mg QD", which will become
#'   "midazolam" and "ketoconazole".
#' @param case set the case to be "lower" (default), "title" (only the first
#'   letter in each word is capitalized), or "original" (nothing about the case
#'   changes). If you set the case to "lower" or "title", compounds with "OH-",
#'   "O-", "S-", or "R-" as a prefix will still have that capitalized, and if
#'   anyone has any other common exceptions they'd like, email Laura Shireman.
#' @param force TRUE or FALSE (default) for whether to force cleanup of compound
#'   names regardless of whether the compound looks like it was a Simcyp
#'   Simulator compound. Leaving this as FALSE means that, e.g.,
#'   "Sim-Ketoconazole-400 mg QD" will become "ketoconazole", but if your client
#'   has a drug named ABC123 -- so it doesn't look like a Simcyp Simulator
#'   compound -- it will be left alone. 
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
prettify_compound_name <- function(compound_name, 
                                   case = "lower", 
                                   force = FALSE){
   
   OrigCompoundName <- compound_name
   
   # Removing Simulator decorations
   SimText <- "[sS][vV]-|[sS]im-|[wW][sS][pP]-|_[eE][cC]|_[sS][rR]|-[mM][dD]|-[sS][dD]|_[fF][oO]|-[1-9]00 mg [QqMmSsTtBbIidD]{1,3}|_fasted soln|_fed capsule|RES-"
   compound_name <- gsub(SimText, "", compound_name)
   
   SimCompounds <- c("Alfentanil", "Alprazolam", "Atomoxetine", "Atorvastatin", 
                     "Bufuralol", "Buprenorphine", "Bupropion", "Caffeine", 
                     "Celecoxib", "Clozapine", "Crizotinib", "Cyclosporine", 
                     "Dabigatran", "Desipramine", "Dexamethasone", 
                     "Dextromethorphan", "Digoxin", "Drospirenone", 
                     "Duloxetine", "Esomeprazole", "Ethinylestradiol", 
                     "Fedratinib", "Flurbiprofen", "Gemfibrozil", 
                     "Glyburide", "Ibrutinib", "Imipramine", "Itraconazole", 
                     "Lansoprazole", "Lorazepam", "Metformin", "Metoprolol", 
                     "Midazolam", "Montelukast", "Nebivolol", "Nifedipine", 
                     "Omeprazole", "Ondansetron", "Oxycodone", "Phenacetin", 
                     "Pioglitazone", "Pravastatin", "Quinidine", "Raltegravir", 
                     "Repaglinide", "Rivaroxaban", "Rosiglitazone", 
                     "Rosuvastatin", "Ruxolitinib", "Mephenytoin", "Warfarin", 
                     "Sildenafil", "Simvastatin", "Siponimod", "Tenofovir", 
                     "Theophylline", "Tolbutamide", "Tolterodine", "Triazolam", 
                     "Valsartan", "Voriconazole", "Zidovudine", "Zolpidem",
                     "Adalimumab", "Atezolizumab", "Efalizumab", "Etanercept", 
                     "Omalizumab", "Trastuzumab")
   
   # Dealing with case
   compound_name <- switch(case, 
                           "lower" = tolower(compound_name), 
                           "title" = str_to_title(compound_name), 
                           "original" = compound_name)
   
   # Adjusting for compounds (metabolites) w/"OH" in name or other
   # idiosyncracies
   compound_name <- sub("^oh ", "OH-", compound_name)
   compound_name <- sub("oh-", "OH-", compound_name)
   compound_name <- sub(" o-", " O-", compound_name)
   compound_name <- sub("-o-", "-O-", compound_name)
   compound_name <- sub("^o-", "O-", compound_name)
   compound_name <- sub(" n-", " N-", compound_name)
   compound_name <- sub("-n-", "-N-", compound_name)
   compound_name <- sub("^n-", "N-", compound_name)
   compound_name <- sub("^s-", "S-", compound_name)
   compound_name <- sub("^r-", "R-", compound_name)
   compound_name <- sub("_fasted soln|_fed capsule", "", compound_name)
   compound_name <- sub("OH-itraconazole", "hydroxyitraconazole", compound_name)
   
   # Misspelled compound files
   compound_name <- sub("levenorgestrel", "levonorgestrel", compound_name)
   
   # Not changing compounds that are likely not from the Simulator if that's
   # what the user wants
   if(force == FALSE){
      compound_name[which(str_detect(OrigCompoundName, 
                                     str_c(c(SimText, SimCompounds), 
                                           collapse = "|")) == FALSE)] <- 
         OrigCompoundName[which(str_detect(OrigCompoundName, 
                                           str_c(c(SimText, SimCompounds), 
                                                 collapse = "|")) == FALSE)]
   }
   
   return(compound_name)
}

