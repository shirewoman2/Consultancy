#' Calculate the ratio of PK parameters between two simulations
#'
#' \code{calc_PK_ratios_mult} matches PK data from a pair of simulator output
#' Excel files and calculates the mean and confidence intervals of the ratios of
#' the requested PK parameters. To do this for multiple pairs of simulator
#' output files, please see the function \code{\link{calc_PK_ratios_mult}}.
#'
#' @param sim_data_file_numerator a simulator output Excel file that will
#'   provide the numerator for the calculated ratios.
#' @param sim_data_file_denominator a simulator output Excel file that will
#'   provide the denominator for the calculated ratios
#' @param paired TRUE (default) or FALSE for whether the study design is paired,
#'   as in, the subjects are \emph{identical} between the two simulations.
#'   \strong{THIS IS AN IMPORTANT DISTINCTION AND WILL AFFECT HOW THE
#'   CALCULATIONS ARE PERFORMED!} An example of a paired study would be a DDI
#'   study where each subject has a measurement without the effector of interest
#'   and then has a second measurement \emph{with} the effector. The comparison
#'   is for repeated measurements of the \emph{same subject}. An example of an
#'   unpaired study design would be comparing healthy volunteers to subjects
#'   with hepatic impairment because those are measurements on \emph{different}
#'   subjects. For paired study designs, the order of operations is to calculate
#'   each subject's mean ratio and then to calculate the mean of those ratios.
#'   For unpaired study designs, the order of operations is to calculate the
#'   mean for the numerator simulation and then divide it by the mean for the
#'   denominator simulation. Would this be clearer if you could see the
#'   mathematical equations? We agree but can't easily include equations in the
#'   help file. However, if you run this and save the output to a Word file, the
#'   equations will be included in the output.
#' @param compoundToExtract For which compound do you want to extract PK data?
#'   Options are: \itemize{\item{"substrate" (default),} \item{"primary
#'   metabolite 1",} \item{"primary metabolite 2",} \item{"secondary
#'   metabolite",} \item{"inhibitor 1" -- this can be an inducer, inhibitor,
#'   activator, or suppresesor, but it's labeled as "Inhibitor 1" in the
#'   simulator,} \item{"inhibitor 2" for the 2nd inhibitor listed in the
#'   simulation,} \item{"inhibitor 1 metabolite" for the primary metabolite of
#'   inhibitor 1}}
#' @param tissue For which tissue would you like the PK parameters to be pulled?
#'   Options are "plasma" (default) or "blood" (possible but not as thoroughly
#'   tested).
#' @param PKparameters PK parameters you want to extract from the simulator
#'   output file. Options are: \describe{
#'
#'   \item{"all"}{all possible parameters}
#'
#'   \item{"AUC tab"}{only those parameters on the "AUC" tab (default). The
#'   "AUC_CI" tab or "AUC_SD" tab will be used if "AUC" tab is not present.}
#'
#'   \item{"Absorption tab"}{only those parameters on the "Absorption" tab.
#'   Please note that we haven't developed this function for output in the
#'   "Overall Fa Fg" tab for ADAM-model simulations yet.}
#'
#'   \item{a vector of any combination of specific, individual parameters, each
#'   surrounded by quotes and encapsulated with \code{c(...)}}{An example:
#'   \code{c("Cmax_dose1", "AUCtau_last")}. To see the full set of possible
#'   parameters to extract, enter \code{view(PKParameterDefinitions)} into the
#'   console. Not case sensitive. If you use "_first" instead of "_dose1", that
#'   will also work.}
#'
#'   \item{a vector of individual parameters with one parameter for the
#'   numerator and whatever parameter you want from the other file for the
#'   denominator, separated by "/"}{The previous options are all for when you
#'   want to take the ratio of the \emph{same} parameter for file 1 / file 2.
#'   However, if you want to compare one PK parameter from file 1 with a
#'   \emph{different} parameter for file 2, you can do that with this option.
#'   Here's an example of how to input the parameters so that you can calculate
#'   the dose 1 AUCinf with an inhibitor present for file 1 divided by the
#'   AUCinf for dose 1 with no inhibitor (baseline) for file 2:
#'   \code{PKparameters = c("AUCinf_dose1_withInhib / AUCinf_dose1")} Please
#'   note that the quotes are around \emph{both} of the two parameters!}}
#'
#'   Currently, the PK data are only for the substrate unless noted, although
#'   you can sometimes hack around this by supplying a specific sheet to extract
#'   for a compound other than the substrate, e.g. sheet = "AUC(Sub Pri Met1)".
#'   This has NOT been as well tested, though, so be sure to check that you're
#'   getting what you expected!
#' @param sheet_PKparameters_num (optional) If you want the PK parameters for
#'   the numerator to be pulled from a specific tab in
#'   \code{sim_data_file_numerator}, list that tab here. Most of the time, this
#'   should be left as NA.
#' @param sheet_PKparameters_denom (optional) If you want the PK parameters for
#'   the numerator to be pulled from a specific tab in
#'   \code{sim_data_file_denominator}, list that tab here. Most of the time,
#'   this should be left as NA.
#' @param existing_exp_details If you have already run
#'   \code{\link{extractExpDetails_mult}} to get all the details from the "Input
#'   Sheet" (e.g., when you ran extractExpDetails_mult you said
#'   \code{exp_details = "Summary tab"} or \code{exp_details = "all"}), you can
#'   save some processing time by supplying that object here, unquoted. If left
#'   as NA, this function will run \code{extractExpDetails} behind the scenes to
#'   figure out some information about your experimental set up.
#' @param mean_type What kind of means and confidence intervals do you want
#'   listed in the output table? Options are "arithmetic" or "geometric"
#'   (default).
#' @param include_num_denom_columns TRUE (default) or FALSE for whether to
#'   include columns in the output table for the numerator data alone and
#'   columns for the denominator alone. For example, if you wanted to calculate
#'   the dose 1 AUC ratio for cancer patients compared to healthy volunteers,
#'   settting \code{include_num_denom_columns = TRUE} would give you that ratio
#'   and also a column with summary statistics on the AUC for cancer patients
#'   and a column with summary statistics on the AUC for healthy volunteers.
#'   Setting it to FALSE would give you only the ratios.
#' @param conf_int confidence interval to use; default is 90\%
#' @param includeCV TRUE (default) or FALSE for whether to include rows for CV
#'   in the table
#' @param includeConfInt TRUE (default) or FALSE for whether to include whatever
#'   confidence intervals were included in the simulator output file. Note that
#'   the confidence intervals are geometric since that's what the simulator
#'   outputs (see an AUC tab and the summary statistics; these values are the
#'   ones for, e.g., "90\% confidence interval around the geometric mean(lower
#'   limit)").
#' @param prettify_columns TRUE (default) or FALSE for whether to make easily
#'   human-readable column names. TRUE makes pretty column names such as "AUCinf
#'   (h*ng/mL)" whereas FALSE leaves the column with the R-friendly name from
#'   \code{\link{extractPK}}, e.g., "AUCinf_dose1". We're still tweaking this to
#'   make it look just right!
#' @param prettify_compound_names TRUE (default) or FALSE on whether to make
#'   compound names prettier in the prettified column titles and in any Word
#'   output files. This was designed for simulations where the substrate and any
#'   metabolites, effectors, or effector metabolites are among the standard
#'   options for the simulator, and leaving \code{prettify_compound_names =
#'   TRUE} will make the name of those compounds something more human readable.
#'   For example, "SV-Rifampicin-MD" will become "rifampicin", and
#'   "Sim-Midazolam" will become "midazolam". Set each compound to the name
#'   you'd prefer to see in your column titles if you would like something
#'   different. For example, \code{prettify_compound_names = c("inhibitor" =
#'   "teeswiftavir", "substrate" = "superstatin")}. Please note that "inhibitor"
#'   includes \emph{all} the effectors and effector metabolites present, so, if
#'   you're setting the effector name, you really should use something like this
#'   if you're including effector metabolites: \code{prettify_compound_names =
#'   c("inhibitor" = "teeswiftavir and 1-OH-teeswiftavir", "substrate" =
#'   "superstatin")}.
#' @param rounding option for what rounding to perform, if any. Options are:
#'   \describe{\item{NA or "Consultancy"}{All output will be rounded according
#'   to Simcyp Consultancy Team standards: to three significant figures when the
#'   value is < 100 or to the ones place if the value is >= 100. Please see the
#'   function \code{\link{round_consultancy}}, which does the rounding here.}
#'   \item{"none"}{No rounding will be performed.} \item{"significant X" where
#'   "X" is a number}{Output will be rounded to X significant figures. "signif
#'   X" also works fine.} \item{"round X" where "X" is a number}{Output will be
#'   rounded to X digits} \item{"Word only"}{Output saved to Word or a csv file
#'   will be rounded using the function \code{\link{round_consultancy}}, but
#'   nothing will be rounded in the output R object. This can be useful when you
#'   want to have nicely rounded and formatted output in a Word file but you
#'   \emph{also} want to use the results from \code{calc_PK_ratios} to make
#'   forest plots, which requires numbers that are \emph{not} rounded.}}
#' @param checkDataSource TRUE (default) or FALSE for whether to include in the
#'   output a data.frame that lists exactly where the data were pulled from the
#'   simulator output file. Useful for QCing.
#' @param returnExpDetails TRUE or FALSE (default) for whether to return the
#'   simulator experimental details, which this function will look up anyway
#'   behind the scenes. If TRUE, this will return a list, and each set of
#'   simulation details will be an item in that list.
#' @param save_table optionally save the output table and, if requested, the QC
#'   info, by supplying a file name in quotes here, e.g., "My nicely formatted
#'   table.docx" or "My table.csv", depending on whether you'd prefer to have
#'   the main PK table saved as a Word or csv file. If you supply only the file
#'   extension, e.g., \code{save_table = "docx"}, the name of the file will be
#'   the file name plus "PK summary table" with that extension and output will
#'   be located in the same folder as \code{sim_data_file}. If you supply
#'   something other than just "docx" or just "csv" for the file name but you
#'   leave off the file extension, we'll assume you want it to be ".csv". While
#'   the main PK table data will be in whatever file format you requested, if
#'   you set \code{checkDataSource = TRUE}, the QC data will be in a csv file on
#'   its own and will have "- QC" added to the end of the file name.
#'   \strong{WARNING:} SAVING TO WORD DOES NOT WORK ON SHAREPOINT. This is a
#'   Microsoft permissions issue, not an R issue. If you try to save on
#'   SharePoint, you will get a warning that R will save your file to your
#'   Documents folder instead.
#' @param fontsize the numeric font size for Word output. Default is 11 point.
#'   This only applies when you save the table as a Word file.
#'
#' @return A list or a data.frame of PK data that optionally includes where the
#'   data came from
#' @export
#'
#' @examples
#' # No examples yet.
#' 
calc_PK_ratios <- function(sim_data_file_numerator,
                           sim_data_file_denominator, 
                           paired = TRUE,
                           compoundToExtract = "substrate",
                           tissue = "plasma",
                           PKparameters = "AUC tab", 
                           sheet_PKparameters_num = NA,
                           sheet_PKparameters_denom = NA,
                           existing_exp_details = NA,
                           mean_type = "geometric", 
                           include_num_denom_columns = TRUE, 
                           conf_int = 0.9, 
                           includeCV = TRUE, 
                           includeConfInt = TRUE,
                           prettify_columns = TRUE,
                           prettify_compound_names = TRUE,
                           rounding = NA,
                           checkDataSource = TRUE, 
                           returnExpDetails = FALSE,
                           save_table = NA, 
                           fontsize = 11){
    
    # Error catching ----------------------------------------------------------
    # Check whether tidyverse is loaded
    if("package:tidyverse" %in% search() == FALSE){
        stop("The SimcypConsultancy R package also requires the package tidyverse to be loaded, and it doesn't appear to be loaded yet. Please run `library(tidyverse)` and then try again.")
    }
    
    # Check for appropriate input for arguments
    tissue <- tolower(tissue)
    if(tissue %in% c("plasma", "blood") == FALSE){
        warning("You have not supplied a permissible value for tissue. Options are `plasma` or `blood`. The PK parameters will be for plasma.", 
                call. = FALSE)
        tissue <- "plasma"
    }
    
    # Only returning geometric means and CI's if they want unpaired data.
    # Uncertain how to set things up otherwise.
    if(paired == FALSE & mean_type != "geometric"){
        warning("You have supplied unpaired data and requested something other than geometric means and confidence intervals. We have only set this function up for unpaired data with geometric means and confidence intervals, so that is what will be returned.", 
                call = FALSE)
        mean_type <- "geometric"
    }
    
    
    # Main body of function -------------------------------------------------
    
    ## Extracting PK ---------------------------------------------------------
    
    # Checking whether the user wants to have different PK parameters for
    # numerator than for denominator.
    if(any(str_detect(PKparameters, "/"))){
        Comparisons <- as.data.frame(str_split_fixed(
            PKparameters, pattern = "( )?/( )?", n = 2)) %>% 
            rename(PKparam_num = V1, PKparam_denom = V2) %>% 
            mutate(PKparam_denom = ifelse(PKparam_denom == "", PKparam_num, PKparam_denom), 
                   OrigPK = PKparameters)
        
    } else {
        Comparisons <- data.frame(PKparam_num = PKparameters, 
                                  PKparam_denom = PKparameters)
    }
    
    # Checking experimental details to only pull details that apply
    if("logical" %in% class(existing_exp_details)){ # logical when user has supplied NA
        Deets <- extractExpDetails_mult(sim_data_file = c(sim_data_file_numerator, 
                                                          sim_data_file_denominator), 
                                        exp_details = "Summary tab", 
                                        annotate_output = FALSE)
    } else {
        Deets <- switch(as.character("File" %in% names(existing_exp_details)), 
                        "TRUE" = existing_exp_details, 
                        "FALSE" = deannotateDetails(existing_exp_details))
        
        if("data.frame" %in% class(Deets)){
            Deets <- Deets %>% filter(File %in% c(sim_data_file_numerator, 
                                                  sim_data_file_denominator))
            
            if(nrow(Deets != 2)){
                Deets <- extractExpDetails_mult(sim_data_file = c(sim_data_file_numerator, 
                                                                  sim_data_file_denominator), 
                                                exp_details = "Summary tab", 
                                                annotate_output = FALSE)
            }
        }
    }
    
    suppressWarnings(
        PKnumerator <- extractPK(sim_data_file = sim_data_file_numerator,
                                 compoundToExtract = compoundToExtract, 
                                 tissue = tissue,
                                 PKparameters = Comparisons$PKparam_num, 
                                 sheet = sheet_PKparameters_num,
                                 existing_exp_details = Deets,
                                 returnAggregateOrIndiv = "both",
                                 returnExpDetails = FALSE) 
    )
    
    suppressWarnings(
        PKdenominator <- extractPK(sim_data_file = sim_data_file_denominator, 
                                   compoundToExtract = compoundToExtract, 
                                   tissue = tissue,
                                   PKparameters = Comparisons$PKparam_denom, 
                                   sheet = sheet_PKparameters_denom,
                                   existing_exp_details = Deets,
                                   returnExpDetails = FALSE,
                                   returnAggregateOrIndiv = "both")
    )
    
    # RETURN TO THIS LATER. We could add code to check and compare them for file
    # 2 to make absolutely sure they match when they should. Future plan... For
    # now, using the numerator Deets as the default.
    Deets_denom <- Deets %>% filter(File == sim_data_file_denominator)
    Deets <- Deets %>% filter(File == sim_data_file_numerator)
    
    # If user specified the sheet, then they're likely to get both AUCt and
    # AUCtau as well as CLt and CLtau b/c extractPK doesn't know what dose
    # number this is for b/c that's not included in the Excel output. When that
    # happens, we don't want to have BOTH show up in the results b/c they'll be
    # duplicates. Remove the AUCt and CLt columns from both sets of PK results
    # and just keep AUCtau and CLtau. User will be able to figure out what it
    # should be called and change that column name if they want.
    if(all(c("AUCt", "AUCtau") %in% names(PKnumerator$individual))){
        PKnumerator$individual <- PKnumerator$individual %>% select(-AUCt, -CLt)
        PKdenominator$individual <- PKdenominator$individual %>% select(-AUCt, -CLt)
    }
    
    
    ## Determining column format & arrangement ---------------------------------
    
    # Keeping track of which columns came from where and the desired order
    # in the output
    ColNames <- data.frame(ValType = c(rep("NumeratorSim", ncol(PKnumerator$individual)), 
                                       rep("DenominatorSim", ncol(PKdenominator$individual))), 
                           OrigName = c(names(PKnumerator$individual), 
                                        names(PKdenominator$individual)), 
                           Parameter = c(names(PKnumerator$individual),
                                         names(PKdenominator$individual))) %>% 
        filter(!Parameter %in% c("Individual", "Trial")) %>% 
        arrange(ValType, OrigName) %>% 
        mutate(OrigName_ValType = paste(OrigName, ValType))
    
    # Checking that we can make the correct comparisons
    if(all(Comparisons$PKparam_num == Comparisons$PKparam_denom)){
        
        if(any(Comparisons$PKparam_num %in% c("AUC tab", "all", "Absorption tab", 
                                              "Regional ADAM") == TRUE |
               any(Comparisons$PKparam_denom %in% c("AUC tab", "all", "Absorption tab", 
                                                    "Regional ADAM") == TRUE))){
            Comparisons <- data.frame(PKparam_num = intersect(names(PKnumerator$aggregate), 
                                                              names(PKdenominator$aggregate))) %>% 
                unique() %>% 
                mutate(PKparam_denom = PKparam_num) %>% 
                filter(PKparam_num != "Statistic")
        }
        
        # Removing any parameters that are not matched in the other simulation.
        # NB: Using the names of the aggregated data rather than the individual
        # b/c aggregated data will NOT include, e.g., AUCinf, if there was any
        # trouble extrapolating to infinity.
        
        if(all(names(PKnumerator$aggregate) %in% names(PKdenominator$aggregate)) == FALSE){
            warning(
                paste0("The parameters ", 
                       str_comma(paste0("`", setdiff(names(PKnumerator$aggregate),
                                                     names(PKdenominator$aggregate)))), 
                       " were available in the numerator but not in the denominator simulation and thus will not be included in your output."),
                call. = FALSE)
            
            Comparisons <- Comparisons %>% 
                filter(PKparam_num %in% names(PKdenominator$aggregate))
        }
        
        if(all(names(PKdenominator$individual) %in% names(PKnumerator$individual)) == FALSE){
            warning(
                paste0("The parameters ", 
                       str_comma(paste0("`", setdiff(names(PKdenominator$individual),
                                                     names(PKnumerator$individual)), 
                                        "`")), 
                       " were available in the denominator but not in the numerator simulation and thus cannot be included in your output."),
                call. = FALSE)
            
            Comparisons <- Comparisons %>% 
                filter(PKparam_denom %in% names(PKnumerator$aggregate))
        }
        
    } else {
        # This is when user wanted different parameters for numerator and
        # denominator files. Making sure that we have a matched set. 
        Comparisons <- Comparisons %>% 
            mutate(NumCheck = PKparam_num %in% names(PKnumerator$aggregate), 
                   DenomCheck = PKparam_denom %in% names(PKdenominator$aggregate)) %>% 
            filter(NumCheck == TRUE & DenomCheck == TRUE) %>% 
            select(-NumCheck, -DenomCheck)
    }
    
    # !!!!!!!!!!!!!!! CHANGING COL NAMES FOR DENOMINATOR !!!!!!!!!!!!!!!!!!!!!!
    # Because we need to match parameters when joining, renaming PK parameters
    # in PKdenominator to match the names in PKnumerator even though they're not
    # *really* the same PK parameters necessarily. We'll deal with that
    # difference later.
    PKdenominator$individual <- 
        PKdenominator$individual[, c("Individual", "Trial", Comparisons$PKparam_denom)]
    names(PKdenominator$individual) <- c("Individual", "Trial", Comparisons$PKparam_num)
    
    PKdenominator$aggregate <-
        PKdenominator$aggregate[, c("Statistic", Comparisons$PKparam_denom)]
    names(PKdenominator$aggregate) <- c("Statistic", Comparisons$PKparam_num)
    
    Comparisons$PKparam_denomREVISED <- Comparisons$PKparam_num
    
    # # RETURN TO THIS. Checking conc units
    # TEMP <- match_units(
    #     PKnumerator$aggregate %>% 
    #         rename(Conc = i) %>% 
    #         mutate(CompoundID = compoundToExtract, 
    #                Conc_units = Deets$Units_Cmax, 
    #                Time = 1, Time_units = "hours"),
    #     goodunits = list("Conc_units" = adjust_conc_units, 
    #                      "Time_units" = "hours"), 
    #     MW = c(compoundToExtract = 
    #                switch(compoundToExtract, 
    #                       "substrate" = Deets$MW_sub, 
    #                       "primary metabolite 1" = Deets$MW_met1, 
    #                       "primary metabolite 2" = Deets$MW_met2, 
    #                       "secondary metabolite" = Deets$MW_secmet, 
    #                       "inhibitor 1" = Deets$MW_inhib, 
    #                       "inhibitor 2" = Deets$MW_inhib2, 
    #                       "inhibitor 1 metabolite" = Deets$MW_inhib1met)))
    # MyPKResults_all$aggregate[, i] <- TEMP$Conc
    # rm(TEMP)
    
    # Setting the rounding option
    round_opt <- function(x, round_fun){
        
        round_fun <- ifelse(is.na(round_fun), "consultancy", tolower(round_fun))
        round_fun <- ifelse(str_detect(tolower(round_fun), "word"), "none", round_fun)
        
        suppressWarnings(
            NumDig <- as.numeric(str_trim(sub("signif(icant)?|round", "", round_fun)))
        )
        
        if(str_detect(round_fun, "signif|round") & 
           !str_detect(round_fun, "[0-9]{1,}")){
            warning("You appear to want some rounding, but we're not sure how many digits. We'll use 3 for now, but please check the help file for appropriate input for the argument `rounding`.", 
                    call. = FALSE)
            NumDig <- 3
        }
        
        round_fun <- str_trim(sub("[0-9]{1,}", "", round_fun))
        round_fun <- ifelse(str_detect(round_fun, "signif"), "signif", round_fun)
        
        Out <- switch(round_fun, 
                      "round" = round(x, digits = NumDig),
                      "signif" = signif(x, digits = NumDig), 
                      "consultancy" = round_consultancy(x), 
                      "none" = x)
        
        return(Out)
    }
    
    
    
    if(paired){
        ## Making paired comparisons -----------------------------------------
        
        MyPKResults <- PKnumerator$individual %>% 
            pivot_longer(cols = -c(Individual, Trial), 
                         names_to = "Parameter", 
                         values_to = "NumeratorSim") %>% 
            full_join(PKdenominator$individual %>% 
                          pivot_longer(cols = -c(Individual, Trial), 
                                       names_to = "Parameter", 
                                       values_to = "DenominatorSim"), 
                      join_by(Individual, Trial, Parameter)) %>% 
            mutate(Ratio = NumeratorSim / DenominatorSim, 
                   ID = paste(Individual, Trial))
        
        # Making sure that subjects were matched between numerator and
        # denominator
        NumIDs <- paste(PKumerator$individual$Individual, 
                        PKumerator$individual$Trial)
        DenomIDs <- paste(PKdenominator$individual$Individual, 
                          PKdenominator$individual$Trial)
        
        if(all(NumIDs %in% MyPKResults$ID) & all(DenomIDs %in% MyPKResults$ID) == FALSE){
            warning("You do not appear to have perfectly matched subjects in your numerator and denominator simulations. Since you have requested calculations for a paired study design, we'll only use the subjects that are matched in both for making calculations. If you actually have an unpaired study design, please change `paired` to `FALSE` and try again.", 
                    call. = FALSE)
            
            MyPKResults <- MyPKResults %>% filter(ID %in% intersect(NumIDs, DenomIDs))
        }
        
        if(include_num_denom_columns == FALSE){
            MyPKResults <- MyPKResults %>% select(-NumeratorSim, -DenominatorSim)
        } 
        
        MyPKResults <- MyPKResults %>% 
            pivot_longer(cols = switch(as.character(include_num_denom_columns), 
                                       "TRUE" = c("NumeratorSim", "DenominatorSim", "Ratio"), 
                                       "FALSE" = "Ratio"),
                         names_to = "ValType", 
                         values_to = "Value") %>% 
            left_join(data.frame(ValType = "DenominatorSim", 
                                 Parameter = Comparisons$PKparam_denomREVISED,
                                 CorrectName = Comparisons$PKparam_denom), 
                      by = join_by(Parameter, ValType)) %>% 
            mutate(CorrectName = ifelse(is.na(CorrectName), Parameter, CorrectName)) %>% 
            select(-Parameter) %>% rename(Parameter = CorrectName) %>% 
            group_by(Parameter, ValType) %>% 
            summarize(Mean = switch(
                mean_type, 
                "geometric" = round_opt(gm_mean(Value), rounding), 
                "arithmetic" = round_opt(mean(Value, na.rm = T), rounding)), 
                
                CV = switch(
                    mean_type, 
                    "geometric" = round_opt(100*gm_CV(Value), rounding),
                    "arithmetic" = round_opt(100*sd(Value, na.rm = T) /
                                                 mean(Value, na.rm = T), rounding)),
                
                CI_l = switch(
                    mean_type, 
                    "geometric" = round_opt(gm_conf(Value, CI = conf_int)[1], rounding),
                    "arithmetic" = round_opt(confInt(Value, CI = conf_int)[1], rounding)), 
                
                CI_u = switch(
                    mean_type, 
                    "geometric" = round_opt(gm_conf(Value, CI = conf_int)[2], rounding),
                    "arithmetic" = round_opt(confInt(Value, CI = conf_int)[2], rounding))) %>%
            
            pivot_longer(cols = -c("Parameter", "ValType"), 
                         names_to = "Statistic", 
                         values_to = "Value") %>% 
            left_join(ColNames %>% select(ValType, OrigName, Parameter), 
                      by = join_by(Parameter, ValType)) %>% 
            mutate(Parameter = case_when(is.na(OrigName) ~ Parameter, 
                                         complete.cases(OrigName) ~ OrigName),
                   Parameter = paste(Parameter, ValType)) %>% 
            select(-ValType, -OrigName) %>% 
            pivot_wider(names_from = Parameter, values_from = Value)
        
        
    } else {
        
        ## Making unpaired comparisons -----------------------------------------
        
        # Using calculations recommended by Frederic Bois for the confidence
        # interval. (Note to self: See email from March 3, 2023. -LSh)
        geomratio_stats <- function(x_num, x_denom){
            
            # Log transforming individual data
            logx_num <- log(x_num)
            logx_denom <- log(x_denom)
            
            # Calculating the difference of the means of the log-transformed
            # data; this is equivalent to the ratio of the geometric means,
            # i.e., gm_mean(x1) / gm_mean(x2) = exp(mean(logx1) - mean(logx2))
            LogGeomeanRatio <- mean(logx_num) - mean(logx_denom)
            
            # Variance of each vector
            Var_num <- var(logx_num)/length(x_num)
            Var_denom <- var(logx_denom)/length(x_denom)
            
            # Using that to calculate the variance of the ratio and then the
            # standard deviation
            Var_delta <- sum(Var_num, Var_denom)
            SD_delta <- sqrt(Var_delta)
            
            CI_lower_delta <- LogGeomeanRatio - qnorm(1-(1-conf_int)/2)*SD_delta
            CI_upper_delta <- LogGeomeanRatio + qnorm(1-(1-conf_int)/2)*SD_delta
            
            Out <- c("GeomeanRatio" = exp(LogGeomeanRatio), 
                     "GeoRatio_CI_lower" = exp(CI_lower_delta),
                     "GeoRatio_CI_upper" = exp(CI_upper_delta))
            
            return(Out)
            
        }
        
        PKRatios <- PKnumerator$individual %>% 
            pivot_longer(cols = -c(Individual, Trial), 
                         names_to = "PKparameter", values_to = "Value") %>% 
            mutate(NumDenom = "Num") %>% 
            bind_rows(PKdenominator$individual %>% 
                          pivot_longer(cols = -c(Individual, Trial), 
                                       names_to = "PKparameter", values_to = "Value") %>% 
                          mutate(NumDenom = "Denom")) %>% 
            group_by(PKparameter) %>% 
            summarize(Mean = geomratio_stats(x_num = Value[NumDenom == "Num"], 
                                                  x_denom = Value[NumDenom == "Denom"])[1], 
                      CI_l = geomratio_stats(x_num = Value[NumDenom == "Num"], 
                                             x_denom = Value[NumDenom == "Denom"])[2], 
                      CI_u = geomratio_stats(x_num = Value[NumDenom == "Num"], 
                                             x_denom = Value[NumDenom == "Denom"])[3])
        
        # Getting this into a form that matches form from paired option.
        MyPKResults <- PKRatios %>% 
            # select(PKparameter, matches("Ratio")) %>% 
            pivot_longer(cols = -PKparameter, 
                         names_to = "Statistic", values_to = "Value") %>% 
            mutate(Value = round_opt(Value, rounding), 
                   ValType = "Ratio", 
                   PKparameter = paste(PKparameter, ValType))
        
        if(include_num_denom_columns){
            
            suppressMessages(
                MyPKResults <- MyPKResults %>% 
                    bind_rows(
                        PKnumerator$aggregate %>%
                            pivot_longer(cols = -Statistic, 
                                         names_to = "PKparameter", values_to = "Value") %>% 
                            mutate(ValType = "NumeratorSim", 
                                   Value = round_opt(Value, rounding)) %>% 
                            
                            bind_rows(
                                # Earlier, had to change column names of denominator results for
                                # matching w/numerator results when joining data.frames. Now, need
                                # to change names back to what the values *actually are* so that
                                # things don't get any further confused.
                                PKdenominator$aggregate %>%
                                    pivot_longer(cols = -Statistic, 
                                                 names_to = "PKparam_num", values_to = "Value") %>% 
                                    left_join(Comparisons %>% select(PKparam_num, PKparam_denom), 
                                              by = "PKparam_num") %>% 
                                    rename(PKparameter = PKparam_denom) %>% select(-PKparam_num) %>% 
                                    mutate(ValType = "DenominatorSim", 
                                           Value = round_opt(Value, rounding))) %>% 
                            
                            filter(Statistic %in% 
                                       switch(mean_type,
                                              "geometric" = c("Geometric Mean", "Geometric CV", 
                                                              "90% confidence interval around the geometric mean(lower limit)", 
                                                              "90% confidence interval around the geometric mean(upper limit)"),
                                              "arithmetic" = c("Mean", "cv"))) %>% 
                            mutate(PKparameter = paste(PKparameter, ValType), 
                                   Statistic = sub("Geometric ", "", Statistic), 
                                   Statistic = recode(Statistic, 
                                                      "90% confidence interval around the geometric mean(lower limit)" = "CI_l", 
                                                      "90% confidence interval around the geometric mean(upper limit)" = "CI_u"))) %>% 
                            select(-ValType) %>% 
                            pivot_wider(names_from = PKparameter,
                                        values_from = Value)
                    )
        }
    }
    
    # Filtering ColNames for columns that are still present and then using
    # ColNames to set factor order
    ColNames <- ColNames %>%
        filter((ValType == "DenominatorSim" &
                    Parameter %in% switch(
                        as.character(all(Comparisons$PKparam_num == 
                                             Comparisons$PKparam_denom)), 
                        "TRUE" = Comparisons$PKparam_denomREVISED, 
                        "FALSE" = Comparisons$PKparam_denom)) |
                   (ValType == "NumeratorSim" &
                        Parameter %in% Comparisons$PKparam_num))
    ColOrder <- unique(c(ColNames$OrigName_ValType,
                         paste(unique(ColNames$Parameter[
                             ColNames$ValType == "NumeratorSim"]), "Ratio")))
    
    MyPKResults <- MyPKResults[, c("Statistic", ColOrder)]
    
    # At this point, MyPKResults has columns for Statistic and then one
    # column for each combination of Parameter and ValType, and the order
    # left-to-right is 1) all PK parameters for the denominator (control)
    # simulation, in alphabetical order, 2) all PK parameters for the
    # numerator (comparison) simulation, in alphabetical order, 3) all
    # ratios of numerator / denominator PK in alphabetical order. If user
    # only wanted ratios, then the only ValType is "Ratio". Otherwise,
    # ValType is "Ratio", "DenominatorSim", and "NumeratorSim".
    
    
    # Tidying up names of columns, etc. ------------------------------------
    StatNames <- c(
        "Mean" = "Simulated", 
        "CV" = "CV",
        "CI_l" = "90% CI - Lower", 
        "CI_u" = "90% CI - Upper")
    names(StatNames) <- sub("90", round(conf_int*100), names(StatNames))
    MyPKResults$Statistic <- StatNames[MyPKResults$Statistic]
    
    # Only including the variability measurements user requested
    if(includeCV == FALSE){
        MyPKResults <- MyPKResults %>% 
            filter(!Statistic %in% c("CV"))
    }
    
    if(includeConfInt == FALSE){
        MyPKResults <- MyPKResults %>% 
            filter(!Statistic %in% c("90% CI - Lower", "90% CI - Upper"))
    }
    
    if(any(str_detect(PKparameters, "/"))){
        # If the PK parameters for the numerator are different from the PK
        # parameters in the denominator, have to do some stuff to get the
        # columns in the correct order and also looking pretty.
        suppressMessages(
            NewColNames1 <- data.frame(OrigName = ColOrder[str_detect(ColOrder, "Ratio")]) %>% 
                left_join(Comparisons %>% mutate(OrigName = paste(Comparisons$PKparam_num, "Ratio"))) %>% 
                mutate(GoodColName = OrigPK)
        )
        NewColNames2 <- data.frame(OrigName = ColOrder[!str_detect(ColOrder, "Ratio")]) %>% 
            mutate(GoodColName = OrigName)
        NewColNames <- NewColNames1 %>% bind_rows(NewColNames2) %>% 
            select(OrigName, GoodColName) %>% 
            mutate(OrigName = factor(OrigName, levels = ColOrder)) %>% 
            arrange(OrigName)
        
        MyPKResults <- MyPKResults[, c("Statistic", as.character(NewColNames$OrigName))]
        names(MyPKResults) <- c("Statistic", NewColNames$GoodColName)
    } 
    
    if(prettify_columns){
        PrettyCol <- data.frame(OrigName = names(MyPKResults)[
            !names(MyPKResults) == "Statistic"])
        PrettyCol$PKparameter <- 
            sapply(PrettyCol$OrigName, 
                   FUN = function(x) str_split_fixed(x, " ", n = 2)[1])
        suppressMessages(
            PrettyCol <- PrettyCol %>% 
                left_join(
                    AllPKParameters %>% 
                        mutate(PKparameter = 
                                   switch(any(str_detect(names(MyPKResults), "_dose1|_last")),
                                          "TRUE" = PKparameter, 
                                          "FALSE" = sub("_dose1|_last", "", PKparameter)), 
                               PrettifiedNames = 
                                   switch(any(str_detect(names(MyPKResults), "_dose1|_last")),
                                          "TRUE" = PrettifiedNames,
                                          "FALSE" = sub("Dose 1 |Last dose ", "", PrettifiedNames))) %>% 
                        select(PKparameter, PrettifiedNames)) %>% 
                unique()
        )
        
        PrettyCol <- PrettyCol %>% 
            mutate(GoodCol =
                       unlist(purrr::pmap(list(x = PKparameter, 
                                               y = PrettifiedNames, 
                                               z = OrigName), 
                                          ~ gsub(..1, ..2, ..3))))
        
        if(any(duplicated(PrettyCol$GoodCol))){
            # This happens when CLt and CLinf are included.
            PrettyCol <- PrettyCol %>% 
                mutate(GoodCol = 
                           ifelse(str_detect(PKparameter, "CLt"),
                                  sub("h\\)",
                                      "h, calculated using interval to time t)",
                                      GoodCol), 
                                  GoodCol))
        }
        
        PrettyCol <- PrettyCol %>% pull(GoodCol)
        
        # Adjusting units as needed.
        PrettyCol <- sub("\\(ng/mL.h\\)", paste0("(", Deets$Units_AUC, ")"), PrettyCol)
        PrettyCol <- sub("\\(L/h\\)", paste0("(", Deets$Units_CL, ")"), PrettyCol)
        PrettyCol <- sub("\\(ng/mL\\)", paste0("(", Deets$Units_Cmax, ")"), PrettyCol)
        PrettyCol <- sub("\\(h\\)", paste0("(", Deets$Units_tmax, ")"), PrettyCol)
        
        # Just making absolutely sure that the order of columns matches
        MyPKResults <- MyPKResults[, c("Statistic", names(MyPKResults)[
            !names(MyPKResults) == "Statistic"])]
        
        # Setting prettified names.
        names(MyPKResults) <- c("Statistic", PrettyCol)
        names(MyPKResults) <- sub("DenominatorSim", "denominator", names(MyPKResults))
        names(MyPKResults) <- sub("NumeratorSim", "numerator", names(MyPKResults))
    }
    
    # Checking on possible effectors to prettify
    MyEffector <- c(Deets$Inhibitor1, Deets$Inhibitor1Metabolite, 
                    Deets$Inhibitor2)
    if(length(MyEffector) > 0){
        MyEffector <- str_comma(MyEffector[complete.cases(MyEffector)])
        MyEffector <- ifelse(MyEffector == "", NA, MyEffector)
    } else {
        MyEffector <- NA
    }
    
    if(any(complete.cases(MyEffector))){
        
        if(class(prettify_compound_names) == "logical" &&
           prettify_compound_names){
            MyEffector <- prettify_compound_name(MyEffector)
        }
        
        if(class(prettify_compound_names) == "character"){
            names(prettify_compound_names)[
                str_detect(tolower(names(prettify_compound_names)), 
                           "inhibitor")][1] <- "inhibitor"
            MyEffector <- prettify_compound_names["inhibitor"]
        }
        
        # Prettifying effector names as necessary
        names(MyPKResults) <- sub("effector", MyEffector, names(MyPKResults))
    }
    
    # Saving --------------------------------------------------------------
    MyPKResults_out <- MyPKResults
    
    if(complete.cases(save_table)){
        
        # Rounding as necessary
        if(complete.cases(rounding) && rounding == "Word only"){
            MyPKResults <- MyPKResults %>% 
                mutate(across(.cols = where(is.numeric), 
                              .fns = round_opt, round_fun = "Consultancy"))
            
        } 
        
        # Checking whether they have specified just "docx" or just "csv" for
        # output b/c then, we'll use sim_data_file as file name. This allows us
        # to determine what the path should be, too, for either sim_data_file or
        # for some specified file name.
        if(str_detect(sub("\\.", "", save_table), "^docx$|^csv$")){
            OutPath <- dirname(sim_data_file)
            save_table <- sub("xlsx", 
                              # If they included "." at the beginning of the
                              # file extension, need to remove that here.
                              sub("\\.", "", save_table),
                              basename(sim_data_file))
        } else {
            # If they supplied something other than just "docx" or just "csv",
            # then check whether that file name is formatted appropriately.
            
            if(str_detect(basename(save_table), "\\..*")){
                if(str_detect(basename(save_table), "\\.docx") == FALSE){
                    # If they specified a file extension that wasn't docx, make that
                    # file extension be .csv
                    save_table <- sub("\\..*", ".csv", save_table)
                }
            } else {
                # If they didn't specify a file extension at all, make it .csv. 
                save_table <- paste0(save_table, ".csv")
            }
            
            # Now that the file should have an appropriate extension, check what
            # the path and basename should be.
            OutPath <- dirname(save_table)
            save_table <- basename(save_table)
        }
        
        if(str_detect(save_table, "docx")){ 
            # This is when they want a Word file as output
            
            # May need to change the working directory temporarily, so
            # determining what it is now
            CurrDir <- getwd()
            
            OutPath <- dirname(save_table)
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
            
            FileName <- basename(save_table)
            
            # Storing some objects so they'll work with the markdown file
            PKToPull <- PKparameters
            MeanType <- mean_type
            sim_data_file <- str_comma(c(basename(sim_data_file_numerator),
                                         basename(sim_data_file_denominator)))
            FromCalcPKRatios <- TRUE
            
            rmarkdown::render(system.file("rmarkdown/templates/pk-summary-table/skeleton/skeleton.Rmd",
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
            # This is when they want a .csv file as output. In this scenario,
            # changing the value "simulated" in the list of stats to include
            # whether it was arithmetic or geometric b/c that info is included
            # in the Word file but not in the table itself.
            MyPKResults <- MyPKResults %>% 
                mutate(Statistic = sub("Simulated", 
                                       paste("Simulated", MeanType, "mean"), Statistic))
            write.csv(MyPKResults, paste0(OutPath, "/", save_table), row.names = F)
        }
    }
    
    Out <- list("Table" = MyPKResults_out)
    
    if(checkDataSource){
        Out[["QC"]] <- bind_rows(PKnumerator$QC, PKdenominator$QC)
        
        if(complete.cases(save_table)){ 
            write.csv(Out[["QC"]], sub(".csv|.docx", " - QC.csv", save_table), row.names = F)
        }
        
    }
    
    if(returnExpDetails){
        Out[["ExpDetails_num"]] <- Deets
        Out[["ExpDetails_denom"]] <- Deets_denom
    }
    
    if(length(Out) == 1){
        return(Out[[1]])
    } else {
        return(Out)
    }
    
}

