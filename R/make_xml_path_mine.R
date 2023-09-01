#' Make the user name in the path of observed data overlay XML files and fixed
#' trial design XML files match that of the current user
#'
#' Have you ever wanted to run a bunch of simulations that someone else set up
#' but had to open each individual workspace just to change the path for the
#' observed data? \code{make_xml_path_mine} changes the path of any XML files in
#' simulation workspaces -- observed data overlay files and fixed trial design
#' files -- to whatever path would work for the current user to then run those
#' files.
#'
#' @param sim_workspace_files the set of workspace files to modify; must end in
#'   ".wksz" if you're specifying individual files. Leave as NA to change all
#'   the workspaces in the current folder or set to "recursive" to change all
#'   the workspaces in the current folder and \emph{all} the subfolders below
#'   it. BE CAREFUL. This function changes workspaces, so please be certain
#'   you're making changes you definitely want. 
#'
#' @return saves workspace files 
#' @export
#'
#' @examples
#' \dontrun{
#' make_xml_path_mine()
#' }
#' 
#' 

make_xml_path_mine <- function(sim_workspace_files = NA){
   
   change_xml_path(sim_workspace_files = NA,
                   new_sim_workspace_files = NA,
                   workspace_objects = NA,
                   new_xml_path = "only change user", 
                   save_workspaces = TRUE)
   
}

