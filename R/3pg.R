#' Run the 3PG model and return model results
#'
#' Accepts climate data, species characteristics, and site configuration 
#'   information and returns model outputs
#'
#' @param config Either the path to a 3PG configuration file OR a list object
#'   with the appropriate structure (see examples/read function).
#' @param climate Either the path to a 3PG climate file OR a data.frame object
#'   with the appropriate structure (see examples/read function). If specified,
#'   these data take priori over an input specified in the config file. The data
#'   should be sorted in order from first month to final month. Using Tmin and 
#'   Tmax you can calculated VPD with get_VPD() and frost_days with frost_days().
#'   Required columns are Tav, VPD, Rain, Solar.rad, Frost.Days, Ca, D13Catm, 
#'   and d18O.
#' @param output Optional file path for output. If FALSE, then no output is 
#'   given. If either "config" or "" or NULL, output is written to file 
#'   according to the configuration file.
#' @param python_indexing If TRUE, month indices are done in python style for 
#'   compatibily. If FALSE then January is 1, not 0. Defaults to FALSE.   
#' @return A data.frame with model results.
#' @export
#' @examples
#'
#' @seealso \link[r3PG]{load_config}.
#'
#
############################################################################
#
# Information preserved from Python code:
#   Previous author of code variations: joeyzhou1984@gmail.com, Youngil Kim
#   LastModified: YIK update on 8/2/16: PC
############################################################################

run_3pg <- function(config, 
                    climate = NULL, 
                    output = FALSE, 
                    python_indexing = FALSE){

  # TODO add code to check the config file
  
  output_list <- list()
  output_list <- instance3PG(config, climate, output, python_indexing)
  
  # TODO potentially add code for running multiple times. For R users, likely 
  #  better to leave this out.

  return(data.frame(output_list))
}

