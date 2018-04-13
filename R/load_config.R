#' Load 3PG configuration files
#'
#' This function enables loading of configuration files into R outside of the 
#'   primary run_3PG() function so that it can be modified and explored prior
#'   to running the model.
#'
#' @param fpath_cfg String specifying the location of the configuration file.
#' @return A list with 3PG configuration variables
#' @export
#' @examples
#'
#' @seealso \link[r3PG]{run_3PG}.
#'

load_config <- function(fpath_cfg){
    res <- read.ini(fpath_cfg)
    IO <- res$IO
    res$IO <- NULL
    res <- rapply(object = res, 
                  f = as.numeric, 
                  how = "list")
    res$IO <- IO
    return(res)
}
