#' @title find the path to a specified command
#' @description helper function allowing to find the path to a given command on
#'  the system of the user. If the command is not found, a `stop` is issued and
#'  the program aborts (gracefully).
#' @param command `character` command to be search in the system (e.g., "gdalinfo")
#' @param stop_on_missing PARAM_DESCRIPTION, Default: TRUE
#' @return `character` normalized path to the desired command
#' @author Lorenzo Busetto <lbusett@gmail.com>
#' @examples \dontrun{
#'  # Find the path to `gdalinfo`
#'  gdalinfo_path <- find_command("gdalinfo")
#'  }
#' @export
#' @rdname find_command

find_command <- function(command,
                         stop_on_missing = TRUE) {
  #   __________________________________________________________________________
  #   find command on user machine                                          ####
  command_path <- Sys.which(command)
  if ((command_path == "")) {
    stop("find_command --> `", command, "` was not found on your system. ",
         "Please install it and/or check your PATH environment variables. ",
         "Aborting !")
  } else {
    return(normalizePath(command_path))
  }
}
