#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dtype_string PARAM_DESCRIPTION
#' @param type PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname convert_rastdtype
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom tibble tribble
convert_rastdtype <- function(dtype_string,
                           type){

  call        <- match.call()

  dtype_table <- tibble::tribble(
    ~gdal, ~raster,
    # "Byte-uns",   "INT1S",  # TODO: Implement checks in gdal routines
    "Byte"    ,   "INT1U" ,
    "UInt16"  ,   "INT2U" ,
    "Int16"   ,   "INT2S" ,
    "UInt32"  ,   "INT4U" ,
    "Int32"   ,   "INT4S" ,
    "Float32" ,   "FLT4S" ,
    "Float64" ,   "FLT8S" )
  #"CInt16"  ,   "1",
  # "CInt32"  ,   "1"
  # "CFloat32",   "1"
  # "CFloat64",   "1"
  # )
  if (type == "raster") {
    line <- which(dtype_table$raster == dtype_string)
    if (length(line) != 0 ){
      return(dtype_table[line,])

    } else {
      stop("get_rast_dtype --> ", call[[2]], "is not a recognized ", call[[3]], " format.
           Aborting !")
    }
  }

  if (type == "gdal") {
    line <- which(dtype_table$gdal == dtype_string)
    if (length(line) != 0 ){
      return(dtype_table[line,])
    } else {
      stop("get_rast_dtype --> ", call[[2]], "is not a recognized ", call[[3]], " format.
           Aborting !")
    }
  }

  stop("get_rast_dtype --> ", call[[3]], " must be \"raster\" or \"gdal\". Aborting !")

}
