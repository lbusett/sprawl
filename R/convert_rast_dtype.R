#' @title convert data type between `raster` and `gdal` conventions
#' @description FUNCTION_DESCRIPTION
#' @param dtype_string `character` data type string (e.g., Int16, etc.)
#' @param type `character` - either \"gdal\" or \"raster\"
#' @return `tibble` containing the representation of the data type both for gdal
#'  and raster
#' @examples
#' \dontrun{
#'  in_dtype <- "INT1U"
#'  convert_rastdtype(in_dtype, "raster")
#'
#'  in_dtype <- "Float64"
#'  convert_rastdtype(in_dtype, "gdal")
#'
#'  in_dtype <- "Float123"
#'  convert_rastdtype(in_dtype, "gdal")
#' }
#' @rdname convert_rastdtype
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom tibble tribble
convert_rastdtype <- function(dtype_string, type){

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
    lines <- sapply(dtype_string,
                    FUN = function(x){which(dtype_table$raster == x)},
                    simplify = "array", USE.NAMES = FALSE)
    if (length(line) != 0 ) {
      return(dtype_table[lines,])

    } else {
      stop("get_rast_dtype --> ", call[[2]], " is not a recognized ", call[[3]],
           " format.\nAborting !")
    }
  }

  if (type == "gdal") {
    line <- which(dtype_table$gdal == dtype_string)
    if (length(line) != 0 ){
      return(dtype_table[line,])
    } else {
      stop("\nget_rast_dtype --> ", call[[2]], " is not a recognized ", call[[3]], #nolint
           " format.\nAborting !")
    }
  }

  stop("get_rast_dtype --> ", call[[3]], " must be \"raster\" or \"gdal\".\n",
       "Aborting !")

}
