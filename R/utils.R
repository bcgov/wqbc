punctuate_strings <- function (x, qualifier = "or") {
  if(length(x) == 1)
    return (x)
  n <- length(x)
  paste(paste(x[-n], collapse = ", "), qualifier, x[n])
}

remove_columns_from_x_in_y <- function (x, y) {

  colnames <- colnames(x)[colnames(x) %in% colnames(y)]
  if(length(colnames) >= 1) {
    message("Removed columns ", punctuate_strings(colnames, "and"), " from x")
  }
  x[,!colnames(x) %in% colnames(y), drop = FALSE]
}

## Assign and transform the coordinate system/projection to match the base BC map
proj_bc <- function (data, x, y, input_proj = NULL) {

  if(!requireNamespace("sp", quietly = TRUE))
    stop("sp package not installed")

  if(!requireNamespace("rgdal", quietly = TRUE))
    stop("rgdal package not installed")

  if (is.null(input_proj)) {
    input_proj <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
  }
  output_proj <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

  sp::coordinates(data) <- c(x,y)
  sp::proj4string(data) <- sp::CRS(input_proj)
  data <- sp::spTransform(data, sp::CRS(output_proj))
  as.data.frame(data)
}
