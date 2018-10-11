# TODO:   Get length and precision for numeric values
# 
# Author: Miguel Alvarez
################################################################################

get_precision <- function(x) {
	LEN <- max(nchar(sub(".", "", paste(x), fixed=TRUE)))
	PRES <- max(nchar(paste(as.integer(x))))
	PRES <- LEN - PRES
	cat("length:", LEN, "\n")
	cat("precision:", PRES, "\n")
}
