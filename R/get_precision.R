# TODO:   Get length and precision for numeric values
# 
# Author: Miguel Alvarez
################################################################################

get_precision <- function(x) {
	LEN <- max(nchar(sub(".", "", paste(x), fixed=TRUE)))
	W <- paste(as.integer(x))
	W[W == "NA"] <- NA
	W <- max(nchar(W), na.rm=TRUE)
	PRES <- LEN - W
	cat("length:", LEN, "\n")
	cat("precision:", PRES, "\n")
}
