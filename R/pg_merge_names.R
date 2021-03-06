#' @name pg_merge_names
#' 
#' @title Merge names across data sets
#' 
#' @description 
#' When completing authors names in common names tables, duplicated entries may
#' emerge.
#' To merge incomplete entry with a complete one, the replacement have to be
#' done across data sets (vegetation-plot databases).
#' 
#' @param conn A database connection established by [dbConnect()].
#' @param new_id Integer value, the persisting name id ('TaxonUsageID').
#' @param old_id Integer value, the name id ('TaxonUsageID') to be replaced.
#' @param taxon_names Character vector indicating the name of schema and table
#'     containing taxon names.
#' @param schemas Character vector indicating the schemas containing the
#'     respective databases sharing taxon names.
#' @param ... Further arguments passed among methods.
#' 
#' @rdname pg_merge_names
#' 
#' @export pg_merge_names
#' @exportMethod pg_merge_names
#' 
pg_merge_names <- function (conn, ...) {
	UseMethod("pg_merge_names", conn)
}

#' @rdname pg_merge_names
#' 
#' @method pg_merge_names PostgreSQLConnection
#' @export 
#' 
pg_merge_names.PostgreSQLConnection <- function(conn, new_id, old_id,
		taxon_names = c("tax_commons", "taxonNames"),
		schemas=c("swea_dataveg","sudamerica"),
		...) {
	# Pre-check
	if(length(new_id) > 1)
		stop("Length > 1 is not allowed for argument 'new_id'")
	if(length(old_id) > 1)
		stop("Length > 1 is not allowed for argument 'old_id'")
	n2c <- list()
	for(i in 1:length(schemas)) {
		Query <- paste0("SELECT * FROM \"", schemas[i], "\".names2concepts;\n")
		n2c[[schemas[i]]] <- dbGetQuery(conn, Query)
	}
	n2c <- sapply(n2c, function(x, new, old)
				(new %in% x$TaxonUsageID) & (old %in% x$TaxonUsageID),
			new=new_id, old=old_id)
	if(any(n2c))
		stop(paste0("Replacement will violate double entries for names ",
						"in dataset(s): '", paste(names(n2c)[n2c],
								collapse="' '"), "'"))
	# 1: Changes at 'names2concepts'
	message("Updating 'names2concepts' tables...")
	for(i in 1:length(schemas)) {
		Query <- paste0("UPDATE \"", schemas[i], "\".names2concepts\n",
				"SET \"TaxonUsageID\" = ", new_id, "\n",
				"WHERE \"TaxonUsageID\" = ", old_id, ";\n")
		dbSendQuery(conn, Query)
		## cat(Query, "\n")
	}
	# 2: Changes at 'samples'
	message("Updating 'samples' tables...")
	for(i in 1:length(schemas)) {
		Query <- paste0("UPDATE \"", schemas[i], "\".samples\n",
				"SET \"TaxonUsageID\" = ", new_id, "\n",
				"WHERE \"TaxonUsageID\" = ", old_id, ";\n")
		dbSendQuery(conn, Query)
	}
	# 3: Deleting old name
	Query <- paste0(
			"DELETE FROM \"", paste(taxon_names, collapse="\".\""), "\"\n",
			"WHERE \"TaxonUsageID\" = ", old_id, ";\n")
	dbSendQuery(conn, Query)
	message("DONE!")
}
