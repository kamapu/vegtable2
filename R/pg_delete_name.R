# TODO:   Delete synonyms in Postgres
# 
# Author: Miguel Alvarez
################################################################################

pg_delete_name <- function(conn, schema, usage_id) {
	query <- paste0("
select *
from ", paste0(schema, ".", "names2concepts", collapse=""), "
where \"TaxonUsageID\" in (", paste(usage_id, collapse=","),");
")
	names2concepts <- dbGetQuery(conn, query)
	if(any(names2concepts$NameStatus == "accepted"))
		stop("Attempting to delete accepted name. Use 'pg_delete_concept' instead.")
	response <- askYesNo(paste("Do you really like to delete",
					length(unique(usage_id)), "name(s)?"))
	if(is.na(response))
		stop("Deletion cancelled.", call.=FALSE)
	if(response) {
		# Delete entry in names2concepts
		query <- paste0("
delete from ", paste0(schema, ".", "names2concepts", collapse=""),"
where \"TaxonUsageID\" in (", paste(usage_id, collapse=","),");
")
		dbSendQuery(conn, query)
		# Delete name
		query <- paste0("
delete from ", paste0(schema, ".", "\"taxonNames\"", collapse=""), "
where \"TaxonUsageID\" in (", paste(usage_id, collapse=","),");
")
		dbSendQuery(conn, query)
	}
	message("DONE")
}
