# TODO:   Delete taxon concepts in Postgres
# 
# Author: Miguel Alvarez
################################################################################

pg_delete_concept <- function(conn, schema, concept_id) {
	query <- paste0("
select *
from ", paste0(schema, ".", "names2concepts", collapse=""), "
where \"TaxonConceptID\" in (", paste(concept_id, collapse=","),");
")
	names2concepts <- dbGetQuery(conn, query)
	response <- askYesNo(paste("Do you really like to delete",
					length(unique(concept_id)), "concept(s)?"))
	if(is.na(response))
		stop("Deletion cancelled.", call.=FALSE)
	if(response) {
		# Delete entries in names2concepts
		query <- paste0("
delete from ", paste0(schema, ".", "names2concepts", collapse=""),"
where \"TaxonConceptID\" in (", paste(concept_id, collapse=","),");
")
		dbSendQuery(conn, query)
		# Delete names
		query <- paste0("
delete from ", paste0(schema, ".", "\"taxonNames\"", collapse=""),"
where \"TaxonUsageID\" in (", paste(names2concepts$TaxonUsageID, collapse=","), ");
")
		dbSendQuery(conn, query)
		# Delete concept
		query <- paste0("
delete from ", paste0(schema, ".", "\"taxonRelations\"", collapse=""), "
where \"TaxonConceptID\" in (", paste(concept_id, collapse=","),");
")
		dbSendQuery(conn, query)
	}
	message("DONE")
}
