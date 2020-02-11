#' @name pg_merge_concepts
#' 
#' @title Merge multiple concepts into one
#' 
#' @description 
#' Different taxon concepts may be merged by taxonomic revisions.
#' 
#' All concepts indicated in argument `concept_id` will be set as
#' synonyms of the first concept in the vector.
#' 
#' Take care of producing backups before starting the manipulation of
#' databases.
#' 
#' @param conn A database connection provided by [dbConnect()].
#' @param schema Name of the schema containing taxonomic tables.
#' @param concept_id ID of taxon concepts to be merged.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @export pg_merge_concepts
#' 
pg_merge_concepts <- function(conn, schema, concept_id) {
	if(length(concept_id) < 2)
		stop("Argument 'concept_id' have to be of length 2 or higher.")
	# change status of names
	query <- paste0("
update ", paste0(schema, ".", "names2concepts", collapse=""),"
set \"NameStatus\" = 'synonym'
where \"TaxonConceptID\" in (", paste(concept_id[-1], collapse=","),");
")
	dbSendQuery(conn, query)
	# change names to concept
	query <- paste0("
update ", paste0(schema, ".", "names2concepts", collapse=""), "
set \"TaxonConceptID\" = ", concept_id[1],"
where \"TaxonConceptID\" in (", paste(concept_id[-1], collapse=","), ");
")
	dbSendQuery(conn, query)
	# change entries as parent
	query <- paste0("
update ", paste0(schema, ".", "\"taxonRelations\"", collapse=""), "
set \"Parent\" = ", concept_id[1], "
where \"Parent\" in (", paste(concept_id[-1], collapse=","),");
")
	dbSendQuery(conn, query)
	# delete old concepts
	query <- paste0("
delete from ", paste0(schema, ".", "\"taxonRelations\"", collapse=""), "
where \"TaxonConceptID\" in (", paste(concept_id[-1], collapse=","),");
")
	dbSendQuery(conn, query)
	message("DONE")
}
