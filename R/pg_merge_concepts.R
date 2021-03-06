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
#' @param taxon_relations,taxon_traits,names2concepts Character vectors containing the name
#'     of the schema and for the respective information.
#' @param concept_id ID of taxon concepts to be merged.
#' @param ... Further arguments passed among methods.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @rdname pg_merge_concepts
#' 
#' @export pg_merge_concepts
#' @exportMethod pg_merge_concepts
#' 
pg_merge_concepts <- function (conn, ...) {
	UseMethod("pg_merge_concepts", conn)
}

#' @rdname pg_merge_concepts
#' 
#' @method pg_merge_concepts PostgreSQLConnection
#' @export 
#' 
pg_merge_concepts.PostgreSQLConnection <- function(conn, names2concepts,
		taxon_relations, taxon_traits, concept_id, ...) {
	if(length(concept_id) < 2)
		stop("Argument 'concept_id' have to be of length 2 or higher.")
	# change status of names
	Query <- paste0("UPDATE \"", paste0(names2concepts, collapse = "\".\""),
			"\"\n",
			"SET \"NameStatus\" = 'synonym'\n",
			"WHERE \"TaxonConceptID\" IN (",
			paste(concept_id[-1], collapse=","),");\n")
	dbSendQuery(conn, Query)
	# change names to concept
	Query <- paste0("UPDATE \"", paste0(names2concepts, collapse = "\".\""),
			"\"\n",
			"SET \"TaxonConceptID\" = ", concept_id[1], "\n",
			"WHERE \"TaxonConceptID\" IN (",
			paste(concept_id[-1], collapse=","), ");\n")
	dbSendQuery(conn, Query)
	# change entries as parent
	Query <- paste0("UPDATE \"", paste0(taxon_relations, collapse = "\".\""),
			"\"\n",
			"SET \"Parent\" = ", concept_id[1], "\n",
			"WHERE \"Parent\" IN (", paste(concept_id[-1], collapse=","),");\n")
	dbSendQuery(conn, Query)
	# delete from taxonTraits before deleting concept
	Query <- paste0("DELETE FROM \"",
			paste0(taxon_traits, collapse = "\".\""), "\"\n",
			"WHERE \"TaxonConceptID\" IN (",
			paste(concept_id[-1], collapse=","),");\n")
	dbSendQuery(conn, Query)
	# delete old concepts
	Query <- paste0("DELETE FROM \"",
			paste0(taxon_relations, collapse = "\".\""), "\"\n",
			"WHERE \"TaxonConceptID\" IN (",
			paste(concept_id[-1], collapse=","),");\n")
	dbSendQuery(conn, Query)
	message("DONE!")
}
