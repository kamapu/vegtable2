#' @name accepted_name
#' 
#' @title Switch accepted names by synonyms in a taxon
#' 
#' @description 
#' If necessary to set a synonym in a taxon concept as an accepted name.
#' 
#' @param taxlist An object of class [PostgreSQLConnection-class].
#' @param ConceptID Integer containing concept IDs where to request or set names
#'     for one category.
#' @param accepted Integer indicating the usage name ID to be set as accepted
#'     name. If this usage name does not belong to the concept indicated by
#'     'ConceptID', an error message will be retrieved.
#' @param names2concepts A character vector with two values, the name of the
#'     schema and the name of the table relating names with taxon concepts.
#' @param ... Further arguments passed among methods.
#' 
#' @aliases accepted_name,PostgreSQLConnection,integer-method
#' 
#' @exportMethod accepted_name 
#' 
setMethod("accepted_name", signature(taxlist = "PostgreSQLConnection",
				ConceptID = "integer"),
		function(taxlist, ConceptID, accepted, names2concepts, ...) {
			# Get list of names and check
			Query <- paste0("SELECT \"TaxonUsageID\"\n",
					"FROM \"", paste0(names2concepts, collapse = "\".\""),
					"\"\n",
					"WHERE \"TaxonConceptID\" = ", ConceptID, ";\n")
			usages <- dbGetQuery(taxlist, Query)[[1]]
			if(!accepted %in% usages)
				stop(paste("ID used for accepted name is not an usage name",
								"for this concept."))
			# Set synonyms
			Query <- paste0("UPDATE \"", paste0(names2concepts,
							collapse = "\".\""), "\"\n",
					"SET \"NameStatus\" = 'synonym'\n",
					"WHERE \"TaxonConceptID\" IN (",
					paste0(usages[usages != accepted], collapse = ",", ");\n"))
			dbSendQuery(taxlist, Query)
			# Set accepted name
			Query <- paste0("UPDATE \"", paste0(names2concepts,
							collapse = "\".\""), "\"\n",
					"SET \"NameStatus\" = 'accepted'\n",
					"WHERE \"TaxonConceptID\" = ", accepted, ";\n")
			dbSendQuery(taxlist, Query)
			message("DONE!")
		})
