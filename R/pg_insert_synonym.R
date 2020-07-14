#' @name pg_insert_synonym
#' 
#' @title Insert synonyms in PostgreSQL taxonomic lists
#' 
#' @description 
#' Insert synonyms to existing taxa in a PostgreSQL version of [taxlist-class]
#' objects.
#' 
#' This function is updating the tables `taxonNames` and `names2concepts` in
#' the PostgreSQL version of the database.
#' 
#' @param conn A database connection provided by [dbConnect()].
#' @param taxon_names A character vector of length 2 indicating the name of the
#'     respecitve schema and table in Postgres.
#' @param taxon_relations taxon_names A character vector of length 2 indicating
#'     the name of the respecitve schema and table in Postgres.
#' @param names2concepts taxon_names A character vector of length 2 indicating
#'     the name of the respecitve schema and table in Postgres.
#' @param df A data frame with new names and related information (including
#'     taxon concept ID).
#' @param clean A logical value, whether strings in input 'df' should be cleaned
#'     or not (see [clean_strings()]).
#' @param ... Further arguments passed among methods.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @rdname pg_insert_synonym
#' 
#' @export pg_insert_synonym
#' 
pg_insert_synonym <- function(conn, taxon_names, taxon_relations,
		names2concepts, df, clean=TRUE, ...) {
	if(clean)
		df <- clean_strings(df)
	if(any(!c("TaxonName","AuthorName","TaxonConceptID") %in% colnames(df)))
		stop(paste("Columns 'TaxonName', 'AuthorName' and 'TaxonConceptID'",
						"are mandatory in argument 'df'."))
	# Import names
	SQL <-  paste0("SELECT \"TaxonUsageID\",\"TaxonName\",\"AuthorName\"\n",
			"FROM \"", paste(taxon_names, collapse="\".\""), "\";\n")
	tmp_names <- dbGetQuery(conn, SQL)
	# Import taxon concepts
	SQL <-  paste0("SELECT \"TaxonConceptID\"\n",
			"FROM \"", paste(taxon_relations, collapse="\".\""), "\";\n")
	tmp_concepts <- unlist(dbGetQuery(conn, SQL))
	# Import names2concepts
	SQL <-  paste0("SELECT *\n",
			"FROM \"", paste(names2concepts, collapse="\".\""), "\";\n")
	tmp_nam2con <- dbGetQuery(conn, SQL)
	# 1: Check that the names are not yet in use
	if(any(df$TaxonUsageID %in% tmp_nam2con$TaxonUsageID))
		stop("Some of the names in 'df' are already in use.")
	# 2: Check duplicated combinations in 'df'
	if(any(duplicated(df[,c("TaxonName","AuthorName")])))
		stop("Duplicated combinations detected in 'df'.")
	# 3: Check combinations already existing in database (and recycle)
	if(any(with(df, paste(TaxonName, AuthorName)) %in% with(tmp_names,
					paste(TaxonName, AuthorName)))) {
		message(paste("Some combinations in 'df' already exist in database",
						"and will be recycled.\n"))
		df2 <- df
		df2$TaxonUsageID <- with(tmp_names,
				TaxonUsageID[match(paste(df$TaxonName, df$AuthorName),
								paste(TaxonName,AuthorName))])
		df2 <- df2[!is.na(df2$TaxonUsageID),]
		pgInsert(conn, names2concepts,
				data.frame(df2[,c("TaxonUsageID", "TaxonConceptID")],
						NameStatus="synonym", stringsAsFactors=FALSE))
	}
	# Add new names
	if(any(!with(df, paste(TaxonName, AuthorName)) %in% with(tmp_names,
					paste(TaxonName, AuthorName)))) {
		df <- df[!with(df, paste(TaxonName, AuthorName)) %in% with(tmp_names,
					paste(TaxonName, AuthorName)),]
		df$TaxonUsageID <- max(tmp_names$TaxonUsageID) + c(1:nrow(df))
		# Get colnames of Postgres tables
		description <- get_description(conn)
		col_names <- with(description,
				column[schema == taxon_names[1] & table == taxon_names[2]])
		# Insert to database
		pgInsert(conn, taxon_names, df[,col_names])
		pgInsert(conn, names2concepts,
				data.frame(df[,c("TaxonUsageID", "TaxonConceptID")],
						NameStatus="synonym", stringsAsFactors=FALSE))	
	}
	message("DONE")
}

#' @rdname pg_insert_synonym
#' 
#' @aliases insert_synonym_swea
#' 
#' @export insert_synonym_swea
#' 
insert_synonym_swea <- function(conn,
		taxon_names=c("tax_commons", "taxonNames"),
		taxon_relations=c("swea_dataveg", "taxonRelations"),
		names2concepts=c("swea_dataveg", "names2concepts"),
		df, ...) {
	pg_insert_synonym(conn, taxon_names, taxon_relations, names2concepts,
			df, ...)
}
