# TODO:   Insert new taxon concept
# 
# Author: Miguel Alvarez
################################################################################

pg_insert_concept <- function(conn, schema, df) {
	if(any(!c("TaxonName","AuthorName") %in% colnames(df)))
		stop("Columns 'TaxonName' and 'AuthorName' are mandatory in argument 'df'.")
	if("TaxonConceptID" %in% colnames(df))
		stop("Column 'TaxonConceptID' detected in 'df'. Use 'pg_insert_name' instead?")
	# 1: Extract concept and usage IDs
	query <- paste0(
"SELECT 
	\"taxonNames\".\"TaxonUsageID\"
FROM 
	\"", schema, "\".\"taxonNames\";")
	TaxonUsageID <- dbGetQuery(conn, query)
	query <- paste0(
"SELECT 
	\"taxonRelations\".\"TaxonConceptID\"
FROM 
	\"", schema, "\".\"taxonRelations\";")
	TaxonConceptID <- dbGetQuery(conn, query)
	# 2: Add new IDs to data frame
	df$TaxonUsageID <- max(TaxonUsageID) + c(1:nrow(df))
	df$TaxonConceptID <- max(TaxonConceptID) + c(1:nrow(df))
	# 3: Get colnames of Postgres tables
	taxon_names <- dbGetQuery(conn,
"SELECT
	column_name
FROM
	information_schema.columns
WHERE
	table_name = 'taxonNames';")
	taxon_relations <- dbGetQuery(conn,
"SELECT
	column_name
FROM
	information_schema.columns
WHERE
	table_name = 'taxonRelations';")
	# 2: Insert to database
	pgInsert(conn, c(schema, "taxonNames"),
			df[,colnames(df) %in% taxon_names[,1]])
	pgInsert(conn, c(schema, "taxonRelations"),
			df[,colnames(df) %in% taxon_relations[,1]])
	pgInsert(conn, c(schema, "names2concepts"),
			data.frame(df[,c("TaxonUsageID", "TaxonConceptID")],
					NameStatus="accepted", stringsAsFactors=FALSE))
	message("DONE")
}
