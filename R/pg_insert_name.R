# TODO:   Function for inserting new names in database
# 
# Author: Miguel Alvarez
################################################################################

pg_insert_name <- function(conn, df, schema) {
	if(!"TaxonConceptID" %in% colnames(df))
		stop("Column 'TaxonConceptID' is mandatory in argument 'df'.")
	# 1: Extract usage IDs
	query <- paste0(
"SELECT 
	\"taxonNames\".\"TaxonUsageID\"
FROM 
	\"", schema, "\".\"taxonNames\";")
	TaxonUsageID <- dbGetQuery(conn, query)
	df$TaxonUsageID <- max(TaxonUsageID) + c(1:nrow(df))
	taxon_names <- dbGetQuery(conn,
"SELECT
	column_name
FROM
	information_schema.columns
WHERE
	table_name = 'taxonNames';")
	# 2: Insert to database
	pgInsert(conn, c(schema, "taxonNames"),
			df[,colnames(df) %in% taxon_names[,1]])
	pgInsert(conn, c(schema, "names2concepts"),
			data.frame(df[,c("TaxonUsageID", "TaxonConceptID")],
					NameStatus="synonym", stringsAsFactors=FALSE))
	# 3: restart serials
	next_usage <- dbGetQuery(conn,
			paste0("select max(\"TaxonUsageID\") + 1 from ", schema,
					".\"taxonNames\";"))[,1]
	query <- paste0("alter sequence sudamerica.taxon_usage_id restart with ",
			next_usage, ";")
	dbSendQuery(conn, query)
}
