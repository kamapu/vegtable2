# TODO:   Function for inserting new names in database
# 
# Author: Miguel Alvarez
################################################################################

pg_insert_synonym <- function(conn, schema, df) {
	df <- clean_strings(df)
	if(!"TaxonConceptID" %in% colnames(df))
		stop("Column 'TaxonConceptID' is mandatory in argument 'df'.")
	taxa <- postgres2taxlist(conn, schema)
	## Cross-check
	# 1: Check duplicated combinations in 'df'
	if(any(duplicated(df[,c("TaxonName","AuthorName")])))
		stop("Duplicated combinations detected in 'df'.")
	# 2: Check combinations already existing in database
	if(any(with(df, paste(TaxonName, AuthorName)) %in% with(taxa@taxonNames,
					paste(TaxonName, AuthorName))))
		stop("Some combinations in 'df' already exist in database.")
	# 3: Check existence of concepts in database
	if(any(!df$TaxonConceptID %in% taxa@taxonRelations$TaxonConceptID))
		stop("Some of the taxon concepts in 'df' are not occurring in database.")
	## Inserting data
	# 1: Prepare table
	df$TaxonUsageID <- max(taxa@taxonNames$TaxonUsageID) + c(1:nrow(df))
	taxon_names <- dbGetQuery(conn,
			"SELECT column_name FROM information_schema.columns WHERE table_name = 'taxonNames';")
	taxon_names <- unique(taxon_names$column_name)
	# 2: Insert to database
	pgInsert(conn, c(schema, "taxonNames"), df[,colnames(df) %in% taxon_names])
	pgInsert(conn, c(schema, "names2concepts"),
			data.frame(df[,c("TaxonUsageID", "TaxonConceptID")],
					NameStatus="synonym", stringsAsFactors=FALSE))
	message("DONE")
}
