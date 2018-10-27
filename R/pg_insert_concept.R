# TODO:   Insert new taxon concept
# 
# Author: Miguel Alvarez
################################################################################

pg_insert_concept <- function(conn, schema, df) {
	df <- clean_strings(df)
	if(any(!c("TaxonName","AuthorName") %in% colnames(df)))
		stop("Columns 'TaxonName' and 'AuthorName' are mandatory in argument 'df'.")
	if("TaxonConceptID" %in% colnames(df))
		stop("Column 'TaxonConceptID' detected in 'df'. Use 'pg_insert_synonym' instead?")
	taxa <- postgres2taxlist(conn, schema)
	## Cross-check
	# 1: Check duplicated combinations in 'df'
	if(any(duplicated(df[,c("TaxonName","AuthorName")])))
		stop("Duplicated combinations detected in 'df'.")
	# 2: Check combinations already existing in database
	if(any(with(df, paste(TaxonName, AuthorName)) %in% with(taxa@taxonNames,
					paste(TaxonName, AuthorName))))
		stop("Some combinations in 'df' already exist in database.")
	# 3: Check names already existing as accepted names
	if(any(df$TaxonName %in% accepted_name(taxa)$TaxonName))
		stop("Some names are already existing as accepted names in database.")
	# 4: Check existence of parents in database
	if("Parent" %in% colnames(df) &
			!all(df$Parent %in% taxa@taxonRelations$TaxonConceptID))
		stop("Some entries for 'Parent' in 'df' are not occurring in database.")
	# 5: Check existence of levels in database
	if("Level" %in% colnames(df) &
			!all(paste(df$Level) %in% taxlist::levels(taxa)))
		stop("Some entries for 'Level' in 'df' are not occurring in database.")
	# 6: Check existence of view IDs in database
	if("ViewID" %in% colnames(df) &
			!all(paste(df$ViewID) %in% taxa@taxonViews$ViewID))
		stop("Some entries for 'ViewID' in 'df' are not occurring in database.")
	# 7: Check consistency of levels
	if("Level" %in% colnames(df) & "Parent" %in% colnames(df)) {
		new_levels <- as.integer(factor(df$Level, levels=taxlist::levels(taxa)))
		parent_levels <- with(taxa@taxonRelations,
				as.integer(Level[match(df$Parent, TaxonConceptID)]))
		if(any(new_levels >= parent_levels))
			stop("Children cannot be of equal or higher level than the respective parents.")
	}
	## TODO: Allow the possibility of inserting some taxon traits
	## Prepare data frame
	# 1: Add new IDs to data frame
	df$TaxonUsageID <- max(taxa@taxonNames$TaxonUsageID) + c(1:nrow(df))
	df$TaxonConceptID <- max(taxa@taxonRelations$TaxonConceptID) + c(1:nrow(df))
	# 2: Get colnames of Postgres tables
	taxon_names <- dbGetQuery(conn,
			"SELECT column_name FROM information_schema.columns WHERE table_name = 'taxonNames';")
	taxon_names <- unique(taxon_names$column_name)
	taxon_relations <- dbGetQuery(conn,
			"SELECT column_name FROM information_schema.columns WHERE table_name = 'taxonRelations';")
	taxon_relations <- unique(taxon_relations$column_name)
	## Import tables
	# 2: Insert to database
	pgInsert(conn, c(schema, "taxonNames"), df[,colnames(df) %in% taxon_names])
	pgInsert(conn, c(schema, "taxonRelations"), df[,colnames(df) %in%
							taxon_relations])
	pgInsert(conn, c(schema, "names2concepts"),
			data.frame(df[,c("TaxonUsageID", "TaxonConceptID")],
					NameStatus="accepted", stringsAsFactors=FALSE))
	message("DONE")
}
