# TODO:   Inserting observations (samples and header) in postgres
# 
# Author: Miguel Alvarez
################################################################################

pg_insert_observations <- function(conn, samples, header, names2concepts,
		db_samples, db_header, geom, ...) {
	# Control occurrence of variables
	if(!all(c("ReleveID","TaxonUsageID") %in% colnames(samples)))
		stop("'ReleveID' and 'TaxonUsageID' are mandatory variables in 'samples'.")
	if(!all(c("ReleveID","original_number") %in% colnames(header@data)))
		stop("'ReleveID' and 'original_number' are mandatory variables in 'header'.")
	# Check uniqueness in IDs
	if(any(duplicated(header$ReleveID)))
		stop("Values of 'ReleveID' in 'header' have to be unique")
	if(any(duplicated(header$original_number)))
		stop("Values of 'original_number' in 'header' have to be unique")
	# Check missmatchings on releve IDs
	if(!all(header$ReleveID %in% samples$ReleveID))
		stop("Some values of 'ReleveID' are missing in 'samples'.")
	if(!all(samples$ReleveID %in% header$ReleveID))
		stop("Some values of 'ReleveID' are missing in 'header'.")
	# Check missmatchings with taxon usage IDs
	SQL <- paste0("SELECT \"TaxonUsageID\"\n",
			"FROM \"", paste(names2concepts, collapse="\".\""), "\";\n")
	usage_id <- dbGetQuery(conn, SQL)[,1]
	if(any(!samples$TaxonUsageID %in% samples$TaxonUsageID))
		stop("Some of the values of 'TaxonUsageID' in 'samples' are absent in the database.")
	# Column names in database
	description <- get_description(conn)
	if(any(!colnames(samples) %in%
					description[description$schema == db_samples[1] &
									description$table == db_samples[2],
							"column"]))
		stop("Some variables from 'samples' are not in database.")
	if(any(!colnames(header@data) %in%
					description[description$schema == db_header[1] &
									description$table == db_header[2],
							"column"]))
		stop("Some variables from 'samples' are not in database.")
	# End of checks
	SQL <- paste0("SELECT MAX(\"ReleveID\")\n",
			"FROM \"", paste(db_header, collapse="\".\""), "\";\n")
	N <- unlist(dbGetQuery(conn, SQL))
	# Import of header
	old_id <- header$ReleveID
	header@data <- header@data[,colnames(header@data) != "ReleveID"]
	pgInsert(conn, db_header, header, geom)
	# Retrieve new codes
	SQL <- paste0("SELECT \"ReleveID\",original_number", "\n",
			"FROM \"", paste(db_header, collapse="\".\""), "\"\n",
			"WHERE \"ReleveID\" > ", N, ";\n")
	ref_ids <- dbGetQuery(conn, SQL)
	new_id <- with(ref_ids, ReleveID[match(header$original_number,
							original_number)])
	samples$ReleveID <- new_id[match(samples$ReleveID, old_id)]
	pgInsert(conn, db_samples, samples)
	message("DONE")
}
