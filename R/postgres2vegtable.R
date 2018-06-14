# TODO:   A function for import of the PostgreSQL version of SWEA-Dataveg
# 
# Author: Miguel Alvarez
################################################################################

postgres2vegtable <- function(conn, header_schema, species_schema,
		relations_schema, samples_schema, layers_schema, coverconvert_schema,
		cover_names, geometry, description) {
	# Import taxonomic tables
	if(missing(species_schema)) species_schema <- header_schema
	species_obj <- list()
	
	for(i in c("taxonNames","names2concepts","taxonRelations","taxonViews",
			"taxonLevels","taxonTraits")) {
		species_obj[[i]] <- dbReadDataFrame(conn=conn,
				name=c(species_schema, i))
	}
	# Format for taxlist
	species_obj$taxonRelations$Level <- with(species_obj$taxonLevels,
			factor(species_obj$taxonRelations$Level, Level[order(rank)]))
	species_obj$taxonNames$TaxonConceptID <- with(species_obj$names2concepts,
			TaxonConceptID[match(species_obj$taxonNames$TaxonUsageID,
							TaxonUsageID)])
	species_obj$taxonRelations$Basionym <- with(species_obj$names2concepts[
					species_obj$names2concepts$NameStatus == "basionym",],
			TaxonUsageID[match(species_obj$taxonRelations$TaxonConceptID,
							TaxonConceptID)])
	species_obj$taxonRelations$AcceptedName <- with(species_obj$names2concepts[
					species_obj$names2concepts$NameStatus == "accepted",],
			TaxonUsageID[match(species_obj$taxonRelations$TaxonConceptID,
							TaxonConceptID)])
	species_obj <- with(species_obj,
			new("taxlist", taxonNames=taxonNames, taxonRelations=taxonRelations,
					taxonViews=taxonViews, taxonTraits=taxonTraits))
	# Header data
	if(missing(header_schema)) header_schema <- species_schema
	header <- dbReadDataFrame(conn=conn, name=c(header_schema, "header"))
	if(!missing(geometry)) 
		header <- data.frame(header[,colnames(header) != geometry],
				dbGetQuery(conn,
						paste0("SELECT \"ReleveID\", st_x (", geometry,
								") longitude, st_y (", geometry,
								") latitude FROM ", header_schema, ".header;")),
				stringsAsFactors=FALSE)
	# Relations
	if(missing(relations_schema)) relations_schema <- header_schema
	table_name <- dbGetQuery(conn,
			paste0("SELECT table_name FROM information_schema.tables WHERE table_schema='",
					relations_schema, "';"))[,1]
	table_name <- intersect(table_name, colnames(header))
	relations <- list()
	for(i in table_name)
		relations[[i]] <- dbReadDataFrame(conn=conn,
				name=c(relations_schema, i))
	# Samples
	if(missing(samples_schema)) samples_schema <- header_schema
	samples <- dbReadDataFrame(conn=conn, name=c(samples_schema, "samples"))
	# Layers
	if(missing(layers_schema)) layers_schema <- header_schema
	table_name <- dbGetQuery(conn,
			paste0("SELECT table_name FROM information_schema.tables WHERE table_schema='",
					layers_schema, "';"))[,1]
	table_name <- table_name[table_name %in% colnames(samples) &
					!table_name %in% cover_names]
	layers <- list()
	for(i in table_name)
		layers[[i]] <- dbReadDataFrame(conn=conn, name=c(layers_schema, i))
	# Coverconverts
	if(missing(coverconvert_schema)) coverconvert_schema <- header_schema
	coverconvert <- list()
	for(i in cover_names)
		coverconvert[[i]] <- dbReadDataFrame(conn=conn,
				name=c(coverconvert_schema, i))
	coverconvert_obj <- new("coverconvert")
	for(i in names(coverconvert)) {
		coverconvert_obj@value[[i]] <- with(coverconvert[[i]], factor(symbol,
						levels=symbol))
		coverconvert_obj@conversion[[i]] <- with(coverconvert[[i]],
				c(bottom[1], top))
	}
	# Assembly output object
	if(missing(description)) description <- c(database="no named")
	VEG <- new("vegtable", description=description, samples=samples,
			layers=layers, header=header, species=species_obj,
			relations=relations, coverconvert=coverconvert_obj)
	return(VEG)
}
