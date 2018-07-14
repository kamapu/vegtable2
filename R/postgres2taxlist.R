# TODO:   Import of taxlist objects from PostgreSQL databases
# 
# Author: Miguel Alvarez
################################################################################

postgres2taxlist <- function(conn, schema) {
	species_obj <- list()
	for(i in c("taxonNames","names2concepts","taxonRelations","taxonViews",
			"taxonLevels","taxonTraits")) {
		species_obj[[i]] <- dbReadDataFrame(conn=conn,
				name=c(schema, i))
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
	if(ncol(species_obj$taxonTraits) == 0)
		species_obj$taxonTraits <- data.frame(TaxonConceptID=integer())
	species_obj <- with(species_obj,
			new("taxlist",
					taxonNames=clean_strings(taxonNames),
					taxonRelations=clean_strings(taxonRelations),
					taxonViews=clean_strings(taxonViews),
					taxonTraits=clean_strings(taxonTraits)))
	return(species_obj)
	message("DONE")
}
