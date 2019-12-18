# TODO:   Import of taxlist objects from PostgreSQL databases
# 
# Author: Miguel Alvarez
################################################################################

# Main function
postgres2taxlist <- function(conn, taxon_names, taxon_relations, taxon_traits,
		taxon_views, taxon_levels, names2concepts, subset_levels=TRUE,
		subset_views=TRUE, as_list=FALSE, verbose=TRUE, ...) {
	species_obj <- list()
	# Import taxon names
	if(verbose)
		message("Importing taxon names...")
	SQL <- paste0("SELECT *\n",
			"FROM \"", paste(taxon_names, collapse="\".\""), "\";\n")
	species_obj$taxonNames <- dbGetQuery(conn, SQL)
	# Import taxon concepts
	if(verbose)
		message("Importing taxon concepts...")
	SQL <- paste0("SELECT *\n",
			"FROM \"", paste(taxon_relations, collapse="\".\""), "\";\n")
	species_obj$taxonRelations <- dbGetQuery(conn, SQL)
	# Link names and concepts
	SQL <- paste0("SELECT *\n",
			"FROM \"", paste(names2concepts, collapse="\".\""), "\";\n")
	concepts <- dbGetQuery(conn, SQL)
	species_obj$taxonNames$TaxonConceptID <-
			concepts$TaxonConceptID[match(species_obj$taxonNames$TaxonUsageID,
							concepts$TaxonUsageID)]
	species_obj$taxonNames <-
			species_obj$taxonNames[
					!is.na(species_obj$taxonNames$TaxonConceptID),]
	# Add status (accepted names)
	species_obj$taxonRelations$AcceptedName <-
			with(concepts[concepts$NameStatus == "accepted",],
					TaxonUsageID[
							match(species_obj$taxonRelations$TaxonConceptID,
									TaxonConceptID)])
	species_obj$taxonRelations$Basionym <-
			with(concepts[concepts$NameStatus == "basionym",],
					TaxonUsageID[
							match(species_obj$taxonRelations$TaxonConceptID,
									TaxonConceptID)])
	# Retrieve levels
	SQL <-  paste0("SELECT *\n",
			"FROM \"", paste(taxon_levels, collapse="\".\""), "\";\n")
	tax_levels <- dbGetQuery(conn, SQL)
	if(subset_levels) tax_levels <- tax_levels[tax_levels$Level %in%
						species_obj$taxonRelations$Level,]
	tax_levels <- tax_levels[order(tax_levels$rank),]
	species_obj$taxonRelations$Level <- factor(species_obj$taxonRelations$Level,
			tax_levels$Level)
	# Retrieve taxon traits
	if(!missing(taxon_traits)) {
		SQL <-  paste0("SELECT *\n",
				"FROM \"", paste(taxon_traits, collapse="\".\""), "\";\n")
		species_obj$taxonTraits <- dbGetQuery(conn, SQL)
	} else species_obj$taxonTraits <- data.frame(TaxonConceptID=integer(0))
	# Import taxon views
	if(verbose)
		message("Importing taxon views...")
	SQL <-  paste0("SELECT *\n",
			"FROM \"", paste(taxon_views, collapse="\".\""), "\";\n")
	species_obj$taxonViews <- dbGetQuery(conn, SQL)
	colnames(species_obj$taxonViews)[colnames(species_obj$taxonViews) ==
					"data_source"] <- "ViewID"
	if(subset_views)
		species_obj$taxonViews <- species_obj$taxonViews[
				species_obj$taxonViews$ViewID %in%
						species_obj$taxonRelations$ViewID,]
	if(verbose)
		message("DONE")
	if(as_list) return(species_obj) else {
		species_obj <- with(species_obj,
				new("taxlist",
						taxonNames=clean_strings(taxonNames),
						taxonRelations=clean_strings(taxonRelations),
						taxonViews=clean_strings(taxonViews),
						taxonTraits=clean_strings(taxonTraits)))
		return(species_obj)
	}
}

# Wrappers
swea_tax <- function(conn,
		taxon_names=c("tax_commons","taxonNames"),
		taxon_relations=c("swea_dataveg","taxonRelations"),
		taxon_traits=c("swea_dataveg","taxonTraits"),
		taxon_views=c("commons","data_source"),
		taxon_levels=c("tax_commons","taxonLevels"),
		names2concepts=c("swea_dataveg","names2concepts"),
		...) {
	return(postgres2taxlist(conn, taxon_names, taxon_relations, taxon_traits,
					taxon_views, taxon_levels, names2concepts, ...))
}

# Wrappers
sudamerica_tax <- function(conn,
		taxon_names=c("tax_commons","taxonNames"),
		taxon_relations=c("sudamerica","taxonRelations"),
		taxon_traits=c("sudamerica","taxonTraits"),
		taxon_views=c("commons","data_source"),
		taxon_levels=c("tax_commons","taxonLevels"),
		names2concepts=c("sudamerica","names2concepts"),
		...) {
	return(postgres2taxlist(conn, taxon_names, taxon_relations, taxon_traits,
					taxon_views, taxon_levels, names2concepts, ...))
}
