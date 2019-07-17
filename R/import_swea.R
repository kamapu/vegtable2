# TODO:   Personalized import routine for SWEA-Dataveg
# 
# Author: Miguel Alvarez
################################################################################

import_swea <- function(conn,
		header_schema="swea_dataveg",
		coverconvert_schema="commons",
		cover_names=c("br_bl","b_bbds","ordinal"),
		geometry="plot_centroid",
		description=c(
				database="SWEA-Dataveg",
				author="Miguel Alvarez",
				givd_code="AF-00-006",
				url="https://kamapu.github.io/swea_dataveg.html"),
		add_relation,
		references="M:/Literatur/00_DB_JabRef/MiguelReferences.bib", ...) {
	message("\n## Reading Postgres tables...\n")
	VEG <- postgres2vegtable(conn=conn, header_schema=header_schema,
			coverconvert_schema=coverconvert_schema, cover_names=cover_names,
			geometry=geometry, description=description, ...)
	if(!missing(add_relation)) {
		for(i in names(add_relation))
			VEG@relations[[i]] <- dbReadTable(conn=conn, c(add_relation[i], i))
	}
	# Countries
	message("DONE\n\n## Importing countries...\n")
	Countries <- pgGetGeom(conn, c(header_schema, "header"), geometry,
			other.cols=c("ReleveID"))
	Countries_map <- pgGetGeom(conn, c("commons", "countries_map"), "unit",
			other.cols="ADM0_A3")
	Countries <- dbGetQuery(conn, paste0(
					"SELECT \"ReleveID\", \"ADM0_A3\"\n",
					"FROM ", header_schema,".header, commons.countries_map\n",
					"WHERE ST_Intersects(Geometry(", header_schema,".header.",
					geometry,"), commons.countries_map.unit);"))
	VEG@header$country_code <- with(Countries,
			ADM0_A3[match(VEG@header$ReleveID, ReleveID)])
	Countries <- dbGetQuery(conn, "SELECT * FROM commons.countries;")
	colnames(Countries) <- c("country_code","name_short","name_long",
			"population","sov_code_1","sov_code_2","sov_state","continent")
	VEG@relations$country_code <- Countries
	# References
	message("DONE\n\n## Importing source references...\n")
	# References
	Refs <- ReadBib(references, check="warn")
	Refs <- as.data.frame(Refs, stringsAsFactors=FALSE)
	Refs <- data.frame(bibtexkey=rownames(Refs), Refs, stringsAsFactors=FALSE)
	VEG@relations$data_source <- data.frame(VEG@relations$data_source,
			Refs[VEG@relations$data_source$bibtexkey,-1],
			stringsAsFactors=FALSE)
	VEG@relations$data_source <- VEG@relations$data_source[,
			apply(VEG@relations$data_source, 2, function(x) !all(is.na(x)))]
	VEG@species@taxonViews <- data.frame(VEG@species@taxonViews,
			Refs[VEG@species@taxonViews$view_bibtexkey,-1],
			stringsAsFactors=FALSE)
	VEG@species@taxonViews <- VEG@species@taxonViews[,
			apply(VEG@species@taxonViews, 2, function(x) !all(is.na(x)))]
	message("DONE\n")
	return(VEG)
}

# Version for sudamerica
import_sudamerica <- function(conn, header_schema="sudamerica", description,
		...) {
	if(missing(description))
		description=c(database="sudamerica", author="Miguel Alvarez",
				givd_code="SA-CL-001",
				url="https://kamapu.github.io/sudamerica.html")
	import_swea(conn, header_schema, description=description)
}
