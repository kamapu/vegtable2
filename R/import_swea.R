# TODO:   Personalized import routine for SWEA-Dataveg
# 
# Author: Miguel Alvarez
################################################################################

import_swea <- function(conn, header_schema="swea_dataveg",
		coverconvert_schema="commons",
		cover_names=c("br_bl","b_bbds","ordinal"), geometry="plot_centroid",
		description=c(database="SWEA-Dataveg", author="Miguel Alvarez",
				givd_code="AF-00-006",
				url="https://kamapu.github.io/swea_dataveg.html"),
		add_relation=c(country_code="commons"),
		references="M:/Literatur/00_DB_JabRef/MiguelReferences.bib", ...) {
	VEG <- postgres2vegtable(conn=conn, header_schema=header_schema,
			coverconvert_schema=coverconvert_schema, cover_names=cover_names,
			geometry=geometry, description=description, ...)
	for(i in names(add_relation)) {
		VEG@relations[[i]] <- dbReadTable(conn=conn, c(add_relation[i], i))
	}
	# References
	Refs <- ReadBib(references, check ="warn")
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
	return(VEG)
}

## TV-version is deprecated
## swea_import <- function(db="Sweadataveg", ...) {
##     x <- tv2vegtable(db, ...)
##     # 1: Modify presence-absence
##     new_pa <- rep(1, nrow(x@samples))
##     new_pa[is.na(x@samples$pr_ab)] <- NA
##     x@samples$pr_ab <- new_pa
##     # 2: Schultka to percent
##     x@samples$percen[!is.na(x@samples$schltk)] <- as.numeric(paste(
##                     x@samples$schltk[!is.na(x@samples$schltk)]))
##     x@samples <- x@samples[,colnames(x@samples) != "schltk"]
##     # 3: Insert default conversion
##     data(braun_blanquet, envir=environment())
##     x@coverconvert <- braun_blanquet
##     # output
##     return(x)
## }
