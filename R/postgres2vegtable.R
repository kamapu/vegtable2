# TODO:   A function for import of the PostgreSQL version of SWEA-Dataveg
# 
# Author: Miguel Alvarez
################################################################################

postgres2vegtable <- function(conn, tax_args=list(), header, sql_header,
		samples, relations=list(), layers=list(), coverconvert=list(), geometry,
		description, as_list=FALSE, ...) {
	veg_obj <- list()
	# description
	if(!missing(description)) veg_obj$description <- description else
		veg_obj$description <- "Object imported by 'postgres2vegtable()'."
	# species
	veg_obj$species <- do.call(postgres2taxlist, c(conn=conn, tax_args))
	# samples
	message("Importing vegtable body...")
	SQL <- paste0("SELECT *\n",
			"FROM \"", paste(samples, collapse="\".\""), "\";\n")
	veg_obj$samples <- dbGetQuery(conn, SQL)
	# header
	if(missing(sql_header)) {
		if(!missing(geometry)) {
			veg_obj$header <- as.data.frame(pgGetGeom(conn, header, geometry),
					stringsAsFactors=FALSE)
			colnames(veg_obj$header)[ncol(veg_obj$header) + c(-1,0)] <-
					c("longitude","latitude")
		} else {
			SQL <- paste0("SELECT *\n",
					"FROM \"", paste(header, collapse="\".\""), "\";\n")
			veg_obj$header <- dbGetQuery(conn, SQL)
		}
	} else {
		veg_obj$header <- dbGetQuery(conn, sql_header)
		veg_obj$samples <- veg_obj$samples[veg_obj$samples$ReleveID %in%
						veg_obj$header$ReleveID,]
	}
	# layers
	veg_obj$layers <- list()
	for(i in names(layers)) {
		SQL <- paste0("SELECT *\n",
				"FROM \"", paste(layers[[i]], collapse="\".\""), "\";\n")
		veg_obj$layers[[i]] <- dbGetQuery(conn, SQL)
		## veg_obj$layers[[i]] <- veg_obj$layers[[i]][veg_obj$layers[[i]][,i] %in%
		##                 veg_obj$samples[,i],]
	}
	# relations
	veg_obj$relations <- list()
	for(i in names(relations)) {
		SQL <- paste0("SELECT *\n",
				"FROM \"", paste(relations[[i]], collapse="\".\""), "\";\n")
		veg_obj$relations[[i]] <- dbGetQuery(conn, SQL)
		## veg_obj$relations[[i]] <- veg_obj$relations[[i]][
		##         veg_obj$relations[[i]][,i] %in% veg_obj$header[,i],]
	}
	# coverconvert
	veg_obj$coverconvert <- new("coverconvert")
	for(i in names(coverconvert)) {
		SQL <- paste0("SELECT *\n",
				"FROM \"", paste(coverconvert[[i]], collapse="\".\""), "\";\n")
		cover_tab <- dbGetQuery(conn, SQL)
		veg_obj$coverconvert@value[[i]] <- with(cover_tab,
				factor(symbol, levels=symbol))
		veg_obj$coverconvert@conversion[[i]] <- with(cover_tab,
				c(bottom[1], top))
	}
	# final output
	message("DONE")
	if(as_list) return(veg_obj) else {
		veg_obj <- new("vegtable",
				description=clean_strings(veg_obj$description),
				samples=veg_obj$samples,
				header=clean_strings(veg_obj$header),
				species=veg_obj$species,
				relations=veg_obj$relations,
				coverconvert=veg_obj$coverconvert)
		return(veg_obj)
	}
}

## TODO: Relations and layers can be as subsets
## TODO: If using sql statements in wrappers, detect relations not included in header

# Wrappers
import_sudamerica <- function(conn,
		header=c("sudamerica","header"),
		samples=c("sudamerica","samples"),
		relations=list(
				community_type=c("commons","community_type"),
				data_source=c("commons","data_source")
		),
		layers=list(
				spec_miguel=c("specimens","specimens_miguel")
		),
		coverconvert=list(
				br_bl=c("commons","br_bl"),
				b_bbds=c("commons","b_bbds"),
				ordinal=c("commons","ordinal")
		),
		geometry="plot_centroid",
		get_countries=TRUE,
		...) {
	tax_args <- as.list(formals(sudamerica_tax))
	tax_args <- tax_args[sapply(tax_args, length) > 1]
	tax_args <- lapply(tax_args, eval)
	# Final object	
	veg_obj <- postgres2vegtable(conn, tax_args, header, samples=samples,
			relations=relations, layers=layers,
			coverconvert=coverconvert, geometry=geometry, ...)
	# Adding Country codes
	if(get_countries) {
		message("Re-Importing countries...")
		Countries <- pgGetGeom(conn, header, geometry, other.cols=c("ReleveID"))
		Countries_map <- pgGetGeom(conn, c("commons", "countries_map"), "unit",
				other.cols="ADM0_A3")
		Countries <- dbGetQuery(conn, paste0(
						"SELECT \"ReleveID\", \"ADM0_A3\"\n",
						"FROM \"", paste(header, collapse="\".\""),
						"\", commons.countries_map\n",
						"WHERE ST_Intersects(Geometry(\"",
						paste(header, collapse="\".\""),"\".\"",
						geometry,"\"), commons.countries_map.unit);"))
		veg_obj@header$country_code <- with(Countries,
				ADM0_A3[match(veg_obj@header$ReleveID, ReleveID)])
		Countries <- dbGetQuery(conn, "SELECT * FROM commons.countries;")
		colnames(Countries) <- c("country_code","name_short","name_long",
				"population","sov_code_1","sov_code_2","sov_state","continent")
		veg_obj@relations$country_code <- Countries
		message("DONE")
	}
	return(veg_obj)
}

# Wrappers
import_swea <- function(conn,
		header=c("swea_dataveg","header"),
		samples=c("swea_dataveg","samples"),
		relations=list(
				globe_plots=c("swea_dataveg","globe_plots"),
				swea1_code=c("swea_dataveg","swea1_code"),
				soil_moisture=c("swea_dataveg","soil_moisture"),
				soil_texture=c("swea_dataveg","soil_texture"),
				community_type=c("commons","community_type"),
				data_source=c("commons","data_source"),
				naturalness=c("swea_dataveg","naturalness"),
				record_type=c("swea_dataveg","record_type")
		),
		layers=list(
				veg_layer=c("swea_dataveg","veg_layer"),
				spec_miguel=c("specimens","specimens_miguel")
		),
		coverconvert=list(
				br_bl=c("commons","br_bl"),
				b_bbds=c("commons","b_bbds"),
				ordinal=c("commons","ordinal")
		),
		geometry="plot_centroid",
		get_countries=TRUE,
		...) {
	tax_args <- as.list(formals(swea_tax))
	tax_args <- tax_args[sapply(tax_args, length) > 1]
	tax_args <- lapply(tax_args, eval)
	# Final object	
	veg_obj <- postgres2vegtable(conn, tax_args, header, samples=samples,
					relations=relations, layers=layers,
					coverconvert=coverconvert, geometry=geometry, ...)
	# Adding Country codes
	if(get_countries) {
		message("Re-Importing countries...")
		Countries <- pgGetGeom(conn, header, geometry, other.cols=c("ReleveID"))
		Countries_map <- pgGetGeom(conn, c("commons", "countries_map"), "unit",
				other.cols="ADM0_A3")
		Countries <- dbGetQuery(conn, paste0(
						"SELECT \"ReleveID\", \"ADM0_A3\"\n",
						"FROM \"", paste(header, collapse="\".\""),
						"\", commons.countries_map\n",
						"WHERE ST_Intersects(Geometry(\"",
						paste(header, collapse="\".\""),"\".\"",
						geometry,"\"), commons.countries_map.unit);"))
		veg_obj@header$country_code <- with(Countries,
				ADM0_A3[match(veg_obj@header$ReleveID, ReleveID)])
		Countries <- dbGetQuery(conn, "SELECT * FROM commons.countries;")
		colnames(Countries) <- c("country_code","name_short","name_long",
				"population","sov_code_1","sov_code_2","sov_state","continent")
		veg_obj@relations$country_code <- Countries
		message("DONE")
	}
	return(veg_obj)
}
