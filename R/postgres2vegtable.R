#' @name postgres2vegtable
#' 
#' @title Import PostgreSQL databases into vegtable objects
#' 
#' @description 
#' Import and adaption of Postgres tables into objects of class
#' [vegtable-class].
#' 
#' In the case that some schemas are not mentioned, the function assumes such
#' tables are located in the same schema as the table header. Thus for
#' databases placed in just one schema, this need to be set only in argument
#' `header_schema`.
#' 
#' @param conn A database connection provided by [dbConnect()].
#' @param tax_args List of arguments passed to [postgres2taxlist()].
#' @param header,samples Character vectors indicating the schema and table
#'     containing header and samples information, respectively.
#' @param sql_header SQL statement to be used instead of `header`.
#' @param relations,layers,coverconvert Lists of vectors for the respective
#'     slots, each containing schema and name of required table.
#' @param geometry Name of the variable in header containing the geometry of
#'     the plots.
#' @param description Named vector with metadata.
#' @param as_list Logical value indicating whether a list or an object of class
#'     [vegtable-class] should be returned.
#' @param ... Further arguments passed among methods.
#' @param get_countries Logical argument, specific for the databases
#'     'sudamerica' and 'SWEA-Dataveg', indicating whether country information
#'     should be reimported from integrated map.
#' @param head_cols Character vector indicating the header variables to be
#'     imported (except the coordinates).
#' @param samples_cols Character vector indicating the samples variables to be
#'     imported.
#' 
#' @author Miguel Alvarez, \email{kamapu78@@gmail.com}.
#' 
#' @rdname postgres2vegtable
#' 
#' @export postgres2vegtable
#' 
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

#' @rdname postgres2vegtable
#' 
#' @aliases import_sudamerica
#' 
#' @export import_sudamerica
#' 
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

#' @rdname postgres2vegtable
#' 
#' @aliases import_swea
#' 
#' @export import_swea
#' 
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

#' @rdname postgres2vegtable
#' 
#' @aliases import_bernice
#' 
#' @export import_bernice
#' 

import_bernice <- function(conn,
		head_cols=c("ReleveID", "code_trr228", "original_number", "record_date",
				"plot_size", "data_source", "elevation"),
		samples_cols=c("record_id", "ReleveID", "quadrant", "TaxonUsageID",
				"misspelled_name", "cover_percentage", "frequency"),
		...) {
	# header
	Query <- paste0("SELECT \"", paste(head_cols, collapse="\", \""),
			"\", ST_X(plot_centroid) longitude, ST_Y(plot_centroid) latitude\n",
			"FROM swea_dataveg.header\n", "WHERE data_source = 98;\n")
	header <- dbGetQuery(conn, Query)
	# samples
	Query <- paste0("SELECT \"", paste(samples_cols, collapse="\", \""), "\"\n",
			"FROM swea_dataveg.samples\n",
			"WHERE \"ReleveID\" IN (", paste(header$ReleveID, collapse=","),
			");\n")
	samples <- dbGetQuery(conn, Query)
	# relations
	relations <- list()
	# relations: data_source
	Query <- paste0("SELECT data_source, bibtexkey\n",
			"FROM commons.data_source\n", "WHERE data_source = 98;\n")
	relations$data_source <- dbGetQuery(conn, Query)
	# relations: code_trr228
	Query <- paste0("SELECT code_trr228, plot_group, locality, elevation, ",
			"ST_X(centroid) longitude, ST_Y(centroid) latitude\n",
			"FROM swea_dataveg.code_trr228;\n")
	relations$code_trr228 <- dbGetQuery(conn, Query)
	return(new("vegtable", species=swea_tax(conn, ...), relations=relations,
					header=header, samples=samples))
}
