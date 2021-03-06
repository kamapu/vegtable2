% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db2vegtable.R
\name{db2vegtable}
\alias{db2vegtable}
\alias{db2vegtable.PostgreSQLConnection}
\alias{import_swea}
\alias{import_sam}
\alias{import_bernice}
\title{Import PostgreSQL databases into vegtable objects}
\usage{
db2vegtable(conn, ...)

\method{db2vegtable}{PostgreSQLConnection}(
  conn,
  header,
  sql_header,
  samples,
  relations,
  layers,
  coverconvert,
  geometry,
  description,
  as_list = FALSE,
  ...
)

import_swea(
  conn,
  header = c("swea_dataveg", "header"),
  samples = c("swea_dataveg", "samples"),
  relations = list(globe_plots = c("swea_dataveg", "globe_plots"), swea1_code =
    c("swea_dataveg", "swea1_code"), soil_moisture = c("swea_dataveg", "soil_moisture"),
    soil_texture = c("swea_dataveg", "soil_texture"), community_type = c("commons",
    "community_type"), naturalness = c("swea_dataveg", "naturalness"), record_type =
    c("swea_dataveg", "record_type")),
  layers = list(veg_layer = c("swea_dataveg", "veg_layer"), spec_miguel =
    c("specimens", "specimens_miguel")),
  coverconvert = list(br_bl = c("commons", "br_bl"), b_bbds = c("commons", "b_bbds"),
    ordinal = c("commons", "ordinal")),
  geometry = "plot_centroid",
  get_countries = TRUE,
  get_data_sources = TRUE,
  bib_args = list(),
  taxon_names = c("tax_commons", "taxonNames"),
  taxon_relations = c("swea_dataveg", "taxonRelations"),
  taxon_traits = c("swea_dataveg", "taxonTraits"),
  taxon_views = c("bib_references", "main_table"),
  taxon_levels = c("tax_commons", "taxonLevels"),
  names2concepts = c("swea_dataveg", "names2concepts"),
  ...
)

import_sam(
  conn,
  header = c("sudamerica", "header"),
  samples = c("sudamerica", "samples"),
  relations = list(community_type = c("commons", "community_type")),
  layers = list(spec_miguel = c("specimens", "specimens_miguel")),
  coverconvert = list(br_bl = c("commons", "br_bl"), b_bbds = c("commons", "b_bbds"),
    ordinal = c("commons", "ordinal")),
  geometry = "plot_centroid",
  get_countries = TRUE,
  get_data_sources = TRUE,
  bib_args = list(),
  taxon_names = c("tax_commons", "taxonNames"),
  taxon_relations = c("sudamerica", "taxonRelations"),
  taxon_traits = c("sudamerica", "taxonTraits"),
  taxon_views = c("bib_references", "main_table"),
  taxon_levels = c("tax_commons", "taxonLevels"),
  names2concepts = c("sudamerica", "names2concepts"),
  ...
)

import_bernice(
  conn,
  description = c(Title = "Parthenium survey in Lake Baringo", Author =
    "Bernice Mereina Sainepo", Source = "SWEA-Dataveg (GIVD-AF-00-006)", Version =
    Sys.Date()),
  head_cols = c("ReleveID", "code_trr228", "original_number", "record_date",
    "plot_size", "data_source", "elevation"),
  samples_cols = c("record_id", "ReleveID", "quadrant", "TaxonUsageID",
    "misspelled_name", "cover_percentage", "frequency"),
  ...
)
}
\arguments{
\item{conn}{A database connection provided by \code{\link[=dbConnect]{dbConnect()}}.}

\item{...}{Further arguments passed to \code{\link[=db2taxlist]{db2taxlist()}}.}

\item{header, samples}{Character vectors indicating the schema and table
containing header and samples information, respectively.}

\item{sql_header}{SQL statement to be used instead of \code{header}.}

\item{relations}{A list of vectors indicating the schema and table in the
database, excluding 'data_source'.}

\item{layers, coverconvert}{Lists of vectors for the respective slots,
each containing schema and name of required table.}

\item{geometry}{Name of the variable in header containing the geometry of
the plots.}

\item{description}{Named vector with metadata.}

\item{as_list}{Logical value indicating whether a list or an object of class
\linkS4class{vegtable} should be returned.}

\item{get_countries}{Logical argument, specific for the databases
'sudamerica' and 'SWEA-Dataveg', indicating whether country information
should be reimported from integrated map.}

\item{get_data_sources}{Logical argument whether references should be
imported as data sources or not.}

\item{bib_args}{List of arguments passed to \code{\link[biblio:read_bib]{biblio::read_bib()}}.}

\item{taxon_names, taxon_relations, taxon_traits, taxon_views, taxon_levels, names2concepts}{Arguments passed to \code{\link[=db2taxlist]{db2taxlist()}}.}

\item{head_cols}{Character vector indicating the header variables to be
imported (except the coordinates).}

\item{samples_cols}{Character vector indicating the samples variables to be
imported.}
}
\description{
Import and adaption of Postgres tables into objects of class
\linkS4class{vegtable}.

In the case that some schemas are not mentioned, the function assumes such
tables are located in the same schema as the table header. Thus for
databases placed in just one schema, this need to be set only in argument
\code{header_schema}.
}
