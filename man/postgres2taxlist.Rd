\name{postgres2taxlist}
\alias{postgres2taxlist}
\alias{swea_tax}
\alias{sudamerica_tax}

\title{Import PostgreSQL databases into taxlist objects}
\description{
Ad-hoc function for importing Postgres tables into objects of class
\code{\linkS4class{taxlist}}.
}
\usage{
## Basic function
postgres2taxlist(conn, taxon_names, taxon_relations, taxon_traits,
		taxon_views, taxon_levels, names2concepts, subset_levels=TRUE,
		subset_views=TRUE, as_list=FALSE, verbose=TRUE, ...)

## Alternative for sudamerica
swea_tax(conn, taxon_names=c("tax_commons","taxonNames"),
		taxon_relations=c("swea_dataveg","taxonRelations"),
		taxon_traits=c("swea_dataveg","taxonTraits"),
		taxon_views=c("commons","data_source"),
		taxon_levels=c("tax_commons","taxonLevels"),
		names2concepts=c("swea_dataveg","names2concepts"),
		...)

## Alternative for SWEA-Dataveg
sudamerica_tax(conn, taxon_names=c("tax_commons","taxonNames"),
		taxon_relations=c("sudamerica","taxonRelations"),
		taxon_traits=c("sudamerica","taxonTraits"),
		taxon_views=c("commons","data_source"),
		taxon_levels=c("tax_commons","taxonLevels"),
		names2concepts=c("sudamerica","names2concepts"),
		...)
}
\arguments{
\item{conn}{A database connection provided by \code{\link[DBI]{dbConnect}}.}
\item{taxon_names,taxon_relations,taxon_traits,taxon_views}{Character vectors
	indicating the name of the schema and the table containing the information
	for the respective slots.}
\item{taxon_levels,names2concepts}{Character vectors indicating the name of
	schema and table indicating the taxonomic ranks and the correspondence of
	names to taxonomic concepts.}
\item{subset_levels}{Logical value indicating whether taxonomic ranks should be
	restricted to the used ones or all ranks available in the database.}
\item{subset_views}{Logical value indicating whether to restrict the list of
	views to the used ones or all.}
\item{as_list}{Logical value indicating whether the output should be a list or a
	\code{\linkS4class{taxlist}} object.}
\item{verbose}{Logical value, whether messages should be printed or not.}
\item{...}{Further arguments passed among methods. In the two wrappers the
	arguments are passed to \code{'postgres2taxlist'}.}.
}
\details{
This function has been modified to a new version of the database.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
