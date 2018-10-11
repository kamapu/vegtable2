\name{import_swea}
\alias{import_swea}

\title{Import SWEA-Dataveg}
\description{
This function performs a personalized import for the database
\href{http://www.givd.info/ID/AF-00-006}{SWEA-Dataveg} and is a wrapper for
\code{\link{postgres2vegtable}}.
}
\usage{
import_swea(conn, header_schema="swea_dataveg", coverconvert_schema="commons",
		cover_names=c("br_bl","b_bbds","ordinal"), geometry="plot_centroid",
		description=c(database="SWEA-Dataveg", author="Miguel Alvarez",
				givd_code="AF-00-006",
				url="https://kamapu.github.io/swea_dataveg.html"),
		add_relation=c(country_code="commons"),
		references="M:/Literatur/00_DB_JabRef/MiguelReferences.bib", ...)
}
\arguments{
\item{conn}{A connection to database created by \code{\link[DBI]{dbConnect}}.}
\item{header_schema, coverconvert_schema}{Character value indicating the schema
	containing the header table and tables for conversion of cover,
	respectively.}
\item{cover_names}{Character vector indicating the names of cover values in
	table samples.}
\item{geometry}{Character value indicating the name of column in table header,
	which contains the coordinates for the plots (only point geometry allowed).}
\item{description}{Named character vector containing information to be attached
	as metadata.}
\item{add_relation}{Named character vector indicating relations located in
	different schemas (name is the relation table and value is the schema).}
\item{references}{Path to bibtex file including bibliographic references.}
\item{...}{Further arguments passed to \code{\link{postgres2vegtable}}.}
}
\details{
This function is wrote for a PostgreSQL version of
\href{http://www.givd.info/ID/AF-00-006}{SWEA-Dataveg}.
}
\value{
A \code{\linkS4class{vegtable}} object.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
