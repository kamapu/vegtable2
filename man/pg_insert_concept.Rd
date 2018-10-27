\name{pg_insert_concept}
\alias{pg_insert_concept}
\alias{pg_insert_synonym}

\title{Insert Names or Concepts in PostgreSQL Taxonomic Lists}
\description{
Insert synonyms to existing taxa in a PostgreSQL version of
\code{\linkS4class{taxlist}} objects.
}
\usage{
pg_insert_concept(conn, schema, df)

pg_insert_synonym(conn, schema, df)
}
\arguments{
\item{conn}{A database connection provided by \code{\link[DBI]{dbConnect}}.}
\item{schema}{Name of the schema containing taxonomic tables.}
\item{df}{A data frame with new names and related information (including taxon
	concept ID).}
}
\details{
This function is updating the tables \code{'taxonNames'} and
\code{'names2concepts'} in the PostgreSQL version of the database.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
