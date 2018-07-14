\name{pg_delete_concept}
\alias{pg_delete_concept}
\alias{pg_delete_name}

\title{Delete Names or Concepts in PostgreSQL Taxonomic Lists}
\description{
Deletion of synonyms or taxon concepts in a PostgreSQL version of
\code{\linkS4class{taxlist}} objects.
}
\usage{
pg_delete_concept(conn, schema, concept_id)

pg_delete_name(conn, schema, usage_id)
}
\arguments{
\item{conn}{A database connection provided by \code{\link[DBI]{dbConnect}}.}
\item{schema}{Name of the schema containing taxonomic tables.}
\item{concept_id,usage_id}{ID of taxon concept or taxon usage name to be
	deleted.}
}
\details{
Take care of producing backups before starting the manipulation of databases.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
