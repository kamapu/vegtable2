\name{pg_merge_concepts}
\alias{pg_merge_concepts}

\title{Merge Multiple Concepts into One}
\description{
Different taxon concepts may be merged by taxonomic revisions.
}
\usage{
pg_merge_concepts(conn, schema, concept_id)
}
\arguments{
\item{conn}{A database connection provided by \code{\link[DBI]{dbConnect}}.}
\item{schema}{Name of the schema containing taxonomic tables.}
\item{concept_id}{ID of taxon concepts to be merged.}
}
\details{
All concepts indicated in argument \code{'concept_id'} will be set as synonyms
of the first concept in the vector.

Take care of producing backups before starting the manipulation of databases.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
