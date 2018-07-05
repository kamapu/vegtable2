\name{postgres2vegtable}
\alias{postgres2vegtable}
\alias{postgres2taxlist}

\title{Import PostgreSQL databases into vegtable objects}
\description{
Import and adaption of Postgres tables into objects of class
\code{\linkS4class{vegtable}}.
}
\usage{
## Importing taxlist objects
postgres2taxlist(conn, schema)

## Importing vegtable objects
postgres2vegtable(conn, header_schema, species_schema, relations_schema,
	samples_schema, layers_schema, coverconvert_schema, cover_names, geometry,
	description)
}
\arguments{
\item{conn}{A database connection provided by \code{\link[DBI]{dbConnect}}.}
\item{header_schema}{A character value indicating the schema, where the table
	header is located.}
\item{species_schema,schema}{Name of the schema containing taxonomic tables.}
\item{relations_schema}{Name of the schema containing the relations to header.}
\item{samples_schema}{Name of the schema containing the samples table.}
\item{layers_schema}{Name of the schema containing the layers.}
\item{coverconvert_schema}{Name of the schema containing the conversion table.}
\item{cover_names}{Character vector indicating the variables in samples
	representing cover values.}
\item{geometry}{Name of the variable including the geometry.}
\item{description}{A named vector used as metadata for the vegtable object.}
}
\details{
In the case that some schemas are not mentioned, the function assumes such
tables are located in the same schema as the table header.
Thus for databases placed in just one schema, this need to be set only in
argument \code{'header_schema'}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
\dontrun{
library(vegtable2)
conn <- dbConnect()
Data <- postgres2vegtable(conn, header_schema="veg_database",
	cover_names=c("bb_old","bb_new"), geometry="plot_centroids"
	description=c(database="my database", author="me"))
dbDisconnect(conn)
summary(Data)
}
}
