\name{get_description}
\docType{methods}
\alias{get_description}
\alias{get_description,PostgreSQLConnection-method}

\title{Get descriptions of table columns from Postgres tables}
\description{
Descriptions of variables stored in PostgreSQL tables.
}
\usage{
\S4method{get_description}{PostgreSQLConnection}(conn, ...)
}
\arguments{
\item{conn}{A \code{\linkS4class{PostgreSQLConnection}} object.}
\item{...}{Further arguments passed among methods.}
}
\details{
This function produces a data frame containing information on schemas, tables
and descriptions (i.e. comments) for every single column in tables stored at a
PostgreSQL dataframe. 
}
\value{
An object of class \code{data.frame}.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}.
}
