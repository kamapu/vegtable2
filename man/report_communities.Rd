\name{report_communities}
\docType{methods}
\alias{report_communities}
\alias{report_communities,vegtable-method}

\title{Summary on Plant Communities stored in vegtable Objects}
\description{
This function produces a report on plant communities stored in an object of
class \code{\linkS4class{vegtable}}.
The function is adapted to our own databases and may not work in other data
sets.
}
\usage{
\S4method{report_communities}{vegtable}(veg, bib, filename, title, author,
	date, date_format="\%d-\%m-\%Y", keep_rmd=TRUE, biblio_style="abbrvnat", ...)
}
\arguments{
\item{veg}{An object of class \code{\linkS4class{vegtable}}.}
\item{bib}{Name of bibliography file (.bib) as chararcter value.}
\item{filename}{A character value indicating the name of the resulting file
	without extension.}
\item{title,author}{Character values used in the title page.}
\item{date}{A value of class \code{\link{Date}} to be inserted in the title
	page.}
\item{date_format}{Character value indicating the format used for displaying the
	date in the title page.}
\item{keep_rmd}{Logical value, whether the resulting Rmd file should be deleted
	after rendering or not.}
\item{biblio_style}{Character value indicating the name of the bibliography
	style in the package 'natbib'.}
\item{...}{Further arguments passed among methods.}
}
\details{
This function generates a quick overview of references and communities stored in
a data set formatted as \code{\linkS4class{vegtable}} object.
}
\value{
A PDF file.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\keyword{methods}
