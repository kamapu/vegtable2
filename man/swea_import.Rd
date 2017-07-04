\name{swea_import}
\alias{swea_import}

\title{Personalized import routine}
\description{
This function performs a personalized import for the database
\href{http://www.givd.info/ID/AF-00-006}{SWEA-Dataveg}.
This is a wrapper for \code{\link[vegtable]{tv2vegtable}}.
}
\usage{
swea_import(db="Sweadataveg", ...)
}
\arguments{
\item{db}{Name of the database as character value.}
\item{...}{Further arguments passed to \code{\link[vegtable]{tv2vegtable}}.}
}
\details{
This function is adapted to some peculiarities of the database
\href{http://www.givd.info/ID/AF-00-006}{SWEA-Dataveg}, especially regarding
the cover scales implemented in the database as well as cover conversion.

Among other issues, presence absence values will be converted to \dQuote{1} and
\dQuote{\code{NA}} and it will removed from the conversion scale.
The slot \code{'coverconvert'} will be replaced by
\code{\link[vegtable]{braun_blanquet}}.
}
\value{
A \code{\linkS4class{vegtable}} object.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link[vegtable]{tv2vegtable}}, \code{\link[vegtable]{braun_blanquet}}.
}
