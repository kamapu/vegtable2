\name{vegtable2tvlist}
\alias{vegtable2tvlist}

\title{Convert 'taxlist' Objects to Turboveg-Format.}
\description{
This is a function converting \code{\link[=taxlist-class]{taxlist}} objects to a
'Turboveg-like' format.
It may be udes as prototype for an eventual export function.
}
\usage{
vegtable2tvlist(taxlist, ecodbase=TRUE)
}
\arguments{
\item{taxlist}{A \code{\link[=taxlist-class]{taxlist}} object.}
\item{ecodbase}{A logical value, whether the slot \code{'taxonTraits'} may be
	exported or not.}
}
\details{
This function basically export the slots \code{taxonNames} and
\code{taxonTraits}, adpats the respective column names to Turboveg, and
generates automatically some fields that are usually not requested in
\code{\link[=taxlist-class]{taxlist}} objects.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\seealso{
\code{\link[=taxlist-class]{taxlist}}, \code{\link{tvsplist}}.
}
\examples{
library(taxlist)
data(conosur_splist)

# Example with the genus Weinmannia
Splist <- subset(conosur_splist, Traits=genus == "Weinmannia")
Splist <- vegtable2tvlist(Splist)

# Without ecodbase
Splist <- subset(conosur_splist, Traits=genus == "Weinmannia")
Splist <- vegtable2tvlist(Splist, ecodbase=FALSE)

# Object with missing traits
Splist <- subset(conosur_splist, Traits=genus == "Weinmannia")
Splist@taxonTraits <- Splist@taxonTraits[,1,drop=FALSE]
Splist <- vegtable2tvlist(Splist)
}
