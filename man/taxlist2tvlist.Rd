\name{taxlist2tvlist}
\alias{taxlist2tvlist}

\title{Convert 'taxlist' Objects to Turboveg-Format.}
\description{
This is a function converting \code{\link[=taxlist-class]{taxlist}} objects to a
'Turboveg-like' format.
It may be udes as prototype for an eventual export function.
}
\usage{
taxlist2tvlist(taxlist, ecodbase=TRUE)
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
\code{\linkS4class{taxlist}} objects.

Names belonging to a same concept get a common \code{'LETTERCODE'}.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\seealso{
\code{\linkS4class{taxlist}}, \code{\link[taxlist]{tv2taxlist}}.
}
