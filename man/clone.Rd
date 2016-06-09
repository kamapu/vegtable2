\name{clone}
\alias{clone}

\title{Clone of Turboveg database}
\description{
Packing respective files belonging to a Turboveg database into a zip file.
}
\usage{
clone(db_name, tv_home=tv.home(), stamp=TRUE, overwrite=FALSE, extras=FALSE,
	...)
}
\arguments{
\item{db_name}{Name of database as character value.}
\item{tv_home}{Character value indicating the path to Turboveg's home
	directory.}
\item{stamp}{Logical value indicating whether the date should be included in the
	zip file's name or not.}
\item{overwrite}{Logical value indicating whether existing files should be
	overwritten or not.}
\item{extras}{Logical value indicating the occurrence of extra files (see
	Details).}
\item{...}{Additional arguments passed to function \code{\link{zip}}.}
}
\details{
This function attempts to substitute the backup options from \bold{Turboveg},
generating a backup of the whole database at once.

An unpack function will be provided

For \code{stamp=TRUE}, the system date will be included in the name of the
resulting zip file.

If \code{overwrite=FALSE} overwriting homonymous zip files will be avoided by
inserting a numerical suffix to the resulting zip file.
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}.
}
\examples{
## Add example for this function
}
