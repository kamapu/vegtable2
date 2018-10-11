\name{get_precision}
\alias{get_precision}

\title{Get length and precision for numeric variables}
\description{
This function can be used for define data types in databases according to vector
properties (length and precision).
}
\usage{
get_precision(x)
}
\arguments{
\item{x}{A numeric vector.}
}
\details{
Get precision to define numeric characters in databases (e.g. \code{NUMERIC(5,3)}).
}
\author{
Miguel Alvarez \email{kamapu78@gmail.com}.
}
\examples{
data(iris)
get_precision(iris$Sepal.Length)
}
