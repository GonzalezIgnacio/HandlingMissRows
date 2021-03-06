\name{searchsComb}

\encoding{latin1}
\alias{searchsComb}

\title{
Search the Element of a Combination
}

\description{
Search the element of a combination from all combinations
of the supplied vectors as created by
\code{\link{expand.grid}} without creating the combinations.
This function is internally called by \code{\link{MIMFA}}
and is not usually called directly by a user.
}

\usage{
searchsComb(args, idx)
}

\arguments{
    \item{args}{a list containing the vector sources for combinations.}

    \item{idx}{the index of the searched combination.}
}

\value{
The combination corresponding to \code{idx}.
}

\author{
Ignacio Gonz�lez
}

\keyword{utilities}
