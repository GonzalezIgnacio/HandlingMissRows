\name{missPattern}

\encoding{latin1}
\alias{missPattern}
\alias{print.missPattern}

\title{
Inspects Pattern of Missingness
}

\description{
This function inspects and plots the structure of missing individuals
in data tables.
}

\usage{
missPattern(object, colStrata=NULL, colMissing="grey70", cexTitles=12,
            legTitle="Strata", missLab="miss", showPlot=TRUE)

\method{print}{missPattern}(x, \ldots)
}

\arguments{
    \item{object}{an object of class \code{MIDTList}.}

    \item{x}{an object of class inheriting from \code{missPattern}}

    \item{colStrata}{a character vector of the same length than the
        number of strata, containing the color names to be used to
        annotate the individuals per stratum.}

    \item{colMissing}{the fill color for missing individuals.}

    \item{cexTitles}{a positive number. The amount by which table
        titles should be magnified.}

    \item{legTitle}{character. The legend title.}

    \item{missLab}{character. The label legend for missing individuals.}

    \item{showPlot}{logical. Whether the plot will be displayed. 
        Default is \code{TRUE}.}

    \item{\ldots}{not used currently.}
}

\details{
\code{missPattern} calculates the amount of missing/available individuals 
in each stratum per data table and plots a missingness map showing where
missingness occurs. For plotting, tables are arranged in individuals (rows)
\eqn{\times}{x} features (columns). Data tables are plotted separately on 
a same device showing the pattern of missingness. The individuals are colored
according to their stratum whereas missing individuals (rows) are specific
colored (see \code{colMissing}).
}

\value{
A list with the following components:

    \item{nbMissing}{a \code{data.frame} containing the amount of
        missing/available rows in each stratum per data table.}

    \item{isMissing}{a \code{data.frame} containing the indicator
        matrix for the missing rows.}

    \item{ggp}{an object of class \code{ggplot}.}
}

\author{
Ignacio Gonz�lez
}

\examples{
#-- load data and create MIDTList object
data(NCI60)
midt <- MIDTList(NCI60$mae)

#-- inspects pattern of missingness
patt <- missPattern(midt)
patt
}

\keyword{hplot}
\keyword{dplot}
