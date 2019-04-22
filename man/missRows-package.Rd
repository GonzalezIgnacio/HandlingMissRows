\name{missRows-package}

\encoding{latin1}
\alias{missRows-package}
\alias{missRows}

\docType{package}

\title{
Handling Missing Individuals in Multi-Omics Data Integration
}

\description{
The missRows package implements the MI-MFA method to deal with missing 
individuals ('biological units') in multi-omics data integration. The MI-MFA
method generates multiple imputed datasets from a Multiple Factor Analysis
model, then the yield results are combined in a single consensus solution. 
The package provides functions for estimating coordinates of individuals and
variables, imputing missing individuals, and various diagnostic plots to
inspect the pattern of missingness and visualize the uncertainty due to 
missing values.
}

\details{
    \tabular{ll}{
        Package: \tab missRows\cr
        Type: \tab Package\cr
        Version: \tab 1.0\cr
        Date: \tab 2018-03-19\cr
        License: \tab Artistic-2.0\cr
        Depends: \tab R (>= 3.4)\cr
        Imports: \tab methods, gtools, plyr, ggplot2, stats, grDevices,\cr
                \tab S4Vectors, MultiAssayExperiment
    }
}

\author{
Ignacio Gonz?lez and Valentin Voillet

Maintainer: Ignacio Gonz?lez <ignacio.gonzalez@somewhere.net>
}

\references{
Voillet V., Besse P., Liaubet L., San Cristobal M., Gonz?lez I. (2016).
Handling missing rows in multi-omics data integration:
Multiple Imputation in Multiple Factor Analysis framework.
\emph{BMC Bioinformatics}, 17(40).
}

\keyword{package}

\examples{
## A typical MI-MFA session might look like the following. 
## Here we assume there are two data tables with missing rows, 
## "table1" and "table2", and the stratum for each individual 
## is stored in a data frame "df".

\dontrun{

#-- Data preparation
midt <- newMIDTList(table1, table2, colData=df)

#-- Performing MI
midt <- MIMFA(midt, ncomp=2, M=30)

#-- Analysis of the results
plotInd(midt)
plotVar(midt)
}
}
