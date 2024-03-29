% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ValidTaxo.R
\name{ValidTaxo}
\alias{ValidTaxo}
\title{Store results of simulation in CompareSim}
\usage{
ValidTaxo(list_res, test_taxo)
}
\arguments{
\item{list_res}{Results of \emph{SimFullCom} function applied to the
test dataset: a list of data.tables with simulated taxonomic information.}

\item{test_taxo}{The original taxonomic information for the test dataset.}
}
\value{
This function returns a data.table similar to Data inputted in
argument, with two additional columns (TestData and ValidAsso).
}
\description{
This function is used in the \emph{CompareSim} function.
It adds two columns to the dataset if the option \emph{Results_Simulations}
is set to TRUE : \emph{TestData} is a boolean to know if the tree was
in the test dataset, and \emph{ValidAsso} is a boolean to know if the
associated species is the right one.
}
\details{
This function compares the taxonomic information in the column
GenSpCor of the simulated dataset \emph{list_res} with the original information
from the columns Genus and Species of the dataset \emph{test_taxo}. It then
adds two columns to the dataset, specifying if each tree is used in the
test and if the estimated species is the same as the original one.
}
