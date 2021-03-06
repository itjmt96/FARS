% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Function to read FARS file.}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A filename as input string.}
}
\value{
File data is read into a dataframe object.
}
\description{
This function uses readr and dplyr packages.
It takes a CSV file as an input filename parameter.
If the file does not exist in the working directory, this function stops with an output message that the file does not exist.
If the file exists, this function reads it into dataframe object.
It suppresses messages and progress bar while reading the file.
}
\examples{
\dontrun{
fars_read("accident_2013.csv.bz2")
fars_read(filename="accident_2013.csv.bz2")
}

}

