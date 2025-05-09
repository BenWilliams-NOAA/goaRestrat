% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_index.R
\name{compare_index}
\alias{compare_index}
\title{Compare computed indices with gap}
\usage{
compare_index(data = NULL, index = NULL)
}
\arguments{
\item{data}{list of data provided by the query_data() function (default = NULL)}

\item{index}{list of computed indices, obtained by running the get_index() function (default = NULL)}
}
\value{
a message stating whether computed indices match, or not
}
\description{
Function that compares between internally computed indices with those produced by the gap program and posted to akfin.
This flags when differences in computed indices and gap indices are, on average across the surveys, greater than 0.1\%.
}
