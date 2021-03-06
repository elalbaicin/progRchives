% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal_functions.R
\name{extract_ratings}
\alias{extract_ratings}
\title{Extract Each Album Rating}
\usage{
extract_ratings(url_album)
}
\arguments{
\item{url_album}{An album URL, preferrably those extracted with \link{extract_album_data}.}
}
\description{
Scrape number of ratings and weight of each rating as a dataframe (simple ratings have weight 1, reviews have weight 10 and collaborator reviews have weight 20).
}
\examples{
extract_ratings(url_album = "http://www.progarchives.com/album.asp?id=69658")
extract_ratings(url_album = "http://www.progarchives.com/album.asp?id=22272")
}
