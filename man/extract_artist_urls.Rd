% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal_functions.R
\name{extract_artist_urls}
\alias{extract_artist_urls}
\title{Extract Artist URL and Country from Subgenre Page}
\usage{
extract_artist_urls(url_genre)
}
\arguments{
\item{url_genre}{A subgenre URL, preferrably those extracted with \link{extract_subgenres}.}
}
\description{
Scrape all artist links and countries from a subgenre page as a dataframe
}
\examples{
\dontrun{extract_artist_urls(url_genre = "http://www.progarchives.com/subgenre.asp?style=12")}
\dontrun{extract_artist_urls(url_genre = "http://www.progarchives.com/subgenre.asp?style=35")}
}
