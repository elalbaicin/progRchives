#' ProgArchives Data from 2021-10-18
#'
#' A dataset containing album data by nearly 12,000 Progressive Rock artists.
#'
#' @format A data frame with 66507 rows and 12 variables:
#' \describe{
#'   \item{date}{Date of extraction (2021-10-18)}
#'   \item{subgenre}{Subgenre name}
#'   \item{url_subgenre}{Subgenre URL}
#'   \item{artist}{Band/artist name; different artists might share a name}
#'   \item{country}{Country from which the artist hails}
#'   \item{url_artist}{Artist URL}
#'   \item{album}{Album name; different release might share a name; some artists have no registered release}
#'   \item{url_album}{Album URL}
#'   \item{type}{Type of release: Studio Album, Live, DVD/Video, Boxset/Compilation, Singles/EPs/Fan Club/Promo or NA}
#'   \item{year}{Release year}
#'   \item{avg_rating}{Average rating, ranging from 1.00 to 5.00, including NAs}
#'   \item{n_ratings}{Number of ratings}
#' }
#' @source \url{http://www.progarchives.com/}
"progarchives"