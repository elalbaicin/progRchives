#' Scrape All Album Data from ProgArchives
#'
#' Extract subgenre, artist and album data from \href{http://www.progarchives.com/}{ProgArchives}
#'
#' @param parallel Logical value; should the function use parallelism when extracting album data? Defaults to TRUE.
#'
#' @import purrr
#' @import future
#' @import furrr
#' @import dplyr
#' @import tidyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' extract_genres()
#' }
scrape_progarchives <- function(parallel = TRUE){
  
  # Extract each subgenre info
  main <- extract_subgenres()
  
  # Extract artist URLs
  main <- main %>% 
    mutate(artist_data = map(.x = url_subgenre,
                             .f = extract_artist_urls)) %>% 
    unnest(cols = c(artist_data))
  
  # Use parallel computing?
  if(parallel){
    
    plan(multisession)
    
    fun <- future_map
    
  } else{
    
    fun <- map
    
  }
  
  # Extract album data
  main <- main %>% 
    mutate(album_data = fun(.x = url_artist,
                            .f = insistently(extract_album_data,
                                             rate = rate_backoff()),
                            .progress = TRUE)) %>% 
    unnest(cols = c(album_data),
           keep_empty = TRUE)
  
  # Reorder columns
  main <- main %>% 
    select(date:url_subgenre,
           artist,
           country, 
           url_artist,
           album, 
           url_album,
           type,
           year,
           everything())
  
  return(main)
  
}