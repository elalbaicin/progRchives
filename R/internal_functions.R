#' Extract All Subgenre URLs
#'
#' Scrape all subgenre names and URLs from \href{http://www.progarchives.com/}{ProgArchives} as a dataframe
#'
#' @import rvest
#' @import dplyr
#' @import lubridate
#'
#' @export
#'
#' @examples
#' extract_subgenres()
extract_subgenres <- function(){
  
  # Load main PA page
  main_page <- read_html("http://www.progarchives.com/")
  
  # Find the nodes that contain subgenre names and URLs
  nodes_subgenres <- main_page %>% 
    html_nodes(xpath = '//*[@id="navGenre"]/ul/li[*]/a')
  
  # Extract name and URL from each subgenre and save the data in a data frame
  subgenres <- tibble(date = today(),
                      subgenre = html_text(x = nodes_subgenres), 
                      url_subgenre = html_attr(x = nodes_subgenres,
                                               name = "href") %>% 
                        paste0("http://www.progarchives.com", .))
  
  return(subgenres)
  
}
#' Extract Artist URL and Country from Subgenre Page
#'
#' Scrape all artist links and countries from a subgenre page as a dataframe
#'
#' @param url_genre A subgenre URL, preferrably those extracted with \link{extract_subgenres}.
#'
#' @import rvest
#' @import dplyr
#' @import lubridate
#' @import janitor
#' @import purrr
#' @import stringr
#'
#' @export
#'
#' @examples
#' extract_artist_urls(url_genre = "http://www.progarchives.com/subgenre.asp?style=12")
#' extract_artist_urls(url_genre = "http://www.progarchives.com/subgenre.asp?style=35")
extract_artist_urls <- function(url_genre){
  
  # Load subgenre page
  genre_page <- read_html(url_genre,
                          encoding = "ISO-8859-1")
  
  # Extract table nodes
  main_nodes <- genre_page %>% 
    html_nodes(xpath = '//*[@id="main"]/div[*]/table[*]')
  
  # Keep only those with a title that contain the term "artist"
  node_artists <- keep(.x = main_nodes,
                       .p = ~ !is.na(html_attr(.x, "title"))) %>% 
    keep(.p = ~ str_detect(string = html_attr(.x, "title"), pattern = "[Aa]rtist"))
  
  # Extract each artist URL and country
  artists <- tibble(url_artist = node_artists %>% 
                      html_nodes(css = "a") %>% 
                      html_attr("href") %>% 
                      paste0("http://www.progarchives.com/", .),
                    country = node_artists %>% 
                      html_table() %>% 
                      pluck(1) %>% 
                      row_to_names(row_number = 1) %>% 
                      pull(Country))
  
  return(artists)
  
}
#' Extract Artist and Album Info from Artist Page
#'
#' Scrape artist name and album data from an artist page as a data frame
#'
#' @param url_artist An artist URL, preferrably those extracted with \link{extract_artist_urls}.
#'
#' @import rvest
#' @import dplyr
#' @import lubridate
#' @import janitor
#' @import purrr
#'
#' @export
#' 
#' @examples
#' extract_album_data(url_artist = "http://www.progarchives.com/artist.asp?id=4398")
#' extract_album_data(url_artist = "http://www.progarchives.com/artist.asp?id=671")
extract_album_data <- function(url_artist){
  
  # Load artist page
  artist_page <- read_html(url_artist,
                           encoding = "ISO-8859-1")
  
  # Artist name
  artist_name <- artist_page %>% 
    html_node(xpath = '//*[@id="main"]/div/div[2]/div[3]') %>%
    html_node(css = "strong") %>% 
    html_text() %>% 
    str_remove(pattern = " biography")
  
  # Table nodes
  table_nodes <- c('//*[@id="main"]/div/div[*]/table[',
                   '//*[@id="main"]/div[*]/table[') %>% 
    map(.f = ~ paste0(.x, 1:5, "]")) %>% 
    unlist()

  # Extract each table node
  nodes_albums <- pmap(.l = list(xpath = table_nodes),
                       .f = html_node,
                       x = artist_page)
  
  # Remove empty nodes
  nodes_albums <- nodes_albums %>% 
    discard(.p = ~ is.na(as.character(.x)))
  
  # Count elements in each node
  td_count <- nodes_albums %>% 
    map(.f = html_nodes,
        xpath = "td") %>% 
    map_int(length)
  
  # If there is no album, interrupt the function
  if(all(td_count == 0)){
    
    # Crie um dataframe com o nome do artista
    albums_df <- tibble(artist = artist_name)
    
    # Avise que o artista não tem discos registrados
    warning(paste0("No albums found for this artist. See ", url_artist))
    
    # Entregue o dataframe simplificado
    return(albums_df)
    
  }
  
  # Album types
  album_types <- list("Studio Album",
                      "Live",
                      "DVD/Video",
                      "Boxset/Compilation",
                      "Singles/EPs/Fan Club/Promo")
  
  # Album metadata
  album_meta <- tibble(album = nodes_albums %>% 
                         map(.f = ~ html_nodes(.x,
                                               css = "strong") %>% 
                               html_text) %>% 
                         unlist(),
                       url_album = nodes_albums %>% 
                         map(.f = ~ html_nodes(.x,
                                               css = "a:nth-child(1)") %>% 
                               html_attr("href")) %>% 
                         unlist() %>% 
                         paste0("http://www.progarchives.com/", .))
  
  # Extract type, year, number of ratings and average rating
  album_data <- nodes_albums %>% 
    map2_df(.y = album_types,
            .f = ~ html_nodes(.x, css = "span") %>% 
              html_text() %>% 
              as.numeric() %>% 
              na.omit() %>% 
              matrix(nrow = length(.) / 3,
                     ncol = 3,
                     byrow = TRUE) %>% 
              as_tibble(.name_repair = "minimal") %>% 
              set_names(nm = c("avg_rating",
                               "n_ratings",
                               "year")) %>% 
              mutate(type = .y))
  
  # Final dataframe
  album_df <- bind_cols(album_meta,
                        album_data) %>% 
    mutate(artist = artist_name,
           avg_rating = ifelse(avg_rating == 0, 
                               yes = NA, 
                               no = avg_rating)) %>% 
    select(artist,
           url_album,
           album,
           year,
           type,
           everything())
  
  return(album_df)
  
}
#' Extract Each Album Rating
#'
#' Scrape number of ratings and weight of each rating as a dataframe (simple ratings have weight 1, reviews have weight 10 and collaborator reviews have weight 20).
#'
#' @param url_album An album URL, preferrably those extracted with \link{extract_album_data}.
#'
#' @import rvest
#' @import dplyr
#' @import lubridate
#' @import janitor
#' @import purrr
#'
#' @export
#'
#' @examples
#' extract_ratings(url_album = "http://www.progarchives.com/album.asp?id=69658")
#' extract_ratings(url_album = "http://www.progarchives.com/album.asp?id=22272")
extract_ratings <- function(url_album){
  
  # Return NULL if the album URL is unavailable
  if(is.na(url_album)){
    
    return(NULL)
    
  }
  
  # Load the ratings page
  review_page <- url_album %>%
    str_replace(pattern = "album",
                replacement = "album-reviews") %>% 
    read_html(encoding = "ISO-8859-1")
  
  # Ratings table
  review_table <- review_page %>% 
    html_nodes(xpath = '//*[@id="main"]/div[2]/div[2]')
  
  # Reviews (ratings with reviews)
  reviews <- tibble(collaborator = review_table %>% 
                      html_nodes(xpath = '//*[@id="main"]/div[2]/div[2]/div[*]/div[1]') %>% 
                      html_text() %>% 
                      str_remove_all(pattern = "(?:^\\n{1,100}|\\n{1,100}$)") %>% 
                      str_split(pattern = "\n") %>% 
                      map(.f = str_remove_all, 
                          pattern = "\\r") %>% 
                      map(.f = str_subset,
                          pattern = ".") %>% 
                      map_lgl(.f = ~ length(.x) > 1),
                    stars = review_table %>% 
                      html_nodes(xpath = '//*[@id="main"]/div[2]/div[2]/div[*]/div[2]/img') %>% 
                      html_attr("alt") %>% 
                      str_remove(pattern = " stars") %>% 
                      as.numeric()) %>% 
    mutate(weight = ifelse(collaborator,
                           yes = 20,
                           no = 10))
  
  # Simple ratings
  ratings <- tibble(stars = review_page %>% 
                      html_nodes(xpath = '//*[@id="main"]/div[2]/div[2]/ul/li[*]/img') %>% 
                      html_attr("alt") %>% 
                      str_remove(pattern = " stars") %>% 
                      as.numeric()) %>% 
    mutate(weight = 1)
  
  # Bind reviews and ratings
  info <- bind_rows(reviews,
                    ratings) %>% 
    count(collaborator,
          weight,
          stars,
          name = "count")
  
  # Entregue o resultado
  return(info)
  
}