poly_maker <- function(x){
  sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(x)
    )
  )
}

mcoc <- function(x){sum(x)/length(x)} 

fqi <- function(x){sum(x)/length(x) * sqrt(length(x))}
