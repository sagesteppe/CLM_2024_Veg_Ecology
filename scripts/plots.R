library(tidyverse)
library(sf)
library(patchwork)
setwd('~/Documents/assoRted/CLM_2024_Veg_Ecology/scripts')

################################################################################
################            modified whittaker                  ################
################################################################################
poly_maker <- function(x){
  sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(x)
    )
  )
}

megaplot <- sf::st_polygon(list(rbind(c(0, 0), c(0, 50), c(20, 50), c(20, 0), c(0, 0))))
center <- sf::st_sfc(st_polygon(list(rbind(c(7.5, 35), c(12.5, 35), c(12.5, 15), c(7.5, 15), c(7.5, 35)))))

mid <- dplyr::bind_rows(
  list(
    poly_maker(list(rbind(c(0, 50), c(2, 50), c(2, 45), c(0, 45), c(0, 50)))), 
    poly_maker(list(rbind(c(18, 0), c(18, 5), c(20, 5), c(20, 0), c(18, 0))))
  )
)

small <- bind_rows(
  list(
    poly_maker(list(rbind(c(7.5, 50), c(9.5, 50), c(9.5, 49.5), c(7.5, 49.5), c(7.5, 50)))), # top
    poly_maker(list(rbind(c(12.5, 0), c(10.5, 0), c(10.5, 0.5), c(12.5, 0.5), c(12.5, 0)))), # bottom
    poly_maker(list(rbind(c(0, 9), c(0, 11), c(0.5, 11), c(0.5, 9), c(0, 9)))), # ll_0.5.2
    poly_maker(list(rbind(c(0, 24), c(0, 26), c(0.5, 26), c(0.5, 24), c(0, 24)))), # ul_0.5.2
    poly_maker(list(rbind(c(20, 15), c(19.5, 15), c(19.5, 17), c(20, 17), c(20, 15)))), # lr_0.5.2
    poly_maker(list(rbind(c(20, 41), c(19.5, 41), c(19.5, 39), c(20, 39), c(20, 41)))), # ur_0.5.2
    poly_maker(list(rbind(c(8, 35), c(8, 35.5), c(10, 35.5), c(10, 35), c(8, 35)))), # center_top
    poly_maker(list(rbind(c(8, 15), c(10, 15), c(10, 14.5), c(8, 14.5), c(8, 15)))), # center_bottom
    poly_maker(list(rbind(c(7.5, 18), c(7, 18), c(7, 20), c(7.5, 20), c(7.5, 18)))), # center_left
    poly_maker(list(rbind(c(12.5, 28), c(12.5, 30), c(13, 30), c(13, 28), c(12.5, 28)))) # center_right
  )
) 

# need a fake legend because we are not mappin to aesthetics. 

fake_legend <- data.frame(
  Plot = c('Small', 'Medium', 'Center', 'Full'),
  Size = c('10 - 2 x 0.5m', '2 - 5 x 2m', '1 - 20 x 5m', '1 - 50 x 20m'), 
  x = rep(-5, times = 4), 
  y = rep(-5, times = 4)
) |>
  sf::st_as_sf(coords = c('x', 'y'))

mw <- ggplot() + 
  geom_sf(data = megaplot, fill = '#CAF7E2') + 
  geom_sf(data = mid, fill = '#58B09C') + 
  geom_sf(data = center, fill = '#386150') + 
  geom_sf(data = small, fill = '#DA2C38') + 
  geom_sf(data = fake_legend, aes(color = Plot)) + 
  coord_sf(xlim = c(0, 20), ylim = c(0,50)) + 
  theme_minimal() + 
  scale_y_continuous(
    breaks = c(0, 15, 35, 50), 
    labels = c('0m', '15m', '35m', '50m')) + 
  scale_x_continuous( 
    breaks= c(0, 7.5, 12.5, 20), 
    labels = c('0m', '7.5m', '12.5m', '20m')) + 
  labs(title = 'Modified Whittaker')  + 
  scale_color_manual(
    values = c('#DA2C38', '#386150', '#58B09C', '#CAF7E2'), 
    labels = c('10 - 2 x 0.5m', '2 - 5 x 2m', '1 - 20 x 5m', '1 - 50 x 20m')
  )

ggsave('../images/ModifiedWhittaker.png')

rm(megaplot, mid, center, small, fake_legend)

################################################################################
###########               create nested quadrat plots              #############      
################################################################################
quadrat <- sf::st_polygon(list(rbind(c(0, 0), c(0, 2), c(1, 2), c(1, 0), c(0, 0)))) #whole quad
nest1 <- sf::st_polygon(list(rbind(c(0, 0), c(0, 0.25), c(0.25, 0.25), c(0.25,0), c(0,0))))
nest2 <- sf::st_polygon(list(rbind(c(0, 0), c(0, 0.5), c(0.5, 0.5), c(0.5,0), c(0,0))))
nest3 <- sf::st_polygon(list(rbind(c(0, 0.5), c(0, 1), c(0.5, 1), c(0.5,0.5), c(0, 0.5))))
nest4 <- sf::st_polygon(list(rbind(c(0.5, 0), c(0.5, 1), c(1, 1), c(1,0), c(0.5, 0))))

labels <- data.frame(
  x = c(0.125, 0.35, 0.25, 0.75), 
  y = c(0.125, 0.35, 0.75, 0.50),
  labs_measure = c('1/16m', '1/4m', '1/2m', '1m'), 
  labs_name = c(1, 2, 3, 4)
) |> 
  sf::st_as_sf(coords = c(x = 'x', y = 'y'))

nested <- ggplot()  + 
  geom_sf(data = nest4, fill = '#8cece2') + 
  geom_sf(data = nest3, fill = '#40e0d0') + 
  geom_sf(data = nest2, fill = '#33b3a6') + 
  geom_sf(data = nest1, fill = '#26867c') +
  geom_sf_label(data = labels, aes(label = labs_measure)) + 
  theme_minimal() + 
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 1), 
    labels = c('0m', '0.25m', '0.50m', '1.0m')) + 
  scale_x_continuous(
    breaks= c(0, 0.25, 0.5, 1), 
    labels = c('0m', '0.25m', '0.50m', '1.0m')) + 
  labs(x = NULL, y = NULL, title = 'Nested Quadrats')

ggsave('../images/nested.png')
rm(nest1, nest2, nest3, nest4, quadrat, labels, nested)

################################################################################
##############           create typical cover plot              ################
################################################################################

plot1m <- sf::st_polygon(list(rbind(c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0))))
gr <- st_make_grid(plot1m, n = c(5, 5), what = 'polygons')

ggplot() + 
  geom_sf(data = plot1m, fill = NA) + 
  geom_sf(data = gr, fill = NA) + 
  theme_minimal() + 
  labs(
    title = 'Quadrat with Grids', 
    subtitle = paste0('each cell is ', 100/length(gr) , '% cover')) + 
  scale_y_continuous(
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
    labels = c('0m', '0.2m', '0.4m', '0.6m', '0.8m', '1m')) + 
  scale_x_continuous( 
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
    labels = c('0m', '0.2m', '0.4m', '0.6m', '0.8m', '1m')) 

ggsave('../images/StandardQuadrat.png')
rm(plot1m, gr)

################################################################################
##############            create daubenmire plots               ################
################################################################################

lt <- st_multilinestring(
  list(
    matrix(c(0, 0.125, 0, 0.25), nrow = 2, byrow = T), 
    matrix(c(0, 0.375, 0, 0.5), nrow = 2, byrow = T), 
    matrix(c(0.2, 0.125, 0.2, 0.25), nrow = 2, byrow = T), 
    matrix(c(0.2, 0.375, 0.2, 0.5), nrow = 2, byrow = T)
  )
)
corner <- st_multilinestring(
  list(
    matrix(c(0.129, 0, 0.2, 0), nrow = 2, byrow = T), 
    matrix(c(0.2, 0, 0.2, 0.071), nrow = 2, byrow = T) 
  )
)

dobby_lower <- sf::st_polygon(
  list(rbind(c(0, 0), c(0, 0.5), c(0.2, 0.5), c(0.2, 0), c(0, 0))))

p <- ggplot() + 
  geom_sf(data = dobby_lower, color = 'black', fill = NA, lwd = 2) + 
  geom_sf(data = lt, color = '#FF206E', lwd = 1.2) + 
  geom_sf(data = corner, color= '#FBFF12', lty = 5, lwd = 1.2) + 
  theme_minimal() + 
  labs(title = 'Daubenmire Frame')  +
  scale_y_continuous(
    breaks = c(0, 0.125, 0.25, 0.375, 0.5), 
    labels = c('0m', '0.125m', '0.25m', '0.375m', '0.5m')) + 
  scale_x_continuous( 
    breaks = c(0, 0.129, 0.2), 
    labels = c('0m', '0.129m', '0.2m')
    ) 

coverclasses <- data.frame(
  Class = c(1:6),
  Cover = c('0-5%', '5-25%', '25-50%', '50-75%', '75-95%', '95-100%')
)

dobby <- p + 
  gridExtra::tableGrob(coverclasses, rows = NULL) 

ggsave('../images/Daubenmire.png', dobby, width = 4, height = 6)

rm(lt, corner, dobby_lower, p, coverclasses, dobby)

###############################################################################
#################               BELT TRANSECT              #####################
################################################################################

transect_center <- st_multilinestring(
  list(
    matrix(c(1, 0, 1, 10), nrow = 2, byrow = T)))

belt <- sf::st_polygon(
  list(rbind(c(0, 0), c(0, 10), c(2, 10), c(2, 0), c(0, 0))))

ggplot() + 
  geom_sf(data = belt, fill = '#F5CAC3') + 
  geom_sf(data = transect_center, color = '#84A59D') + 
  scale_y_continuous(
    breaks = c(0, 5, 10), 
    labels = c('0m', '5m', '10m')) + 
  scale_x_continuous( 
    breaks = c(0, 2), 
    labels = c('0m', '2m')
  ) + 
  theme_minimal() + 
  labs(title = 'Belt\nTransect') +
  theme(
    plot.title = element_text(hjust = 0.5),
  ) 

ggsave('../images/BeltTransect.png')

rm(transect_center, belt)
