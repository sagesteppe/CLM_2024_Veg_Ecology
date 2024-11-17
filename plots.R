library(tidyverse)
library(sf)
library(patchwork)
library(RColorBrewer)
library(cowplot)
setwd('~/Documents/assoRted/CLM_2024_Veg_Ecology')

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
  ) + 
  theme(text = element_text(color = 'white'))

ggsave('./images/ModifiedWhittaker.png', height = 7, width = 4, units = 'in')

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
  labs(x = NULL, y = NULL, title = 'Nested Quadrats') + 
  theme(
    plot.title = element_text(hjust = 0.5),
  ) +
  theme(text = element_text(color = 'white'))


ggsave('./images/nested.png')


################################################################################
###########               create nested quadrat plots              #############      
################################################################################

rw1 <- st_multilinestring(
  list(matrix(c(0, 0.25, 0.25, 0.25), nrow = 2, byrow = T), 
       matrix(c(0.25, 0, 0.25, 0.25), nrow = 2, byrow = T)))
rw2 <- st_multilinestring(
  list(matrix(c(0, 0.5, 0.5, 0.5), nrow = 2, byrow = T), 
       matrix(c(0.5, 0, 0.5, 0.5), nrow = 2, byrow = T)))
rw3 <- st_multilinestring(
  list(matrix(c(0, 1, 0.5, 1), nrow = 2, byrow = T), 
       matrix(c(0.5, 1, 0.5, 0.5), nrow = 2, byrow = T)))
rw4 <- st_multilinestring(
  list(matrix(c(0.5, 1, 1, 1), nrow = 2, byrow = T), 
       matrix(c(1, 1, 1, 0), nrow = 2, byrow = T)))

fake_legend <- data.frame(
  In = c('1/16', '1/4', '1/2', '1'),
  x = rep(-5, times = 4), 
  y = rep(-5, times = 4)
) |>
  sf::st_as_sf(coords = c('x', 'y'))

ggplot()  + 
  geom_sf(data = nest4, fill = '#8cece2') + 
  geom_sf(data = nest3, fill = '#40e0d0') + 
  geom_sf(data = nest2, fill = '#33b3a6') + 
  geom_sf(data = nest1, fill = '#26867c') + 
  geom_sf(data = rw1, color = '#ffbc42', lwd = 1.5) + 
  geom_sf(data = rw2, color = '#ffbc42', lty = 2, lwd = 1.5) + 
  geom_sf(data = rw3, color = '#d81159', lwd = 1.5) + 
  geom_sf(data = rw4, color = '#d81159', lty = 2, lwd = 1.5) + 
  geom_sf_label(data = labels, aes(label = labs_measure)) + 
  geom_sf(data = fake_legend, aes(color = In)) + 
  coord_sf(xlim = c(0, 1), ylim = c(0, 1)) + 
  theme_minimal() + 
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 1), 
    labels = c('0m', '0.25m', '0.50m', '1.0m')) + 
  scale_x_continuous(
    breaks= c(0, 0.25, 0.5, 1), 
    labels = c('0m', '0.25m', '0.50m', '1.0m')) + 
  labs(x = NULL, y = NULL, title = 'Nested Quadrats') + 
  theme(
    plot.title = element_text(hjust = 0.5),
  ) + 
  scale_color_manual(
    values = c('#ffbc42', '#ffbc42', '#d81159', '#d81159'), 
    labels = c('1/16', '1/4', '1/2', '1')
  )+
  theme(text = element_text(color = 'white'))


 ggsave('./images/Nested-inOut.png')

################################################################################
################                OUR QUADRAT                  ###################
################################################################################

richness_lab <- data.frame(
  x = c(0.5), 
  y = c(1.5),
  labs_name = c('Species\nRichness')
) |> 
  sf::st_as_sf(coords = c(x = 'x', y = 'y'))

ggplot()  + 
  geom_sf(data = quadrat, fill = '#e66d2e') + 
  geom_sf(data = nest4, fill = '#8cece2') + 
  geom_sf(data = nest3, fill = '#40e0d0') + 
  geom_sf(data = nest2, fill = '#33b3a6') + 
  geom_sf(data = nest1, fill = '#26867c') +
  geom_sf_label(data = labels, aes(label = labs_name)) + 
  geom_sf_label(data = richness_lab, aes(label = labs_name)) + 
  theme_minimal() + 
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 1, 2), 
    labels = c('0m', '0.25m', '0.50m', '1.00m', '2.00m')) + 
  scale_x_continuous(
    breaks= c(0, 0.25, 0.5, 1), 
    labels = c('0m', '0.25m', '0.50m', '1.00m')) + 
  labs(x = NULL, y = NULL, title = 'Quadrat for\nField Activity') + 
  theme(
    plot.title = element_text(hjust = 0.5),
  )  + 
  theme(text = element_text(color = 'white'))

ggsave('./images/FieldActivityQuad.png', width = 4, height = 4.5, units = 'in')

rm(nest1, nest2, nest3, nest4, quadrat, labels, nested, richness_lab)
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
    labels = c('0m', '0.2m', '0.4m', '0.6m', '0.8m', '1m'))  + 
  theme(text = element_text(color = 'white'))

ggsave('./images/StandardQuadrat.png')
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
    )   + 
  theme(text = element_text(color = 'white'))

coverclasses <- data.frame(
  Class = c(1:6),
  Cover = c('0-5%', '5-25%', '25-50%', '50-75%', '75-95%', '95-100%')
)

dobby <- p + 
  gridExtra::tableGrob(coverclasses, rows = NULL) 

ggsave('./images/Daubenmire.png', dobby, width = 4, height = 6)

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
  )   + 
  theme(text = element_text(color = 'white'))

ggsave('./images/BeltTransect.png')

rm(transect_center, belt)

################################################################################
###################               DIVERSITY               ######################
################################################################################


########### clumped distribution ########

## very clumped
very_clumped <- data.frame(
  x = rnorm(n = 10, mean = 15, sd = 5),
  y = rnorm(n = 10, mean = 25, sd = 5), 
  taxon = 'Glomeratus ipsum' 
)

# sparsely clumped 
sparsely_clumped <- data.frame(
  x = rnorm(n = 10, mean = 75, sd = 10),
  y = rnorm(n = 10, mean = 85, sd = 5), 
  taxon = 'Glomeratus sparsum' 
)

# several clumps  
several_clumps <- data.frame(
  x = c( 
    rnorm(n = 10, mean = 20, sd = 15),
    rnorm(n = 5, mean = 40, sd = 5), 
    rnorm(n = 10, mean = 80, sd = 5)
  ), 
  y = c(
    rnorm(n = 10, mean = 85, sd = 5), 
    rnorm(n = 5, mean = 40, sd = 5), 
    rnorm(n = 10, mean = 20, sd = 5)
  ), 
  taxon = 'Glomeratus multi' 
)


############# regularly dispersed plant species #########################

######### abundant ##########
reg_abundant <- expand.grid(
  x = seq(from = 1, to = 100, by = 12), 
  y = seq(from = 1, to = 100, by = 12),
  taxon = 'Regularis abundat'
) 
reg_abundant[,1:2] <- apply(reg_abundant[,1:2], MARGIN = 2, FUN = jitter, amount = 2)


##### not abundant  #########3
reg_not_ab <- expand.grid(
  x = seq(from = 5, to = 100, by = 27), 
  y = seq(from = 5, to = 100, by = 27),
  taxon = 'Regularis sparsum'
) 
reg_not_ab[,1:2] <- apply(reg_not_ab[,1:2], MARGIN = 2, FUN = jitter, amount = 5)


### species restricted to remnant habitat in the center of the site 
interior <- data.frame(
  x = rnorm(n = 10, mean = 50, sd = 15),
  y = rnorm(n = 10, mean = 50, sd = 15), 
  taxon = 'Qualitas interior'
)


### randomly distributed 
random <- data.frame(
  round(randu[sample(1:400, size = 15),1:2] * 100),
  taxon = 'Qualitas rarus'
)

edge <- data.frame(
  x = ceiling(dpois(5:30, lambda = 10)  * 100),
  y = sample(1:100, size = 26), 
  taxon = 'Qualitus latus'
)
plot(x = edge$x, y = edge$y, xlim = c(0, 100))

# put them together
species <- rbind(reg_abundant, reg_not_ab, several_clumps, sparsely_clumped, 
               very_clumped, interior, random, edge) 

rm(reg_abundant, reg_not_ab, several_clumps, sparsely_clumped, 
   very_clumped, interior, random, edge)

shapes <- setNames(
  c(21, 21, 22, 22, 22, 23, 23, 23), 
  as.character(unique(species$taxon))
)
dark8 <- setNames(
  brewer.pal(n = 8, 'Dark2'), 
  as.character(unique(species$taxon))
)

## create a first site with 4 species


# legend here. 
p <- ggplot(species, aes(fill = taxon, color = taxon, shape = taxon, x = x, y = y), size = 2) + 
  geom_jitter() + 
  scale_shape_manual(values = shapes, drop = FALSE) + 
  scale_fill_manual(values = dark8, drop = FALSE) + 
  scale_color_manual(values = dark8, drop = FALSE) + 
  theme_minimal() + 
  labs(x = NULL, y = NULL) + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(hjust = 0.5),
    legend.direction ="horizontal"
  ) + 
  guides(
    fill = guide_legend(ncol = 3),
    ) + 
  theme(text = element_text(color = 'white'))

legend <- cowplot::get_legend(p)
rm(p)

site_a <- filter(species, 
                 !taxon %in% c("Qualitas interior",  "Glomeratus sparsum", 
                                       "Qualitas rarus", "Regularis sparsum"))
plot_a <- ggplot(site_a) + 
  geom_jitter(aes(fill = taxon, color = taxon, shape = taxon, x = x, y = y), size = 2) +
  scale_shape_manual(values = shapes, drop = FALSE) + 
  scale_fill_manual(values = dark8, drop = FALSE) + 
  scale_color_manual(values = dark8, drop = FALSE) + 
  
  theme_minimal() + 
  labs(
    x = NULL, 
    y = NULL, 
    title = 'Site A', 
    subtitle = paste0('\u03b1 = ', length(unique(site_a$taxon)))) + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.position = 'none'
  )+ 
  theme(text = element_text(color = 'white'))

## now create a third site 
site_b <- filter(species, 
                 !taxon %in% c('Glomeratus multi', 'Glomeratus ipsum', 'Qualitus latus'))

plot_b <- ggplot(site_b) + 
  geom_jitter(aes(fill = taxon, color = taxon, shape = taxon, x = x, y = y), size = 2) +
  scale_shape_manual(values = shapes, drop = FALSE) + 
  scale_fill_manual(values = dark8, drop = FALSE) + 
  scale_color_manual(values = dark8, drop = FALSE) + 
  
  theme_minimal() + 
  labs(
    x = NULL, 
    y = NULL, 
    title = 'Site B', 
    subtitle = paste0('\u03b1 = ', length(unique(site_b$taxon)))) + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.position = 'none'
  )+ 
  theme(text = element_text(color = 'white'))

site_c <- filter(species, !taxon %in% c('Glomeratus ipsum', 'Regularis sparsum'))

plot_c <- ggplot(site_c) +
  
  geom_jitter(
    aes(fill = taxon, color = taxon, shape = taxon, x = x, y = y), size = 2) +
  scale_shape_manual(values = shapes, drop = FALSE) + 
  scale_fill_manual(values = dark8, drop = FALSE) + 
  scale_color_manual(values = dark8, drop = FALSE) + 
  
  theme_minimal() + 
  labs(
    x = NULL, 
    y = NULL, 
    title = 'Site C', 
    subtitle = paste0('\u03b1 = ', length(unique(site_c$taxon))))  + 
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.position = 'none'
  )+ 
  theme(text = element_text(color = 'white'))

plot_row <- plot_grid(plot_a, plot_b, plot_c, ncol = 3)
plot_row <- plot_grid(plot_row, legend, ncol = 1, rel_heights = c(1, 0.2))

spp_by_site <- bind_rows(
  cbind(site_a, Site = 'A'), 
  cbind(site_b, Site = 'B'), 
  cbind(site_c, Site = 'C') 
) %>% 
  distinct(Site, taxon)

title <- ggdraw() +  # create a shared title for the plot with three Sites. 
  draw_label(
    paste0(
      "\u03b3-diversity (", 
      length(unique(spp_by_site$taxon)) ,
      " taxa) of an area with three major sites"),
    fontface = 'bold',
    x = 0,
    hjust = 0.0, 
    color = 'white'
  ) +
  theme( # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

diversity_plot <- plot_grid(
  title, plot_row,
  ncol = 1,
  rel_heights = c(0.1, 1)
) 

rm(dark8, shapes, plot_a, plot_b, plot_c)


##### calculate the beta-diversity comparisions between the sites ## 
spp_by_site <- bind_rows(
  cbind(site_a, Site = 'A'), cbind(site_b, Site = 'B'), cbind(site_c, Site = 'C')
) %>% 
  distinct(Site, taxon) %>% 
  mutate(Values = 1) %>% 
  pivot_wider(names_from = taxon, values_from = Values) %>% 
  column_to_rownames('Site') 

spp_by_site <- apply(spp_by_site, MARGIN = 2, FUN = function(x){ifelse(is.na(x), 0, 1)})

rm(dark8, shapes)

beta_d_results <- vegan::betadiver(spp_by_site, 'w')

beta_distances <- data.frame(
  row.names = c('B', 'C'), 
  A = c(0.777, 0.4), 
  B = c('', 0.2727)
)

########## create the beta diversity table for site wise comparisons 
beta_d_tab <- gridExtra::tableGrob(beta_distances) 

DiversityGrid <- plot_grid(
  diversity_plot, beta_d_tab,
  ncol = 2,
  rel_widths = c(1, 0.3)
)


save_plot(filename = './images/Diversity_Plot.png', plot = DiversityGrid, 
          base_width = 8)

rm(site_a, site_b, site_c, dark8, shapes, legend, beta_d_tab, beta_distances,
   plot_row, spp_by_site, beta_d_results, title, dark8, shapes)


################################################################################
##############                  artificial landscape        ####################
################################################################################

lacustrine <- data.frame(
  x = round(rnorm(n = 20, mean = 15, sd = 8)),
  y = round(rnorm(n = 20, mean = 25, sd = 8)) 
) %>% 
  st_as_sf(coords = c('x', 'y')) %>% 
  st_union() %>% f
  st_convex_hull()

rivers <- data.frame(
  x = c(15, 12, 15, 11, 17, 20, 13, 
        25, 40, 45, 58, 68, 75, 85),
  y = c(25, 40, 45, 58, 68, 75, 85, 
        15, 12, 15, 11, 17, 20, 13)
) %>% 
  sf::st_as_sf(coords = c('x', 'y'))
  
# use spdep to identify nearest neighbor, draw lines between these neighbors, 
spdep::knearneigh(rivers, k = 2)  

# now smooth the lines using rmapshaper so that they look more normal. 

ggplot(lacustrine) + 
  geom_sf() + 
  geom_sf(data = rivers) + 
  coord_sf(xlim = c(0, 100), ylim = c(0, 100)) 

## now we generate two hills, in the grid, these start where the rivers start. ##
## we'll use these values as geom_tile values

hills <- data.frame(
  x = c(20, 14, 10, 75, 85, 87, 95, 40, 30),
  y = c(95, 85, 90, 10, 10, 30, 15, 80, 40), 
  z = c(20, 21, 20, 18, 17,  16, 15,  0,  0)
) 

ggplot() + 
  geom_raster(data = hills, aes(x = x, y = y, fill = z), interpolate = T) +
  geom_sf(data = lacustrine, fill = 'darkblue') + 
  geom_sf(data = rivers, color = 'darkblue') + 
  coord_sf(xlim = c(0, 100), ylim = c(0, 100)) +
  geom_point(data = very_clumped, aes(x = x, y = y), color = 'green') + 
  
  # change labels from 0-100 to 0-1000 to make scenario/ plot sizes more plausible 
  theme_minimal() 


### plants around wetland
banks <- data.frame( 
  x = rnorm(n = 30, mean = 15, sd = 12), 
  y = rnorm(n = 30, mean = 25, sd = 12), 
  taxon = 'wetland' 
) |> 
  st_as_sf(coords = c(x = 'x', y = 'y'))



## use the edge species again 
edge <- data.frame(
  x = ceiling(dpois(5:30, lambda = 10) * 100),
  y = sample(1:100, size = 26), 
  taxon = 'Qualitus latus'
)

# get it a bit closer to the river  
edge <- edge %>% 
  rowwise() %>% 
  mutate(x = if_else(x < 10, x + sample(5:10, 1), x))
plot(x = edge$x, y = edge$y, xlim = c(0, 100)) 

# now flip it to the other side 
edge_flipped <- st_as_sf(edge, coords = c('y', 'x'))
edge <- st_as_sf(edge, coords = c('x', 'y'))


########### add a regulary distributed plant to the plains 
reg_abundant <- expand.grid(
  x = seq(from = 25, to = 100, by = 12), 
  y = seq(from = 25, to = 100, by = 12),
  taxon = 'Regularis abundat'
) 
reg_abundant[,1:2] <- apply(reg_abundant[,1:2], MARGIN = 2, FUN = jitter, amount = 2) 
reg_abundant <- st_as_sf(reg_abundant, coords = c('x',  'y'))

reg_not_ab <- expand.grid(
  x = seq(from = 25, to = 100, by = 27), 
  y = seq(from = 25, to = 100, by = 27),
  taxon = 'Regularis sparsum'
) 
reg_not_ab[,1:2] <- apply(reg_not_ab[,1:2], MARGIN = 2, FUN = jitter, amount = 5)
reg_not_ab <- st_as_sf(reg_not_ab, coords = c('x',  'y'))


############### several clumps ###################

# several clumps  
several_clumps <- data.frame(
  x = c( 
    rnorm(n = 10, mean = 20, sd = 7),
    rnorm(n = 10, mean = 40, sd = 5), 
    rnorm(n = 10, mean = 80, sd = 5)
  ), 
  y = c(
    rnorm(n = 10, mean = 85, sd = 5), 
    rnorm(n = 10, mean = 40, sd = 5), 
    rnorm(n = 10, mean = 75, sd = 5)
  ), 
  taxon = 'Glomeratus multi' 
) %>% 
  st_as_sf(coords = c('x', 'y'))


# remove any intersections of the bodies of water to the plants, we will 
# ignore any emergent plants. 

edge <- edge [ lengths(sf::st_intersects(edge, lacustrine)) < 1, ]
banks <-  banks [ lengths(sf::st_intersects(banks, lacustrine)) < 1, ]
edge_flipped <- edge_flipped [ lengths(sf::st_intersects(edge_flipped, lacustrine)) < 1, ]
reg_abundant <- reg_abundant [ lengths(sf::st_intersects(reg_abundant, lacustrine)) < 1, ]
reg_not_ab <- reg_not_ab [ lengths(sf::st_intersects(reg_not_ab, lacustrine)) < 1, ]

several_clumps <- several_clumps[lengths(sf::st_intersects(several_clumps, lacustrine)) < 1, ]


ggplot() + 
  geom_raster(data = hills, aes(x = x, y = y, fill = z), interpolate = T) + 
  geom_sf(data = lacustrine, fill = 'darkblue') + 
  geom_sf(data = rivers, color = 'darkblue') + 
  geom_sf(data = banks, color = 'green') + 
  geom_sf(data = edge_flipped, color = 'orange') + 
  geom_sf(data = edge, color = 'red') +  
  geom_sf(data = reg_abundant, color = 'black') +  
  geom_sf(data= several_clumps, color = 'pink') + 
  
  coord_sf(xlim = c(0, 100), ylim = c(0, 100)) + 
  theme_minimal()
