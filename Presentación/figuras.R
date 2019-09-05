library(data.table)
library(magrittr)
library(ggplot2)
library(metamer)
library(gganimate)

# Start with the datasaurus
# install.packages("datasauRus")
start <- subset(datasauRus::datasaurus_dozen, dataset == "dino")
start$dataset <- NULL

# And we want to preserve means and correlation
mean_cor <- delayed_with(mean(x), mean(y), cor(x, y)) 
N <- 20000
set.seed(42) # To make results reproducible
metamers <- metamerize(start, preserve = mean_cor, N = N)
print(metamers)



start_data <- subset(datasauRus::datasaurus_dozen, dataset == "dino")
start_data$dataset <- NULL

# smiley <- draw_data(start_data)
# simley$.group <- NULL

to_latlon <- function(data) {
   
   list <- lapply(seq_len(nrow(data)), function(i) {
      sf::st_coordinates(data[i, ]$geometry)[, 1:2] 
   })
   
   data <- as.data.frame(do.call(rbind, list))
   colnames(data) <-c("lon", "lat")
   data
}


latinr <- rnaturalearth::ne_coastline(returnclass = "sf") %>% 
   sf::st_crop(c(xmin = -120, xmax = -30, ymin = -60, ymax = 30)) %>% 
   to_latlon() %>% 
   as.data.table() %>% 
   .[, x := scales::rescale(lon, range(start_data$x))] %>% 
   .[, y := scales::rescale(lat, range(start_data$y))] %>% 
   .[, `:=`(lon = NULL, lat = NULL)] %>% 
   .[]



ggplot(latinr, aes(x, y)) +
   geom_point() +
   geom_point(data = start_data)

X <- subset(datasauRus::datasaurus_dozen, dataset == "x_shape")
X$dataset <- NULL

star <- subset(datasauRus::datasaurus_dozen, dataset == "star")
star$dataset <- NULL


which_trim <- function(N) {
   
   floor(sqrt(1:N))
}

metamers <- metamerize(latinr, 
                       preserve = delayed_with(mean(x), mean(y), cor(x, y)),
                       # minimize = mean_dist_to(latinr), 
                       perturbation = 0.08,
                       N = 5000,
                       trim = 50) %>% 
   metamerize(minimize = mean_dist_to(start_data),
              perturbation = 0.08,
              N = 500000,
              trim = 500) %>% 
   metamerize(minimize = mean_dist_to(latinr),
              perturbation = 0.08,
              N = 80000,
              trim = 500)

values <- lapply(metamers, delayed_with(mean_x = mean(x), mean_y = mean(y), cor = cor(x, y))) %>% 
   lapply(as.data.table) %>% 
   lapply(transpose) %>% 
   rbindlist(idcol = ".metamer") %>% 
   setnames(paste0("V", 1:3), c("mean_x", "mean_y", "cor"))

metamers %>% 
   as.data.frame() %>% 
   as.data.table() %>% 
   
   ggplot(aes(x, y)) +
   geom_point() +
   geom_text(data = values, 
             x = 100, y = 100,
             vjust = 1, hjust = 1,
             aes(label = paste0("mx    = ", signif(mean_x, 2), "\n",
                                "my     = ", signif(mean_y, 2), "\n",
                                "cor = ", signif(cor, 2)))) +
   transition_manual(.metamer)


metamers[[1]] %>% 
   ggplot(aes(x, y)) +
   geom_point() +
   geom_text(data = values[1, ], 
             x = 100, y = 100,
             vjust = 1, hjust = 1,
             aes(label = paste0("mx    = ", signif(mean_x, 4), "\n",
                                "my     = ", signif(mean_y, 4), "\n",
                                "cor = ", signif(cor, 4))))

             