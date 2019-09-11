library(data.table)
library(magrittr)
library(ggplot2)
library(metamer)
library(gganimate)
library(ggtext)
# Start with the datasaurus
# install.packages("datasauRus")
library(datasauRus)


# Datasaurio media -----------------
label <- function(x, y) {
   x <- sprintf("%.4f", x)
   x <- strsplit(x, "")[[1]]
   x <- paste0("<span style='color:black'>", 
               paste0(head(x, 4), collapse = ""),
               "</span><span style='color:gray'>",
               paste0(tail(x, -4), collapse = ""),
               "</span>")
   
   y <- sprintf("%.4f", y)
   y <- strsplit(y, "")[[1]]
   y <- paste0("<span style='color:black'>", 
               paste0(head(y, 4), collapse = ""),
               "</span><span style='color:gray'>",
               paste0(tail(y, -4), collapse = ""),
               "</span>")
   
   
   paste0("Media X: ", x, "<br>",
          "Media Y: ", y)
}

datasets <- unique(datasaurus_dozen$dataset)
datasets <- datasets[c(2:length(datasets), 1)]

trunc_means <- delayed_with(trunc(mean(x), 2),
                            trunc(mean(y), 2))

start <- subset(datasauRus::datasaurus_dozen, dataset == "dino")
start$dataset <- NULL
set.seed(42)
for (d in datasets) {
   end <- subset(datasaurus_dozen, dataset == d)
   end$dataset <- NULL
   
   start <- metamerize(start, 
                       preserve = delayed_with(mean(x), mean(y)),
                       minimize = NULL, 
                       N = 2000, 
                       signif = 4,
                       perturbation = 0.06,
                       trim = 500,
                       name = d) %>% 
      metamerize(preserve = delayed_with(mean(x), mean(y)),
                 minimize = mean_dist_to(end), 
                 N = 80000, 
                 trim = 1600,
                 perturbation = 0.04,
                 signif = 4,
                 name = d)
}



m <- start %>% 
   as.data.frame() %>% 
   as.data.table()

N <- uniqueN(m$.name)

g <- ggplot(m, aes(x, y)) +
   geom_point() +
   geom_richtext(data = function(d) d[, .(label = label(mean(x), mean(y))), by = .metamer],
                 x = 85, y = 90, 
                 # hjust = -0.5,
                 family = hrbrthemes::font_rc,
                 size = 7,
                 aes(label = label)) +
   transition_manual(.metamer)


anim_save("Presentación/dino_media.gif",  g, 
          nframes = N*30/2, fps = 20, 
          width = 500, height = 500)


# Datasaurio mediana  -----------------
label <- function(x, y) {
   x <- sprintf("%.4f", x)
   x <- strsplit(x, "")[[1]]
   x <- paste0("<span style='color:black'>", 
               paste0(head(x, 4), collapse = ""),
               "</span><span style='color:gray'>",
               paste0(tail(x, -4), collapse = ""),
               "</span>")
   
   y <- sprintf("%.4f", y)
   y <- strsplit(y, "")[[1]]
   y <- paste0("<span style='color:black'>", 
               paste0(head(y, 4), collapse = ""),
               "</span><span style='color:gray'>",
               paste0(tail(y, -4), collapse = ""),
               "</span>")
   
   
   paste0("Mediana X: ", x, "<br>",
          "Mediana Y: ", y)
}

start <- subset(datasauRus::datasaurus_dozen, dataset == "dino")
start$dataset <- NULL
set.seed(42)
for (d in datasets) {
   end <- subset(datasaurus_dozen, dataset == d)
   end$dataset <- NULL
   
   start <- metamerize(start, 
                       preserve = delayed_with(median(x), median(y)),
                       minimize = NULL, 
                       N = 20000, 
                       signif = 4,
                       trim = 500, 
                       perturbation = 0.03,
                       name = d) %>% 
      metamerize(preserve = delayed_with(median(x), median(y)),
                 minimize = mean_dist_to(end), 
                 N = 200000, 
                 trim = 1600,
                 signif = 4,
                 perturbation = 0.03,
                 name = d)
}



m <- start %>% 
   as.data.frame() %>% 
   as.data.table()

N <- uniqueN(m$.name)

g <- ggplot(m, aes(x, y)) +
   geom_point() +
   geom_richtext(data = function(d) d[, .(label = label(median(x), median(y))), 
                                      by = .metamer],
                 x = 85, y = 90, 
                 # hjust = -0.5,
                 family = hrbrthemes::font_rc,
                 size = 7,
                 aes(label = label)) +
   transition_manual(.metamer)


anim_save("Presentación/dino_mediana.gif",  g, 
          nframes = N*30/2, fps = 20, 
          width = 400, height = 400)

# Latinr ------------------

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


