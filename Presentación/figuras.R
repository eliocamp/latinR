library(data.table)
library(magrittr)
library(ggplot2)
library(metamer)
library(gganimate)
library(ggtext)
# Start with the datasaurus
# install.packages("datasauRus")
library(datasauRus)

wavelength_to_rgb <- function(wavelength, gamma=0.8){
   
   #
   #    Based on code by Dan Bruton
   #    http://www.physics.sfasu.edu/astro/color/spectra.html
   #    '''
   
   if (wavelength >= 380 & wavelength <= 440) {
      attenuation = 0.3 + 0.7 * (wavelength - 380) / (440 - 380)
      R = ((-(wavelength - 440) / (440 - 380)) * attenuation) ^ gamma
      G = 0.0
      B = (1.0 * attenuation) ^ gamma
   }
   else if (wavelength >= 440 & wavelength <= 490) {
      R = 0.0
      G = ((wavelength - 440) / (490 - 440)) ^ gamma
      B = 1.0
   }
   else if (wavelength >= 490 & wavelength <= 510) {
      R = 0.0
      G = 1.0
      B = (-(wavelength - 510) / (510 - 490)) ^ gamma
   }
   else if (wavelength >= 510 & wavelength <= 580) {
      R = ((wavelength - 510) / (580 - 510)) ^ gamma
      G = 1.0
      B = 0.0
   }
   else if (wavelength >= 580 & wavelength <= 645) {
      R = 1.0
      G = (-(wavelength - 645) / (645 - 580)) ^ gamma
      B = 0.0
   }
   else if (wavelength >= 645 & wavelength <= 750) {
      attenuation = 0.3 + 0.7 * (750 - wavelength) / (750 - 645)
      R = (1.0 * attenuation) ^ gamma
      G = 0.0
      B = 0.0
   }
   else {
      R = 0.0
      G = 0.0
      B = 0.0
   }
   R = R * 255
   G = G * 255
   B = B * 255
   return (rgb(floor(R), floor(G), floor(B), max=255))
}


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
   
   start <- start %>% 
      metamerize(preserve = delayed_with(mean(x), mean(y)),
                 minimize = mean_self_closeness,
                 N = 6000,
                 signif = 4,
                 perturbation = 0.06,
                 trim = 600,
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



# espectro --------

library(colorscience)

plot_spectrum <- function(object) {
   
   which <- dimnames(MaterialReferenceData)[[2]] %in% c("wavelength", object)
   
   data <- MaterialReferenceData[, which] %>% 
      as.data.table() 
   colnames(data) <- c("wavelength", "value")
   
   data[wavelength %between% c(380, 700)] %>% 
      .[, color := wavelength_to_rgb(wavelength), by = wavelength] %>% 
      ggplot(aes(wavelength, value)) +
      geom_line(aes(color = color, group = 1), size = 2) +
      # geom_col(aes(fill = color)) +
      scale_x_continuous("Longitud de onda [nm]", limits = c(NA, 700)) +
      scale_y_continuous("Espectro") +
      scale_color_identity() +
      scale_fill_identity()
}




plot_spectrum("Blue")

plot_spectrum("YellowBanana")



# spfit --------
library(spfit)


fit <- sp_fit(cars$dist)

as.character(fit)

ggplot(cars, aes(speed, dist)) +
   geom_point() +
   geom_point(aes(y = predict(sp_fit(dist))), 
              color = "red") +
   geom_point(aes(y = predict(change_digit(sp_fit(dist)))), 
              color = "blue")




# Latinr ------------------
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

to_latlon <- function(data) {
   
   list <- lapply(seq_len(nrow(data)), function(i) {
      sf::st_coordinates(data[i, ]$geometry)[, 1:2] 
   })
   
   data <- as.data.frame(do.call(rbind, list))
   colnames(data) <-c("lon", "lat")
   data
}

start_data <- as.data.table(datasaurus_dozen)[dataset == "dino"][, dataset := NULL]

latinr <- rnaturalearth::ne_coastline(returnclass = "sf") %>% 
   sf::st_crop(c(xmin = -120, xmax = -30, ymin = -60, ymax = 30)) %>% 
   to_latlon() %>% 
   as.data.table() %>% 
   .[, x := scales::rescale(lon, range(start_data$x))] %>% 
   .[, y := scales::rescale(lat, range(start_data$y))] %>% 
   .[, `:=`(lon = NULL, lat = NULL)] %>% 
   .[]


metamers <- metamerize(latinr, 
                       preserve = delayed_with(mean(x), mean(y), cor(x, y)),
                       minimize = mean_dist_to(start_data),
                       perturbation = 0.05,
                       N = 1000)

g <- metamers %>% 
   as.data.frame() %>% 
   as.data.table() %>% 
   .[.metamer %in% 1:3] %>%
   ggplot(aes(x, y)) +
   geom_point()  +
   geom_richtext(data = function(d) d[, .(label = label(mean(x), mean(y))), 
                                      by = .metamer],
                 x = 85, y = 90, 
                 family = hrbrthemes::font_rc,
                 size = 7,
                 aes(label = label)) +
   # facet_wrap(~.metamer)
   transition_manual(.metamer) 


set.seed(42)
metamers <- metamerize(latinr,
                       preserve =  delayed_with(mean(x), mean(y)),
                       # minimize = mean_dist_to(start_data),  
                       signif = c(5, 4),
                       perturbation = 0.08, 
                       N = 10000,
                       trim = 300) %>% 
   metamerize(minimize = mean_dist_to(start_data),
              preserve =  delayed_with(mean(x), mean(y)),
              perturbation = 0.04,
              signif = c(5, 4),
              N = 300000,
              trim = 600) %>% 
   metamerize(perturbation = 0.08, 
              minimize = NULL,
              N = 10000,
              trim = 300) %>% 
   metamerize(minimize = mean_dist_to(latinr),
              N = 300000,
              perturbation = 0.04,
              trim = 600)



metamers %>% 
   as.data.frame() %>% 
   as.data.table() %>% 
   # subset(.metamer %in% c(1, 10, 40, 60)) %>% 
   ggplot(aes(x, y)) +
   geom_point()  +
   geom_richtext(data = function(d) d[, .(label = label(mean(x), mean(y))), 
                                      by = .metamer],
                 x = 85, y = 90, 
                 family = hrbrthemes::font_rc,
                 size = 7,
                 aes(label = label)) +
   transition_manual(.metamer) -> g


file <- "Presentación/latam_full.gif"
anim_save(file,  g, 
          duration = 5, fps = 20, 
          width = 500, height = 500)


img <- magick::image_read(file)

img_anim <- magick::image_animate(img, dispose = "none")

img_png <- magick::image_convert(img_anim, "png")

dump <- lapply(1:3, function(i) {
   magick::image_write(img_png[i], glue::glue("Presentación/frames/latam_{i}.png"),
                       format = "png", flatten = TRUE)   
})



