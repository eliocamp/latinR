library(data.table)
library(magrittr)
library(ggplot2)
library(metamer)
library(gganimate)
library(ggtext)
# Start with the datasaurus
# install.packages("datasauRus")
library(datasauRus)



theme_set(hrbrthemes::theme_ipsum_rc(base_size = 14))

x = c(9, 3.323654, 7.50454, 2.035657, 0.81675)
labs = c("Media X", "SD X", "Media Y", "SD Y", "Cor XY")
digits <- 6
signif <- c(4)

label <- function(x, signif, digits, labs = names(x)) {
   # browser()
   force(labs)
   format <- paste0("%.", digits, "f")
   
   x <- sprintf(format, x, zero.print = TRUE)
   x <- strsplit(x, "")
   
   if (length(signif) == 1) {
      signif <- rep(signif, length(x))
   }
   
   x <- lapply(seq_along(x), function(i) {
      paste0("<span style='color:black'>", 
             paste0(head(x[[i]], signif[i]), collapse = ""),
             "</span><span style='color:gray'>",
             paste0(tail(x[[i]], -signif[i]), collapse = ""),
             "</span>")
   })
   
   # x <- setNames(x, names)
   labs <- formatC(labs, width = max(nchar(labs)))
   # labs <- gsub(" ", "&nbsp;", labs)
   # browser()
   paste0(paste0(labs, ": ", x), collapse = "<br>")
}



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


# Anscombe  --------

data(anscombe)
anscombe <- anscombe %>% 
   as.data.table() %>%
   .[, obs := 1:.N] %>% 
   tidyr::gather(key, value, -obs) %>% 
   tidyr::separate(key, c("coord", "quartet"), sep = 1) %>% 
   as.data.table() %>% 
   dcast(obs + quartet ~ coord) %>% 
   .[, quartet := c("I", "II", "III", "IV")[as.numeric(quartet)]]
   
anscombe %>% 
dplyr::group_by(quartet) %>%
   dplyr::summarize(
      n=n(),
      mean.x=round(mean(x),5), 
      mean.y=round(mean(y),5), 
      sd.x=round(sd(x),5), 
      sd.y=round(sd(y),5), 
      correlation=round(cor(x,y),5))

ggplot(anscombe, aes(x, y)) +
   geom_point() +
   geom_richtext(data = function(d) d[, .(label = label(c("Media X" = mean(x),
                                                          "SD X" = sd(x),
                                                          "Media Y" = mean(y),
                                                          "SD Y" = sd(y),
                                                          "Cor XY" = cor(x, y)),
                                                        signif = c(5), digits = 2)),
                                      by = quartet],
                 # x = 25, y = 10, 
                 # hjust = -0.5,
                 family = hrbrthemes::font_rc,
                 size = 2,
                 aes(x = 25, y = 10,label = label)) +
   facet_wrap(~ quartet) +
   coord_cartesian(clip = "off")



ggsave("Presentación/anscombre.png", height = 4, width = 8)

# Datasaurio media -----------------

# 
# label <- function(x, y) {
#    
#    
#    x <- sprintf("%.4f", x)
#    x <- strsplit(x, "")[[1]]
#    x <- paste0("<span style='color:black'>", 
#                paste0(head(x, 4), collapse = ""),
#                "</span><span style='color:gray'>",
#                paste0(tail(x, -4), collapse = ""),
#                "</span>")
#    
#    y <- sprintf("%.4f", y)
#    y <- strsplit(y, "")[[1]]
#    y <- paste0("<span style='color:black'>", 
#                paste0(head(y, 4), collapse = ""),
#                "</span><span style='color:gray'>",
#                paste0(tail(y, -4), collapse = ""),
#                "</span>")
#    
#    
#    paste0("Media X: ", x, "<br>",
#           "Media Y: ", y)
# }

datasets <- unique(datasaurus_dozen$dataset)
datasets <- datasets[c(2:length(datasets), 1)]

trunc_means <- delayed_with(trunc(mean(x), 2),
                            trunc(mean(y), 2))
preserv <- delayed_with(c("Mean X" = mean(x), 
                          "Mean Y" = mean(y),
                          "Cor XY" = cor(x, y)))

start <- subset(datasauRus::datasaurus_dozen, dataset == "dino")
start$dataset <- NULL
set.seed(42)
for (d in datasets[1:2]) {
   end <- subset(datasaurus_dozen, dataset == d)
   end$dataset <- NULL
   
   start <- start %>% 
      metamerize(preserve = preserv,
                 minimize = mean_self_proximity,
                 N = 6000,
                 signif = 4,
                 perturbation = 0.06,
                 trim = 600,
                 name = d) %>% 
   metamerize(preserve = preserv,
              minimize = mean_dist_to(end),
              # N = 80000,
              N = 8000,
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
   geom_richtext(data = function(d) d[, .(label = label(preserv(.SD), signif = c(4, 4, 7), digits = 6)),
                                      by = .metamer],
                 x = 85, y = 90, 
                 # hjust = -0.5,
                 family = hrbrthemes::font_rc,
                 size = 7,
                 aes(label = label)) +
   transition_manual(.metamer)


anim_save("Presentación/dino_media.gif",  g, 
          nframes = N*30/2, fps = 20, 
          width = 500, height = 500)


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
                       preserve = delayed_with(mean(x), mean(y)),
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



