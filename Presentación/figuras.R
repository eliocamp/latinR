library(data.table)
library(magrittr)
library(ggplot2)
library(metamer)
library(gganimate)
library(ggtext)
# Start with the datasaurus
# install.packages("datasauRus")
library(datasauRus)



# theme_set(hrbrthemes::theme_ipsum_rc(base_size = 14))

theme_set(ggthemes::theme_few(base_size = 14, base_family = hrbrthemes::font_rc))
x = c(9, 3.323654, 7.50454, 2.035657, 0.81675)
labs = c("Media X", "SD X", "Media Y", "SD Y", "Cor XY")
digits <- 6
signif <- c(4)

label <- function(x, signif, digits, labs = names(x), dark = "black", light = "gray") {
   # browser()
   force(labs)
   format <- paste0("%.", digits, "f")
   
   x <- sprintf(format, x, zero.print = TRUE)
   x <- strsplit(x, "")
   
   if (length(signif) == 1) {
      signif <- rep(signif, length(x))
   }
   
   x <- lapply(seq_along(x), function(i) {
      paste0("<span style='color:", dark, "'>", 
             paste0(head(x[[i]], signif[i]), collapse = ""),
             "</span><span style='color:", light, "'>",
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
   geom_smooth(method = "lm", color = "#7700FF", se = FALSE) +
   geom_point() +
   geom_richtext(data = function(d) d[, .(label = label(c("Media X" = mean(x),
                                                          "SD X" = sd(x),
                                                          "Media Y" = mean(y),
                                                          "SD Y" = sd(y),
                                                          "Cor XY" = cor(x, y)),
                                                        signif = c(5), digits = 2)),
                                      by = quartet],
                 label.r = unit(0.05, "lines"),
                 # x = 25, y = 10,
                 # hjust = -0.5,
                 # vjust = -0.5,
                 family = hrbrthemes::font_rc,
                 size = 3.5,
                 aes(x = 18, y = 4, label = label)) +
   scale_x_continuous(limits = c(0, 20)) +
   scale_y_continuous(limits = c(0, 13)) +
   facet_wrap(~ quartet) 
# ggthemes::geom_rangeframe() +




ggsave("Presentación/anscombre.png", height = 4.5, width = 8)

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






g <- plot_spectrum("Blue") 

g <- g + theme(plot.background = element_blank(), panel.background = element_blank())

ggsave("Presentación/azul_espectro.png", g, height = 3, width = 4, bg = "transparent")

g <- plot_spectrum("YellowBanana")

ggsave("Presentación/banana_espectro.png",g,  height = 2, width = 2)



value <- "Blue"
color <- function(data) c(colorscience::spectra2XYZ(as.matrix(data)),
                          data[[value]] >= 0,
                          max(diff(data[[value]], 2)) < 6, 
                          sum(data[[value]]))




Yellow <- as.data.frame(MaterialReferenceData[, c("wavelength", "Yellow")])
Blue <- as.data.frame(MaterialReferenceData[, c("wavelength", "Blue")])


blueish <- metamerize(Blue, 
                      preserve = color,
                      minimize = mean_dist_to(Yellow),
                      # minimize = delayed_with(sd(Blue)),
                      change = value,
                      signif = 2,
                      N = 80000, 
                      perturbation = 0.03,
                      trim = 500)



xyz2hex <- function(xyz) {
   do.call(rgb, as.list(colorscience::XYZ2RGB(xyz)))
}

plot_spectrum("Blue")

plot_spectrum("Yellow")

blueish[[length(blueish)]] %>% 
   as.data.table() %>% 
   [wavelength %between% c(380, 700)] %>% 
   .[, color := wavelength_to_rgb(wavelength), by = wavelength] %>% 
   ggplot(aes(wavelength, Blue)) +
   geom_line(aes(color = color, group = 1), size = 2) +
   # geom_col(aes(fill = color)) +
   scale_x_continuous("Longitud de onda [nm]", limits = c(NA, 700)) +
   scale_y_continuous("Espectro") +
   scale_color_identity() +
   scale_fill_identity()


colors <- blueish %>% 
   as.data.frame() %>% 
   as.data.table() %>% 
   .[, color := xyz2hex(color(.SD)[1:3]), by = .(.metamer)] 

colors_RGB <- colors %>% 
   .[, setNames(as.list(colorscience::XYZ2RGB(color(.SD)[1:3])),
                c("R", "G", "B")),
     by = .metamer, .SDcols = c("wavelength", "Blue")] %>% 
   .[, label := label(c(Rojo = R, Verde = G, Azul = B), 
                      4, 4, dark = "white", light = "gray"), by = .metamer]

colors %>% 
   as.data.frame() %>% 
   as.data.table() %>% 
   # .[.metamer == 1] %>% 
   ggplot(aes(wavelength, Blue)) +
   geom_line() +
   geom_richtext(data = colors_RGB,
                 inherit.aes = FALSE,
                 x = 570, y = 30, 
                 # hjust = -0.5,
                 family = hrbrthemes::font_rc,
                 size = 7,
                 color = "white",
                 aes(label = label, fill = rgb(R, G, B))) +
   scale_x_continuous("Longitud de onda [nm]", limits = c(NA, NA)) +
   scale_y_continuous("Espectro") +
   scale_color_identity()  +
   scale_fill_identity() +
   transition_manual(.metamer) -> g



anim_save("Presentación/colores_metameros.gif", g,
          fps = 20,
          width = 700, height = 500)


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
# label <- function(x, y) {
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
                       minimize = mean_self_proximity,
                       signif = c(5, 4),
                       perturbation = 0.08, 
                       N = 100000,
                       trim = 300) %>% 
   metamerize(minimize = c(mean_dist_to(start_data),
                           mean_self_proximity),
              perturbation = 0.04,
              signif = c(5, 4),
              N = 300000,
              trim = 600) %>% 
   metamerize(perturbation = 0.08, 
              minimize = mean_self_proximity,
              N = 10000,
              trim = 300) %>% 
   metamerize(minimize = c(mean_dist_to(latinr),
                           mean_self_proximity),
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



metamers <- metamerize(latinr,
                       preserve     =  delayed_with(mean(x), mean(y)),
                       minimize     = c(mean_dist_to(dino),
                                        mean_self_proximity),
                       signif       = c(5, 4),
                       perturbation = 0.04, 
                       N            = 300000,
                       trim         = 600) %>% 
   metamerize(minimize = c(mean_dist_to(latinr),
                           mean_self_proximity),
              N        = 300000,
              trim     = 600)



# Elefante -------
elefante <- data.table::fread("elefante.csv")
elefante <- setNames(elefante, c("x", "y"))

elefante$x <- scales::rescale(elefante$x, c(-2.5, 2.5))
elefante$y <- scales::rescale(elefante$y, c(0, 1))

xout <- seq(-2.5, 2.5, length.out = 64)
elefante_density <- as.data.frame(with(elefante, approx(x, y, xout = xout)))

elefante <- function(data) {
   y <- density(data$x, from = -2.5, to = 2.5, n = 64)$y
   y <- y/max(y)
   mean(abs(y - elefante_density$y))
}

elefante_bg <- jpeg::readJPEG("principito-elefante.jpg")
elefante_bg <- raster::as.matrix(raster::raster(elefante_bg))
elefante_bg <- reshape2::melt(elefante_bg)
elefante_bg$x <- scales::rescale(elefante_bg$Var2, c(-2.5, 3))
elefante_bg$y <- scales::rescale(-elefante_bg$Var1, c(0, 1))


set.seed(42)
elefantes <- metamerize(data.frame(x = rt(200, 5)), # dataset inicial
                        preserve = moments_n(1:3),  # función a preservar
                        minimize = elefante,        # función a minimizar
                        annealing = FALSE,          # todos los metámeros minimizan `minimize` 
                        signif = 3,
                        perturbation = 0.01,
                        N = 250000)  


# N <- nrow(elefante_bg)
# elefante_bg <- elefante_bg[rep(seq_len(N), 10), ]
# elefante_bg$.metamer <- as.integer(rep(seq(1, length(elefantes), length.out = 10), each = N))

elefantes <- elefantes %>% 
   as.data.frame() %>% 
   as.data.table()


ele_labs <- elefantes %>% 
   .[, as.list(moments_n(1:3)(.SD)), by  = .metamer] %>% 
   .[, label := label(c("M1" = x_1, "M2" = x_2, "M3" = x_3), c(5, 3, 3), 5), by = .metamer]


ggplot(elefantes, aes(x, group = .metamer)) +
   # geom_raster(data = elefante_bg, 
   # aes(x, y, fill = value, alpha = .metamer)) +
   geom_density(aes(y = ..scaled..), size = 1) + 
   geom_richtext(data = ele_labs,
                 label.r = unit(0.05, "lines"),
                 family = hrbrthemes::font_rc,
                 size = 5,
                 aes(x = 2, y = 0.6, label = label)) +
   scale_y_continuous("Densidad", 
                      breaks = scales::pretty_breaks(n = 2)) +
   scale_x_continuous(limits = c(-2.5, 3)) +
   coord_cartesian(ylim = c(0, 1), clip = "off") +
   scale_fill_gradient(low = "black", high = "white", guide = "none") +
   scale_alpha(range = c(0, 1), guide = "none") +
   # coord_fixed(ratio = 320/89) 
   # transition_time(.metamer) +
   transition_manual(.metamer) +
   theme(panel.grid = element_blank(), strip.text = element_blank(),
         aspect.ratio = 89/320) -> g

file <- "Presentación/sombrero.gif"
anim_save(file,  g, renderer = gifski_renderer(loop = FALSE),
          duration = 5, fps = 20, 
          width = 320*3, height = 89*3)


N <- nrow(elefante_bg)
elefante_bg <- elefante_bg[rep(seq_len(N), 2), ]
elefante_bg$frame <- as.integer(rep(c(1, 2), each = N))


ggplot(elefantes[.metamer == max(.metamer)], aes(x)) +
   geom_raster(data = elefante_bg,
               aes(x, y, fill = value, alpha = frame)) +
   geom_density(aes(y = ..scaled..), size = 1) + 
   geom_richtext(data = ele_labs[.metamer == max(.metamer)],
                 label.r = unit(0.05, "lines"),
                 family = hrbrthemes::font_rc,
                 size = 5,
                 aes(x = 2, y = 0.6, label = label)) +
   scale_y_continuous("Densidad", 
                      breaks = scales::pretty_breaks(n = 2)) +
   scale_x_continuous(limits = c(-2.5, 3)) +
   coord_cartesian(ylim = c(0, 1), clip = "off") +
   scale_fill_gradient(low = "black", high = "white", guide = "none") +
   scale_alpha(range = c(0, 1), guide = "none") +
   transition_states(frame) +
   # transition_manual(.metamer) +
   theme(panel.grid = element_blank(), strip.text = element_blank(),
         aspect.ratio = 89/320) -> g

file <- "Presentación/sombrero_fin.gif"
anim_save(file,  g, 
          duration = 3, fps = 20,
          width = 320*3, height = 89*3)





energia <- function(data) {
   with(data, c(h*9.8 + 1/2*v^2, 
                h > 0))
}

altura <- function(data) {
   data$h
}



condicion_inicial <- data.frame(h = 1, v = 0) 
set.seed(42)
estados_posibles  <- metamerize(condicion_inicial, 
                                preserve = energia, 
                                minimize = altura,
                                N = 100000, 
                                perturb = 0.5)

estados_posibles %>% 
   as.data.frame() %>% 
   ggplot(aes(v, h)) +
   geom_line()




temperatura_potencial <- function(aire) {
   with(aire, metR::Adiabat(presion*100, temperatura + 273.16))
}
presion <- function(aire) aire$presion

aire <- data.frame(presion = 1013, temperatura = 15)
adiabatica <- metamerize(aire, 
                         preserve = temperatura_potencial,
                         minimize = presion,
                         perturbation = c(5, 0.1),
                         signif = 3,
                         N = 50000)


ggplot(adiabatica, aes(temperatura, presion), n = 100) +
   
   stat_function(fun = function(x) Adiabat(t = x + 273.16, 
                                           theta = temperatura_potencial(aire))/100,
                 color = "red", size = 2) +
   geom_point() +
   scale_x_continuous("Temperatura (°C)") +
   scale_y_continuous("Presión (hPa)") +
   coord_trans(y = reverselog_trans())

ggsave("Presentación/adiabatica.png", width = 8, height = 4)  
