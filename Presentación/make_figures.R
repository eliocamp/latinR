## Crea los plots para la presentación

library(ggplot2)
library(data.table)
library(metR)
library(scales)
library(dplyr)
library(grid)
month.abb_sp <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                  "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
names(month.abb_sp) <- as.character(1:12)

theme_field <- function(w = 4) {
   theme(legend.position = "bottom", legend.box = "vertical",
         legend.key.width = unit(w, "lines"),
         panel.grid = element_line(color = "gray20", size = 0.2, linetype = 3),
         panel.ontop = TRUE
   )
}
theme_set(theme_minimal())
w <- 4 
theme_update(panel.grid = element_line(color = "gray20", size = 0.2, linetype = 3),
                   panel.ontop = TRUE
)

WaveFlux <- function(psi, p = 250, a = 6371000) {
   k <- p*100/(a^2*2000)
   psi <- copy(psi)
   psi[, c("psi.dlon", "psi.dlat") := Derivate(psi.z ~ lon + lat,
                                               cyclical = c(TRUE, FALSE))] %>%
      .[, psi.ddlon := Derivate(psi.z ~ lon, cyclical = TRUE, order = 2),
        by = lat] %>%
      .[, psi.dlondlat := Derivate(psi.dlon ~ lat),
        by = lon] %>%
      .[, `:=`(f.lon = k/cos(lat*pi/180)*(psi.dlon^2 - psi.z*psi.ddlon),
               f.lat = k*(psi.dlon*psi.dlat - psi.z*psi.dlondlat))]
   list(f.lon = psi$f.lon, f.lat = psi$f.lat)
}


dir <- "Presentación/img"
name <- function(file) {
   paste0(dir, "/", file, ".png")
}

ggsave2 <- function(file, width = 24, height = NA, ...) {
   ggsave(name(file), units = "cm", width = width, height = height, ...)
}

map <- map_data("world2")
geom_map <- geom_path(data = subset(map, lat <0),
                      aes(long, lat, group = group), color = "black", size = 0.2) 
geom_map2 <- geom_path(data = subset(map, lat < 40),
                      aes(long, lat, group = group), color = "black", size = 0.2) 

data <- ReadNetCDF("~/DATOS/NCEP Reanalysis/hgt.mon.mean.nc", 
                   subset = list(level = 500, lat = -90:0))
setnames(data, c("level", "hgt"), c("lev", "gh"))

data[, gh.z := Anomaly(gh), by = .(lat, time)]
data[, gh.a := Anomaly(gh), by = .(lon, lat, month(time))]

gdata <- data[time == time[.N]]
arrow_data <- data[, mean(gh), by = .(lon, lat, month(time))] %>% 
   .[, c("u", "v") := GeostrophicWind(V1, lon, lat)] %>% 
   .[lat %~% -34 & lon %~% (-58 + 360)]

remove(data)

ggplot(gdata, aes(lon, lat)) +
   stat_contour(aes(z = gh.z, fill = ..level..), geom = "polygon", binwidth = 30) +
   geom_contour(aes(z = gh.z), color = "black", size = 0.15, binwidth = 30) +
   geom_map +
   # scale_y_latitude() +
   # scale_x_longitude() +
   coord_quickmap(ylim = c(-90, -25)) +
   scale_fill_gradient2("Altura\ngeopotencial", 
                        high = muted("red"), low = muted("blue"), 
                        guide = guide_legend()) +
   theme_gray()

ggsave2("ggplot_mal")


ggplot(gdata, aes(lon, lat)) +
   geom_contour_fill(aes(z = gh.z), breaks = AnchorBreaks(0, 30, 0)) +
   geom_contour2(aes(z = gh.z), color = "black", breaks = AnchorBreaks(0, 30, 0),
                 size = 0.15) +
   geom_map +
   scale_y_latitude() +
   scale_x_longitude() +
   coord_quickmap(ylim = c(-90, -25)) +
   scale_fill_divergent("Altura\ngeopotencial",
                        guide = guide_colorstrip(),
                         breaks = AnchorBreaks(0, 30, 0))

ggsave2("geom_contour_fill")

ggplot(gdata, aes(lon, lat)) +
   geom_contour_fill(aes(z = gh.z), breaks = AnchorBreaks(0, 30, 0)) +
   geom_contour2(aes(z = gh.z), color = "black", breaks = AnchorBreaks(0, 30, 0),
                 size = 0.15) +
   geom_map +
   geom_text_contour(aes(z = gh.z), breaks = AnchorBreaks(0, 30, 0),
                     stroke = 0.2, rotate = FALSE) +
   scale_y_latitude() +
   scale_x_longitude() +
   coord_quickmap(ylim = c(-90, -25)) +
   scale_fill_divergent("Altura\ngeopotencial",
                        guide = guide_colorstrip(),
                        breaks = AnchorBreaks(0, 30, 0))

ggsave2("geom_text_contour")

ggplot(gdata, aes(lon, lat)) +
   geom_contour_fill(aes(z = gh.z), breaks = AnchorBreaks(0, 30, 0)) +
   geom_contour_tanaka(aes(z = gh.z), color = "black", breaks = AnchorBreaks(0, 30, 0),
                       range = c(0.01, 0.7)) +
   geom_map +
   scale_y_latitude() +
   scale_x_longitude() +
   coord_quickmap(ylim = c(-90, -25)) +
   scale_fill_divergent("Altura\ngeopotencial",
                        guide = guide_colorstrip(),
                        breaks = AnchorBreaks(0, 30, 0))

ggsave2("geom_contour_tanaka")


stream <- ReadNetCDF("~/DATOS/NCEP Reanalysis/psi.mon.mean.nc",
                     subset = list(level = 0.2101,
                                   lat = -90:10,
                                   time = unique(gdata$time)))

stream[, psi.z := Anomaly(psi), by = lat]
stream[, c("u", "v") := WaveFlux(.SD)]

scale <- 30
my_arrow <- arrow(length = unit(0.2, "lines"))
ggplot(stream, aes(lon, lat)) +
   geom_segment(aes(xend = lon + scale*u, yend = lat + scale*v),
                arrow = my_arrow) +
   geom_map +
   scale_y_latitude() +
   scale_x_longitude() +
   coord_quickmap(ylim = c(-90, -25)) 

ggsave2("geom_segment")

ggplot(stream, aes(lon, lat)) +
   geom_segment(aes(xend = lon + scale*u, yend = lat + scale*v),
                arrow = my_arrow) +
   geom_map +
   scale_y_latitude() +
   scale_x_longitude() +
   coord_map(ylim = c(-70, -25))
ggsave2("geom_segment_proj")

ggplot(stream, aes(lon, lat)) +
   geom_segment(aes(xend = lon + scale*u, yend = lat + scale*v),
                arrow = my_arrow) +
   geom_map +
   scale_y_latitude() +
   scale_x_longitude() +
   coord_polar()
ggsave2("geom_segment_polar", height = 6.35*2.54, width = 6.35*2.54)

ggplot(arrow_data, aes(month, 1)) +
   geom_arrow(aes(dx = u, dy = v), preserve.dir = TRUE) +
   scale_mag(guide = "none") +
   scale_y_continuous("", breaks = NULL) +
   scale_x_continuous("Mes", breaks = 1:12, labels = month.abb_sp)
ggsave2("geom_segment_dir")



ggplot(stream, aes(lon, lat)) +
   geom_contour_fill(aes(z = psi.z*1e-6), breaks = AnchorBreaks(0, 5, 0)) +
   geom_contour2(aes(z = psi.z*1e-6), color = "black", breaks = AnchorBreaks(0, 5, 0),
                 size = 0.15) +
   geom_vector(aes(dx = u, dy = v), skip = 2, min.mag = 0.05) + 
   scale_mag("Flujo de\nacción de onda",labels = 0.5) +
   geom_map +
   scale_y_latitude() +
   scale_x_longitude() +
   coord_quickmap(ylim = c(-90, -25)) +
   scale_fill_divergent("Función\ncorriente",
                        guide = "none",
                        breaks = AnchorBreaks(0, 5, 0))
ggsave2("geom_vector")

stream2 <- ReadNetCDF("~/DATOS/NCEP Reanalysis/psi.mon.mean.nc",
                     subset = list(level = 0.2101,
                                   lat = -90:60))
stream2[, psi.z := Anomaly(psi), by = .(lat, time)]

stream2[month(time) == 1, .(psi.z = mean(psi.z)), by = .(lon, lat)] %>% 
   .[, c("u", "v") := WaveFlux(.SD)] -> stream2


ggplot(stream2, aes(lon, lat)) +
   geom_contour_fill(aes(z = psi.z*1e-6), breaks = AnchorBreaks(0, 5, 0)) +
   geom_contour2(aes(z = psi.z*1e-6), color = "black", breaks = AnchorBreaks(0, 5, 0),
                 size = 0.15) +
   # geom_streamline(aes(dx = u, dy = v), skip = 2, min.L = 3, 
   #                 arrow.length = 0.2, res = 3) + 
   geom_vector(aes(dx = u, dy = v), skip = 2, alpha = 1) +
   scale_mag("Flujo de\nacción de onda", max = 0.7) +
   geom_map2 +
   scale_y_latitude() +
   scale_x_longitude() +
   coord_quickmap(ylim = c(-90, 40)) +
   scale_fill_divergent("Función\ncorriente",
                        guide = "none",
                        breaks = AnchorBreaks(0, 5, 0))
ggsave2("geom_vector_chico")

ggplot(stream2, aes(lon, lat)) +
   geom_contour_fill(aes(z = psi.z*1e-6), breaks = AnchorBreaks(0, 5, 0)) +
   geom_contour2(aes(z = psi.z*1e-6), color = "black", breaks = AnchorBreaks(0, 5, 0),
                 size = 0.15) +
   # geom_streamline(aes(dx = u, dy = v), skip = 2, min.L = 3, 
   #                 arrow.length = 0.2, res = 3) + 
   geom_vector(aes(dx = u, dy = v), skip = 2, alpha = 1) +
   scale_mag("Flujo de\nacción de onda", max = 0.1) +
   geom_map2 +
   scale_y_latitude() +
   scale_x_longitude() +
   coord_quickmap(ylim = c(-90, 40)) +
   scale_fill_divergent("Función\ncorriente",
                        guide = "none",
                        breaks = AnchorBreaks(0, 5, 0))
ggsave2("geom_vector_grande")


ggplot(stream2, aes(lon, lat)) +
   geom_contour_fill(aes(z = psi.z*1e-6), breaks = AnchorBreaks(0, 5, 0)) +
   geom_contour2(aes(z = psi.z*1e-6), color = "black", breaks = AnchorBreaks(0, 5, 0),
                 size = 0.15) +
   geom_streamline(aes(dx = u, dy = v), skip = 2, min.L = 0.5,
                   L = 10, xwrap = c(0, 360),
                   arrow.length = 0.2, res = 3) +
   geom_vector(aes(dx = u, dy = v), skip = 2, alpha = 0) +
   scale_mag("Flujo de\nacción de onda", max = 0.1) +
   geom_map2 +
   scale_y_latitude() +
   scale_x_longitude() +
   coord_quickmap(ylim = c(-90, 40)) +
   scale_fill_divergent("Función\ncorriente",
                        guide = "none",
                        breaks = AnchorBreaks(0, 5, 0))
ggsave2("geom_streamline")

gdata %>% 
      ggplot(aes(lon, lat)) +
   geom_contour_fill(aes(z = gh.z), breaks = AnchorBreaks(0, 30, 0)) +
   geom_contour2(aes(z = gh.z), color = "black", breaks = AnchorBreaks(0, 30, 0),
                 size = 0.15) +
   geom_map +
   scale_y_latitude(limits = c(-90, -25)) +
   scale_x_longitude() +
   coord_polar() +
   scale_fill_divergent("Altura\ngeopotencial",
                        guide = guide_colorstrip(),
                        breaks = AnchorBreaks(0, 30, 0))

ggsave2("ggperiodic_mal", height = 6.35*2.54, width = 6.35*2.54)

g <- ggperiodic::periodic(gdata, lon = c(0, 360))

gdata <- ggperiodic::periodic(gdata, lon = c(0, 360))

ggplot(g, aes(lon, lat)) +
   geom_contour_fill(aes(z = gh.z), breaks = AnchorBreaks(0, 30, 0)) +
   geom_contour2(aes(z = gh.z), color = "black", breaks = AnchorBreaks(0, 30, 0),
                 size = 0.15) +
   geom_map +
   scale_y_latitude(limits = c(-90, -25)) +
   scale_x_longitude() +
   coord_polar() +
   scale_fill_divergent("Altura\ngeopotencial",
                        guide = guide_colorstrip(),
                        breaks = AnchorBreaks(0, 30, 0))
ggsave2("ggperiodic", height = 6.35*2.54, width = 6.35*2.54)