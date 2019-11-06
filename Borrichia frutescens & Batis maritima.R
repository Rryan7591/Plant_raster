packages<-c("dismo","rgbif","rdryad","utils","readxl","spotifyr","ggridges","viridis","rasterVis")
sapply(packages, require, character.only=T)

bofr_lu <- name_lookup(query = 'Borrichia frutescens', return = 'data')

bofr_taxon <- print(as.integer(names(which.max(table(bofr_lu$nubKey)))))

occ_count(taxonKey = bofr_taxon, georeferenced = TRUE)

usa <- isocodes[grep("United States", isocodes$name), "code"]

states <- map_data("state")
Coast_set <- subset(states, region %in% c("maine","new hampshire","massachusetts","connecticut",
                                          "new york","new jersey","delaware","maryland","virginia",
                                          "north carolina","south carolina","georgia","florida",
                                          "alabama","mississippi","louisiana","texas","rhode island"))

bofr_data <- occ_search(taxonKey = bofr_taxon, 
                        return = 'data', 
                        country = usa, 
                        hasCoordinate = TRUE)

bofr_df <- as.data.frame(cbind(bofr_data$US$scientificName,
                               bofr_data$US$institutionCode,
                               bofr_data$US$stateProvince,
                               bofr_data$US$verbatimLocality))

bofr_coords <- cbind(type.convert(bofr_data[["US"]][["decimalLongitude"]], as.is = TRUE),
                type.convert(bofr_data[["US"]][["decimalLatitude"]], as.is = TRUE))

bofr_info <- cbind(bofr_df,bofr_coords)

colnames(bofr_info) <- c("species","dataset","state","location","longitude","latitude")

ggplot(data = bofr_info, aes(x=longitude, y=latitude)) +
  geom_polygon(data = Texas, aes(x=long, y = lat, group = group),
               fill = "white", color="black") +
  geom_point(aes(color = state)) +
  coord_fixed(xlim = c(-107,-93), ylim = c(25,37)) +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Sea Ox-eye occurance in Texas") + 
  guides(color=guide_legend("Legend")) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right") +
  theme(legend.title.align = 0.5, legend.box.just = "center") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

bofr_xy <- bofr_info %>% select(5:6)

bofr_bio_extent <- extent(x = c(
  min(bofr_xy$longitude),
  max(bofr_xy$longitude),
  min(bofr_xy$latitude),
  max(bofr_xy$latitude)))


bofr_extent <- extent(-107,-93,25,37)

bofr_bioclim <- getData(name = "worldclim", res = 2.5, var = "bio")

names(bofr_bioclim) <- c("Ann Mean Temp","Mean Diurnal Range","Isothermality","Temperature Seasonality",
                         "Max Temp Warmest Mo","Min Temp Coldest Mo","Ann Temp Range","Mean Temp Wettest Qtr",
                         "Mean Temp Driest Qtr","Mean Temp Warmest Qtr","Mean Temp Coldest Qtr","Annual Precip",
                         "Precip Wettest Mo","Precip Driest Mo","Precip Seasonality","Precip Wettest Qtr",
                         "Precip Driest Qtr","Precip Warmest Qtr","Precip Coldest Qtr")


bofr_bioclim_extent <- crop(x = bofr_bioclim, y = bofr_bio_extent)
bofr_bioclim_model <- bioclim(x = bofr_bioclim_extent, p = bofr_xy)
bofr_presence_model <- dismo::predict(object = bofr_bioclim_model, 
                                 x = bofr_bioclim_extent, 
                                 ext = bofr_bio_extent)

gplot(bofr_presence_model) + 
  geom_raster(aes(fill=value)) +
  geom_polygon(data = Texas, aes(x= long, y = lat, group = group),
               fill = NA, color="black") +
  geom_point(data = bofr_xy, aes(x = longitude, y = latitude), color = "black", alpha = 0.5) +
  scale_fill_gradientn(colours=c("brown","yellow","darkgreen"), "Probability") +
  coord_fixed(xlim = c(-107,-93), ylim = c(25,37)) +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Probability of Sea Ox-eye Occurrence") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

bama_lu <- name_lookup(query = 'Batis maritima', return = 'data')

bama_taxon <- print(as.integer(names(which.max(table(bama_lu$nubKey)))))

occ_count(taxonKey = bama_taxon, georeferenced = TRUE)

usa <- isocodes[grep("United States", isocodes$name), "code"]

states <- map_data("state")
Coast_set <- subset(states, region %in% c("maine","new hampshire","massachusetts","connecticut",
                                          "new york","new jersey","delaware","maryland","virginia",
                                          "north carolina","south carolina","georgia","florida",
                                          "alabama","mississippi","louisiana","texas","rhode island"))

bama_data <- occ_search(taxonKey = bama_taxon, 
                        return = 'data', 
                        country = usa, 
                        hasCoordinate = TRUE)

bama_df <- as.data.frame(cbind(bama_data$US$scientificName,
                               bama_data$US$institutionCode,
                               bama_data$US$stateProvince,
                               bama_data$US$verbatimLocality))

bama_coords <- cbind(type.convert(bama_data[["US"]][["decimalLongitude"]], as.is = TRUE),
                     type.convert(bama_data[["US"]][["decimalLatitude"]], as.is = TRUE))

bama_info <- cbind(bama_df,bama_coords)

colnames(bama_info) <- c("species","dataset","state","location","longitude","latitude")

ggplot(data = bama_info, aes(x=longitude, y=latitude)) +
  geom_polygon(data = Coast_set, aes(x=long, y = lat, group = group),
               fill = "white", color="black") +
  geom_point(aes(color = state)) +
  coord_fixed(xlim = c(-110,-65), ylim = c(25,50)) +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Saltwort occurance") + 
  guides(color=guide_legend("Legend")) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right") +
  theme(legend.title.align = 0.5, legend.box.just = "center") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

bama_xy <- bama_info %>% select(5:6)

bama_bio_extent <- extent(x = c(
  min(bama_xy$longitude),
  max(bama_xy$longitude),
  min(bama_xy$latitude),
  max(bama_xy$latitude)))


bama_extent <- extent(-107,-93,25,37)

bama_bioclim <- getData(name = "worldclim", res = 2.5, var = "bio")

names(bama_bioclim) <- c("Ann Mean Temp","Mean Diurnal Range","Isothermality","Temperature Seasonality",
                         "Max Temp Warmest Mo","Min Temp Coldest Mo","Ann Temp Range","Mean Temp Wettest Qtr",
                         "Mean Temp Driest Qtr","Mean Temp Warmest Qtr","Mean Temp Coldest Qtr","Annual Precip",
                         "Precip Wettest Mo","Precip Driest Mo","Precip Seasonality","Precip Wettest Qtr",
                         "Precip Driest Qtr","Precip Warmest Qtr","Precip Coldest Qtr")


bama_bioclim_extent <- crop(x = bama_bioclim, y = bama_bio_extent)
bama_bioclim_model <- bioclim(x = bama_bioclim_extent, p = bama_xy)
bama_presence_model <- dismo::predict(object = bama_bioclim_model, 
                                      x = bama_bioclim_extent, 
                                      ext = bama_bio_extent)

gplot(bama_presence_model) + 
  geom_raster(aes(fill=value)) +
  geom_polygon(data = Texas, aes(x= long, y = lat, group = group),
               fill = NA, color="black") +
  geom_point(data = bama_xy, aes(x = longitude, y = latitude), color = "black", alpha = 0.5) +
  scale_fill_gradientn(colours=c("brown","yellow","darkgreen"), "Probability") +
  coord_fixed(xlim = c(-107,-93), ylim = c(25,37)) +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Probability of Saltwort Occurrence") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

rm <- merge(bofr_presence_model, bama_presence_model)

gplot(rm) + 
  geom_raster(aes(fill=value)) +
  geom_polygon(data = Texas, aes(x= long, y = lat, group = group),
               fill = NA, color="black") +
  scale_fill_gradientn(colours=c("brown","yellow","darkgreen"), "Probability") +
  coord_fixed(xlim = c(-107,-93), ylim = c(25,37)) +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Probability of Sea Ox-eye Occurrence") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = "right") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

