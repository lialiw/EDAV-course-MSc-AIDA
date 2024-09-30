#> Sources:

#> https://geocompr.robinlovelace.net/spatial-class.html#raster-classes
#> https://geocompr.robinlovelace.net/attr.html
#> https://geocompr.robinlovelace.net/spatial-operations.html#merging-rasters
#> https://geocompr.robinlovelace.net/geometric-operations.html#extent-and-origin
#> https://geocompr.robinlovelace.net/raster-vector.html
#> https://geocompr.robinlovelace.net/read-write.html#iovec

#> https://github.com/dkotzaitsis/openMapProject/tree/master/download_resources 

#> https://www.rdocumentation.org/packages/sf/versions/0.2-2/topics/st_read 
#> https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
#> https://r-spatial.github.io/sf/articles/sf5.html
#> https://cran.r-project.org/web/packages/sf/vignettes/sf5.html
#> https://ggplot2.tidyverse.org/reference/ggsf.html
#> https://rpubs.com/RatherBit/188960
#> https://r-spatial.github.io/sf/reference/st_jitter.html
#> https://rdrr.io/cran/sf/man/st_coordinates.html
#> https://www.rdocumentation.org/packages/methods/versions/3.6.2/topics/show

#> http://www.sthda.com/english/wiki/running-rstudio-and-setting-up-your-working-directory-easy-r-programming 
#> https://www.geeksforgeeks.org/change-column-name-of-a-given-dataframe-in-r/
#> http://www.r-tutor.com/elementary-statistics/numerical-measures/standard-deviation
#> https://cran.r-project.org/web/packages/lessR/vignettes/Extract.html
#> http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization


library(udunits2)
# https://github.com/r-spatial/sf/issues/1534
# https://stackoverflow.com/questions/62797210/problem-with-sf-package-in-r-how-can-i-solve-it/65465515#65465515
# https://github.com/Nowosad/spDataLarge
library(sf)
library(terra) 
library(dplyr)
library(ggplot2)

#reading and examining the data
file_location = "~/30n000e_20101117_gmted_mea300.tif"
mareNostrum_rast =  rast(file_location)
#mareNostrum_rast; plot(mareNostrum_rast)

file_location = "~/GRC_ADM2.shp"
adm_shape = read_sf(file_location)
#adm_shape; plot(adm_shape)
adm_shape
plot(adm_shape[0])

file_location = "~/poleis.shp"
pol_shape = read_sf(file_location)
#pol_shape; plot(pol_shape)

file_location = "~/places.shp"
plac_shape = read_sf(file_location)
#plac_shape; plot(plac_shape)


# 1. Create a new raster file that only contains Greece.

# --> 6.2 Raster cropping
gr_cropped = crop(mareNostrum_rast, vect(adm_shape))
plot(gr_cropped)
gr_final = mask(gr_cropped, vect(adm_shape))
plot(gr_final)
#class(gr_final)

#setwd("C:/R_edav/hw6")
writeRaster(gr_final, filename = "~/greece_rast.tif", datatype = "INT2U", overwrite=TRUE) 


# 2. Calculate the altitude for each capital of Greece. Provide a map showing the 
# prefectures of Greece and the capitals where the size of the point corresponding 
# to each capital is proportional to the altitude of the capital.

# Both target and cropping objects must have the same projection.
#> #zion = st_transform(zion, crs(srtm)) --> # 6.2 Raster cropping

pol_shape = st_transform(pol_shape, crs(gr_final))
#plot(gr_final); plot(pol_shape, add=TRUE)

# --> 6.3 Raster extraction
elevation = terra::extract(gr_final, vect(pol_shape))
capitals_el = cbind(pol_shape,elevation)
#capitals_el; View(capitals_el)
colnames(capitals_el)[4] <- "height"
#capitals_el; plot(capitals_el); View(capitals_el)

# --> 2.2.4 Base plot arguments
cex = capitals_el$height / max(capitals_el$height) 
plot(adm_shape[0])
plot(st_geometry(capitals_el),add=TRUE, cex=cex)


# 3. Calculate the average altitude and standard deviation of altitude for each prefecture in Greece.
# Provide two choropleth maps of the prefectures of Greece, one based on average altitude and one
# based on the standard deviation of altitude.

function_quest3 <- function(aShape, baseRaster){
  # Both target and cropping objects must have the same projection.
  aShape = st_transform(aShape, crs(baseRaster))
  #plot(baseRaster);  plot(aShape)
  
  elevation = terra::extract(baseRaster, vect(aShape))
  colnames(elevation)[2] <- "height"
  #View(elevation)
  
  # --> #4.2.5 Non-overlapping joins
  ele_mean_std = elevation %>% group_by(ID) %>% summarize(height_mean=mean(height), height_std=sd(height))
  aShape_ele_mean_std = cbind(aShape, ele_mean_std)
  #aShape_ele_mean_std
  
  plot(aShape_ele_mean_std["height_mean"])
  plot(aShape_ele_mean_std["height_std"])
  
  return(aShape_ele_mean_std)
}

adm_shape_ele_mean_std = function_quest3(adm_shape, gr_final)


# 4. For each prefecture, calculate the absolute difference between the average altitude of the prefecture
# and the altitude of its capital and provide the corresponding choropleth map.

function_quest4 <-function(elements_sh, basicRaster, base_sh){
  
  elements_sh = st_transform(elements_sh, crs(basicRaster))
  base_sh = st_transform(base_sh, crs(basicRaster))
  
  #class(capitals_el)
  
  sf::sf_use_s2(FALSE) # https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
  elements_base_ele_mean_std = st_join(base_sh, elements_sh)
  #view(elements_base_ele_mean_std)
  elements_base_ele_mean_std = elements_base_ele_mean_std %>% mutate(diff_means = abs(elements_base_ele_mean_std$height - elements_base_ele_mean_std$height_mean))
  #view(elements_base_ele_mean_std)
  
  plot(elements_base_ele_mean_std["diff_means"])
  
  sf::sf_use_s2(TRUE)
}

function_quest4(capitals_el, gr_final, adm_shape_ele_mean_std)


# 5. Who are the top 10 prefectures in terms of average altitude, and who are the top 10 prefectures in
# terms of altitude standard deviation?

adm_shape_ele_mean_std %>% top_n(n=10,wt=height_mean) %>% dplyr::select(NAME, height_mean)
adm_shape_ele_mean_std %>% top_n(n=10,wt=height_std)%>% dplyr::select(NAME, height_std)


# 6. Calculate the altitude for each location (places.shp) in Greece. Create a map showing the locations
# that are above 1500 meters along with their names. Locations that are inhabited places should
# appear in a different color.

# Both target and cropping objects must have the same projection.
plac_shape = st_transform(plac_shape, crs(gr_final)); plac_shape

# --> 6.3 Raster extraction
elevation = terra::extract(gr_final, vect(plac_shape))
places_ele = cbind(plac_shape,elevation)
colnames(elevation)[2] <- "height"
#places_ele; View(places_ele)

high_places = places_ele %>% filter(places_ele$height>1500) %>%  st_as_sf(coords=c("x","y")) %>% st_set_crs(crs(gr_final)) # -> random_points

sf::sf_use_s2(FALSE) # https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
high_adm_join = st_join(high_places, adm_shape); high_adm_join
high_adm_join$name = as.character(high_adm_join$name)

#ggplot() + geom_sf(data = adm_shape) + geom_sf(data = st_jitter(high_adm_join, 0.2), aes(color = population)) + geom_sf_text(data = st_jitter(high_adm_join, 0.2), aes(label = name), check_overlap = TRUE)

ggplot() + geom_sf(data = adm_shape) + geom_sf(data = st_jitter(high_adm_join, 0.2), aes(color = population)) +
  geom_sf_text(data = st_jitter(high_adm_join, 0.2), aes(label = name))

sf::sf_use_s2(TRUE)


# 7. Reclassify the raster file you created in step 1 into 6 categories: 0-500, 500-1000, 1000-1500, 1500-
# 2000, 2000-2500, 2500-3000 meters, and create the corresponding map.

#read file
greece_rast = rast("~/greece_rast.tif")
plot(greece_rast)

# --> #4.3.3 Local operations
#> first to construct a reclassification matrix, where the first column corresponds to the lower and the second column to the upper end of the class. 
rcl = matrix(c(0, 500, 1, 500, 1000, 2, 1000, 1500, 3, 1500, 2000, 4, 2000, 2500, 5, 2500, 3000, 6), ncol = 3, byrow = TRUE)
rcl

recl = classify(greece_rast, rcl = rcl)
plot(recl)


# 8. Take the straight line that connects Veria with Kozani and calculate the altitude profile along this
# line (according to the example of Section 5.4.2 of the book “Geocomputation with R”). Try the
# same for two other cities or points of your choice on the map.

function_quest8 <- function(pairPoints, baseShape){
  
  pair_coor = st_coordinates(pairPoints)#; pair_coor
  
  pair_transect = pair_coor %>% st_linestring() %>%  st_sfc(crs = crs(gr_final)) %>% st_sf()
  pair_transect
  
  pair_transect$id = 1:nrow(pair_transect)
  pair_transect = st_segmentize(pair_transect, dfMaxLength = 250)
  pair_transect = st_cast(pair_transect, "POINT")
  
  pair_transect = pair_transect %>% group_by(id) %>% mutate(dist = st_distance(geometry)[, 1]) 
  
  elevation = terra::extract(gr_final, vect(pair_transect))
  colnames(elevation)[2] <- "height"
  pair_transect = cbind(pair_transect, elevation)
  #pair_transect; View(pair_transect)
  
  
  plot(pair_transect$height)  
  
  g<-ggplot(data = pair_transect, aes(x=ID, y=height)) + geom_line()+ geom_point()
  show(g)
  
  g<-ggplot() + geom_sf(data = baseShape) + geom_sf(data = pair_transect) + geom_sf_text(data = st_jitter(pairPoints, 0.3), aes(label = NAME))  
  show(g)
}


#View(pol_shape)
BerKozPoints = pol_shape[pol_shape$NAME=="Beroia" |pol_shape$NAME=="Kozani", 1:ncol(pol_shape)]
#class(BerKozPoints); BerKozPoints
function_quest8(BerKozPoints, adm_shape)

GreLeivPoints = pol_shape[pol_shape$NAME=="Grebena" |pol_shape$NAME=="Leivadia", 1:ncol(pol_shape)]
#class(GreLeivPoints); GreLeivPoints
function_quest8(GreLeivPoints, adm_shape)


# 9. Search and find other shapefiles and raster files for Greece and create three (3) interesting maps
# that result from operations on vector and raster data similar to those of Sections 3 to 5 of the book
# Geocomputation with R.

#reading and examining the data

file_location = "~/dimoi.shp"
dimoi_shape = read_sf(file_location)
View(dimoi_shape)
dimoi_shape = subset(dimoi_shape, select=-c(gid_0,name_0, gid_1,gid_2,gid_3,type_3,engtype_3, cc_3, hasc_3))
dimoi_shape
plot(dimoi_shape[8])

# a)

dimoi_shape_ele_mean_std = function_quest3(dimoi_shape, gr_cropped)
dimoi_shape_ele_mean_std


# b)

dimoi_shape_ele_mean_std = st_transform(dimoi_shape_ele_mean_std, crs(gr_cropped))
adm_shape_ele_mean_std = st_transform(adm_shape_ele_mean_std, crs(gr_cropped))

sf::sf_use_s2(FALSE) # https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
dimoi_adm_ele_mean_std = st_join(dimoi_shape_ele_mean_std, adm_shape_ele_mean_std)
#dimoi_adm_ele_mean_std

dimoi_adm_ele_mean_std = dimoi_adm_ele_mean_std %>% mutate(diff_means = abs(dimoi_adm_ele_mean_std$height_mean.x - dimoi_adm_ele_mean_std$height_mean.y))
#view(capitals_adm_ele_mean_std)
plot(dimoi_adm_ele_mean_std["diff_means"])

dimoi_adm_ele_mean_std = dimoi_adm_ele_mean_std %>% mutate(diff_stds = abs(dimoi_adm_ele_mean_std$height_std.x - dimoi_adm_ele_mean_std$height_std.y))
#view(capitals_adm_ele_mean_std)
plot(dimoi_adm_ele_mean_std["diff_stds"])

sf::sf_use_s2(TRUE)


# c)

function_quest4(capitals_el, gr_cropped, dimoi_shape_ele_mean_std)

