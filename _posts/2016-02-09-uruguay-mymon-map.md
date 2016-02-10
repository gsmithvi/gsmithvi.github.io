---
title: "Making trellis maps in R with raster layers"
layout: post
comments: true
---

# Making trellis maps in R with raster layers

{% raw %}

We don't often think of parrots as successful invasive species. The monk parakeet, _Myiopsitta monachus_, is a widespread invader in the Northern hemisphere. Monk parakeets are native to temperate South America, and have been dispersed to other countries by means of the global pet trade. Monks survive winters in Chicago, Brooklyn and Washington State. Recently, SEO/BirdLife volunteers estimated around 20,000 monk parakeets in Spanish cities. 

I've been working on maps of monk parakeet sightings using Global Biodiversity Information Facility (GBIF) data. Introduced populations are nearly always found in cities or suburbs. I've included some code below to explore how monk parakeets are distributed across different land class categories in Uruguay, a country thought to be the origin for introduced populations. Keep in mind that GBIF sightings will be biased by human reporting.

The map I will make here is heavily based off of Oscar Perpiñán Lamigueiro's code to create lattice maps using raster layers: see [Oscar's blog](https://procomun.wordpress.com/2012/02/20/maps_with_r_2/) for his original code.





```r
# make sure to install packages
library(maps)
library(mapdata)
library(ggmap)
library(ggplot2)
library(maptools)  #for shapefiles
library(scales)  #for transparency
library(RColorBrewer)
library(rgeos)
library(rgdal)
library(sp)
library(rgbif)
library(raster)
library(rasterVis)
library(colorspace)
library(lattice)
library(latticeExtra)
library(grid)
library(gridExtra)
library(grDevices)
```

Read in GBIF monk parakeet (Myiopsitta monachus) sightings and Uruguay administrative bouandaries

```r
mymon <- read.csv("mymon.csv", header = TRUE)

# Uruguay administrative boundaries from http://www.gadm.org/country
URY_adm0 <- readShapePoly("URY_adm0.shp", proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) # country border

# convert country level data to data frame for mapping
URY_adm0_df <- fortify(URY_adm0)
```

Retain only GBIF observations contained within the country level polygon, using \code{sp::over()}.
I did this because GBIF observations do not always contain accurate stateProvince information. Also, reading in Uruguay roads as shapefiles. 

```r
# create SpatialPoints object of monk parakeet observations
lon <- mymon$decimalLongitude
lat <- mymon$decimalLatitude
mat <- as.matrix(cbind(lon, lat))
p <- SpatialPoints(mat, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# determine if each sighting is inside or outside of Uruguay country polygon
URY <- !is.na(over(p, as(URY_adm0, "SpatialPolygons")))

lon.URY.in <- lon[which(URY == TRUE)]
lat.URY.in <- lat[which(URY == TRUE)]

# create new SpatialPoints object to convert all Uruguay observations to the 
# same coordinate system as gadm data
mat2 <- as.matrix(cbind(lon.URY.in, lat.URY.in))
p2 <- SpatialPoints(mat2, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

conv.lon <- p2@coords[, 1]
conv.lat <- p2@coords[, 2]
points <- data.frame(lon = conv.lon, lat = conv.lat)

# read in Uruguay roads data
# from Natural Earth http://www.naturalearthdata.com/downloads/10m-cultural-vectors/
URYroadsOGR <- readOGR("URY_roads.shp", "URY_roads")
str(URYroadsOGR)
URYroads <- spTransform(URYroadsOGR, CRS("+proj=longlat +datum=WGS84"))
URYroads_df <- fortify(URYroads)
str(URYroads_df)
```

Read in and crop global population raster

```r
# download global population and land cover MODIS data as GEOTiff
# http://neo.sci.gsfc.nasa.gov

# bounding box for Uruguay
b <- extent(-59, -53, -35.5, -29.5)

# read in population data and crop to fit Uruguay
pop <- raster("SEDAC_POP_2000-01-01_rgb_3600x1800.FLOAT-2.TIFF")
pop <- crop(pop, b)
pop[pop == 99999] <- NA

# disaggregating raster to create higher resolution (smaller cells), 
# for a less pixelated map, this doesn't work well for the land cover raster
pop2 <- disaggregate(pop, fact = 10, method = "bilinear")
```

![map](./images/uruguay-pop.png) 

Read in and crop global land cover raster layers

```r
# I'm only using a handful of land cover categories for Uruguay
# Check out all categories:
# http://eoimages.gsfc.nasa.gov/images/news/NasaNews/ReleaseImages/LCC/Images/lcc_key.jpg

# read in land cover data and crop to fit Uruguay
landClass <- raster("LandClass_RenderData")
landClass <- crop(landClass, b)
landClass[landClass %in% c(0, 254)] <- NA

# classify by categories found in Uruguay
landClass <- cut(landClass, c(8, 10, 11, 12, 13))

# add a Raster Atribute Table (RAT) and define the raster as categorical data
landClass <- ratify(landClass)

# configure the RAT: 
# 1) create a RAT data.frame using the levels method
# 2) set the values for each class (to be used by levelplot)
# 3) assign this RAT to the raster 
rat <- levels(landClass)[[1]]

# name the RAT classes by land cover category
rat$classes <- c("GRASSLANDS", "WETLANDS", "CROPLANDS", "URBAN")
levels(landClass) <- rat

# specify limits of population scale (z-scale), which will be used to scale population data
# lowering max limit of population scale, since Uruguay is not densely populated
# at <- pTotal$legend$bottom$args$key$at # from original code
at <- seq(pop2@data@min, pop2@data@max, pop2@data@max/16)

# classes <- levels(factor(whichMax)) # from original code
nClasses <- nrow(rat)
```

![map](./images/uruguay-landcover.png) 


Creating a lattice map object, with land cover layer now scaled by population numbers

```r
# initialize pList, a list of levelplots
pList <- NULL

# initializing even and odd vectors to assign diversifying colors to adjacent land classes
ev <- c(seq(2, nClasses, 2))
od <- c(seq(1, nClasses, 2))

# looping over land classes to create list of levelplots 
pList <- lapply(1:nClasses, function(i){
  
  landSub <- landClass
  
  # assign NAs to all land classes that are not pulled out by the current iteration of i
  landSub[!(landClass == rat$ID[i])] <- NA 

  popSub <- mask(pop2, landSub) # mask population by land classes
  
  # color land class categories
  step <- 180/nClasses
  if(grepl("URB", rat$classes[i])){ # making urban category stand out
    pal <- rev(heat_hcl(28, h = 0,
               c. = c(80,30), l = c(30,90), power = c(1/5,1.5)))[-c(1:10)]
    }else if(i %in% ev){ 
    pal <- rev(terrain_hcl(28, h = (30 + step*(i-1))%%360,
               c. = c(65,0), l = c(45,95), power = c(1/3, 1.5)))[-c(1:10)]
  }else if(i %in% od){ 
    pal <- rev(heat_hcl(28, h = (90 + step*(i-1))%%360,
               c. = c(80,30), l = c(30,90), power = c(1/5,1.5)))[-c(1:10)]
  }
  
  # creating levelplot for the current land class
  pClass <- levelplot(popSub, at=at,
                      col.regions=pal, margin=FALSE,
                      labels = list(labels = c(ceiling(at)), cex = 3), 
                      xlab = list(label = "Longitude", cex = 3),
                      ylab = list(label= "Latitude", cex = 3),
                      scales = list(cex = 2))
})

# collapsing levelplots into one 
p <- Reduce('+', pList) # only one legend appears

cols <- 1
rows <- 2

# write function to add a title below each legend
addTitle <- function(legend, title){

  # the urban legend ends up on the right hand side,
  # and the text gets skewed, so I'm treating this label differently
  if(grepl("URB", title)){ 
    titleGrob <- textGrob(title, gp=gpar(fontsize=25), rot = 270, x = 0.1, y = 0.9,
                          just = "left")
  } else{
    titleGrob <- textGrob(title, gp=gpar(fontsize=25), rot = 270, y = 0.9,
                          just = "left")
  }
  
  legendGrob <- eval(as.call(c(as.symbol(legend$fun), legend$args)))
  
  # specifying one column and two rows for the legend and title below
  ly <- grid.layout(ncol=cols, nrow=rows, heights=unit(c(0.8,0.2), rep('npc', cols)), 
                    widths=unit(rep(0.1,rows), rep('npc', rows)))
  
  # packing legends with titles into a frame Grob
  fg <- frameGrob(ly, name=paste('legendTitle', title, sep='_'))
  pg <- packGrob(fg, legendGrob, col=1, row=1)
  pg <- packGrob(pg, titleGrob, col=1, row=2)

}

# run addTitle() for each legend that needs it 
for (i in 1:nClasses){
  lg <- pList[[i]]$legend$right
  lg$labels <- ceiling(at)
  lg$args$key$labels$cex = ifelse(i==nClasses, 2, 0) # only the last legend needs labels
  pList[[i]]$legend$right <- list(fun='addTitle',
                                  args=list(legend=lg, title=rat$classes[i]))
}

# create list of legends
legendList <- lapply(pList, function(x){
  lg <- x$legend$right
  clKey <- eval(as.call(c(as.symbol(lg$fun), lg$args)))
  clKey
})
 
# create a function to collapse the list into a single legend
# adapted from latticeExtra::: mergedTrellisLegendGrob
packLegend <- function(legendList){
  N <- length(legendList)
  ly <- grid.layout(nrow = 1,  ncol = N)
  g <- frameGrob(layout = ly, name = "mergedLegend")
  for (i in 1:N) g <- packGrob(g, legendList[[i]], col = i)
  g
}
 
# Now, p's legend includes all the legends
p$legend$right <- list(fun = 'packLegend',  args = list(legendList = legendList))

dev.off()
```


Adding other layers to the trellis object p: country border, roads and GBIF sightings

```r
# creating another legend for these additional layers
lGrob <- legendGrob(labels = c("URY border", "URY roads",
                               expression(italic(M.monachus )~sightings)), 
                    nrow = 1, ncol = 3,
                    default.units = "lines", pch = 21, 
                    gp = gpar(fill = c(diverge_hcl(12, h = 246,40, c=96)[1],
                              diverge_hcl(12, h = 246,40, c=96)[6],
                    heat.colors(12, alpha = 0.6)[9]), 
                    col = c("black", "black",
                    heat.colors(12)[3]), cex = 2))

# blank Grob will go between legend and main Grob
bGrob <- rectGrob(gp = gpar(col = "white"))

# add country polygon, road lines and GBIF points to p
mainGrob <- p + 
  latticeExtra::layer(sp.polygons(URY_adm0, lwd = 4, col = diverge_hcl(12, h = 246,40, c=96)[1])) +
  latticeExtra::layer(sp.lines(URYroadsOGR, lwd = 2, col = diverge_hcl(12, h = 246,40, c=96)[6])) +
  latticeExtra::layer(panel.points(points, pch = 21, cex = 2.5, fill = heat.colors(12, alpha = 0.6)[9], col = heat.colors(12)[3]))

# create a legend for the population zscale
labGrob <- textGrob(expression("Population density" (persons/km^{2})), 
                    default.units = "lines", rot = 270, gp = gpar(fontsize = 27))

h <- 40
w <- 40

# create image file, this can also be done with an x11() window
trellis.device('png', file = "Mymon_Uruguay_landcover.png", units = "cm", height = h,
               width = w, res = 600)
grid.arrange(arrangeGrob(lGrob, bGrob, mainGrob, nrow = 3, heights = unit(c((h/h)*2, h/(h*10), h - h/h - h/h*2), units = 'cm')), labGrob, ncol = 2, 
             widths = unit(c(35, 2), units = 'cm'))
```

![map](./images/mymon-uruguay-landcover.png) 

{% endraw %}

