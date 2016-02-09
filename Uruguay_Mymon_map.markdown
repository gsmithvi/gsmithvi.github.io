---
published: true
status: publish
author: Grace SMith Vidaurre
layout: post
title: Monk parakeets and land use in Uruguay 
---
 
We don't often think of parrots as successful invasive species. The monk parakeet, _Myiopsitta monachus_, is a widespread invader in the Northern hemisphere. Monk parakeets are native to temperate South America, and have been dispersed to other countries by means of the global pet trade. Monks survive winters in Chicago, Brooklyn and Washington State. Recently, SEO/BirdLife volunteers estimated around 20,000 monk parakeets in Spanish cities . 
 
Lately, I've been working on making maps of monk parakeet sightings using Global Biodiversity Information Facility (GBIF) data. Introduced populations are nearly always found in cities or suburbs. I've included some code below to explore how monk parakeets are distributed across different land class categories in Uruguay, a country thought to be the origin for introduced populations. Keep in mind that GBIF sightings will be biased by human reporting.
 
The map I will make here is heavily based off of Oscar Perpiñán Lamigueiro's code to create lattice maps using raster layers: see [link](https://procomun.wordpress.com/2012/02/20/maps_with_r_2/) for Oscar's original code.
 

    # make sure to install packages
    library(maps)

    ## Warning: package 'maps' was built under R version 3.2.3

    ## 
    ##  # ATTENTION: maps v3.0 has an updated 'world' map.        #
    ##  # Many country borders and names have changed since 1990. #
    ##  # Type '?world' or 'news(package="maps")'. See README_v3. #

    library(mapdata)
    library(maptools)  #for shapefiles

    ## Loading required package: sp

    ## Checking rgeos availability: TRUE

    library(scales)  #for transparency
    library(RColorBrewer)
    library(rgeos)

    ## rgeos version: 0.3-15, (SVN revision 515)
    ##  GEOS runtime version: 3.4.2-CAPI-1.8.2 r3921 
    ##  Linking to sp version: 1.2-1 
    ##  Polygon checking: TRUE

    library(rgdal)

    ## Loading required package: methods

    ## rgdal: version: 1.1-3, (SVN revision 594)
    ##  Geospatial Data Abstraction Library extensions to R successfully loaded
    ##  Loaded GDAL runtime: GDAL 1.11.2, released 2015/02/10
    ##  Path to GDAL shared files: /usr/local/Cellar/gdal/1.11.2_2/share/gdal
    ##  Loaded PROJ.4 runtime: Rel. 4.8.0, 6 March 2012, [PJ_VERSION: 480]
    ##  Path to PROJ.4 shared files: (autodetected)
    ##  Linking to sp version: 1.2-1

    library(sp)
    library(rgbif)
    library(raster)

    ## Warning: no function found corresponding to methods exports from 'raster'
    ## for: 'overlay'

    library(rasterVis)

    ## Loading required package: lattice

    ## Loading required package: latticeExtra

    library(colorspace)

    ## 
    ## Attaching package: 'colorspace'

    ## The following object is masked from 'package:raster':
    ## 
    ##     RGB

    library(lattice)
    library(latticeExtra)
    library(grid)
    library(gridExtra)
    library(grDevices)
 
Read in GBIF monk parakeet (Myiopsitta monachus) sightings and Uruguay administrative bouandaries

    mymon <- read.csv("mymon.csv", header = TRUE)
    str(mymon)

    ## 'data.frame':	32644 obs. of  14 variables:
    ##  $ X                : int  3 4 5 6 7 8 9 10 11 12 ...
    ##  $ gbifID           : int  1051269368 1001114892 149527253 149527358 149527381 149527475 149527573 149527705 149527727 149527728 ...
    ##  $ countryCode      : Factor w/ 22 levels "AE","AR","BE",..: 5 2 20 20 20 20 20 20 20 20 ...
    ##  $ decimalLatitude  : num  -16.7 -30.5 26.5 40.6 27.7 ...
    ##  $ decimalLongitude : num  -57.8 -59.5 -80.2 -74 -82.7 ...
    ##  $ genus            : Factor w/ 1 level "Myiopsitta": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ scientificName   : Factor w/ 3 levels "Myiopsitta monachus (Boddaert, 1783)",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ stateProvince    : Factor w/ 178 levels "","A Coruña",..: 102 1 65 117 65 65 48 65 65 79 ...
    ##  $ taxonID          : int  NA 176066 NA NA NA NA NA NA NA NA ...
    ##  $ verbatimLocality : Factor w/ 211 levels "","; Brazil; ; ; ; ; ; ; ; ; Enciozilhada; Faz Sãodose",..: 65 1 1 1 1 1 1 1 1 1 ...
    ##  $ vernacularName   : Factor w/ 3 levels "","Monk Parakeet",..: 1 1 2 2 2 2 2 2 2 2 ...
    ##  $ year             : int  1926 NA 2007 2007 2008 2008 2008 2008 2008 2000 ...
    ##  $ publishingCountry: Factor w/ 10 levels "","AR","BE","CA",..: 10 9 10 10 10 10 10 10 10 10 ...
    ##  $ speciesKey       : int  2479407 2479407 2479407 2479407 2479407 2479407 2479407 2479407 2479407 2479407 ...

    # Uruguay administrative boundaries from http://www.gadm.org/country
    URY_adm0 <- readShapePoly("URY_adm0.shp", proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) # country border
    str(URY_adm0)

    ## Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
    ##   ..@ data       :'data.frame':	1 obs. of  70 variables:
    ##   .. ..$ GADMID    : int 233
    ##   .. ..$ ISO       : Factor w/ 1 level "URY": 1
    ##   .. ..$ NAME_ENGLI: Factor w/ 1 level "Uruguay": 1
    ##   .. ..$ NAME_ISO  : Factor w/ 1 level "URUGUAY": 1
    ##   .. ..$ NAME_FAO  : Factor w/ 1 level "Uruguay": 1
    ##   .. ..$ NAME_LOCAL: Factor w/ 1 level "Uruguay|Rep\xfablica Oriental del Uruguay": 1
    ##   .. ..$ NAME_OBSOL: Factor w/ 0 levels: NA
    ##   .. ..$ NAME_VARIA: Factor w/ 0 levels: NA
    ##   .. ..$ NAME_NONLA: Factor w/ 0 levels: NA
    ##   .. ..$ NAME_FRENC: Factor w/ 1 level "Uruguay": 1
    ##   .. ..$ NAME_SPANI: Factor w/ 1 level "Uruguay": 1
    ##   .. ..$ NAME_RUSSI: Factor w/ 1 level "???????": 1
    ##   .. ..$ NAME_ARABI: Factor w/ 1 level "???????": 1
    ##   .. ..$ NAME_CHINE: Factor w/ 1 level "???": 1
    ##   .. ..$ WASPARTOF : Factor w/ 0 levels: NA
    ##   .. ..$ CONTAINS  : Factor w/ 0 levels: NA
    ##   .. ..$ SOVEREIGN : Factor w/ 1 level "Uruguay": 1
    ##   .. ..$ ISO2      : Factor w/ 1 level "UY": 1
    ##   .. ..$ WWW       : Factor w/ 0 levels: NA
    ##   .. ..$ FIPS      : Factor w/ 1 level "UY": 1
    ##   .. ..$ ISON      : num 858
    ##   .. ..$ VALIDFR   : Factor w/ 1 level "~1900": 1
    ##   .. ..$ VALIDTO   : Factor w/ 1 level "Present": 1
    ##   .. ..$ AndyID    : num 253
    ##   .. ..$ POP2000   : num 3337080
    ##   .. ..$ SQKM      : num 178141
    ##   .. ..$ POPSQKM   : num 18.7
    ##   .. ..$ UNREGION1 : Factor w/ 1 level "South America": 1
    ##   .. ..$ UNREGION2 : Factor w/ 1 level "Americas": 1
    ##   .. ..$ DEVELOPING: num 1
    ##   .. ..$ CIS       : num 0
    ##   .. ..$ Transition: num 0
    ##   .. ..$ OECD      : num 0
    ##   .. ..$ WBREGION  : Factor w/ 1 level "Latin America & Caribbean": 1
    ##   .. ..$ WBINCOME  : Factor w/ 1 level "Upper middle income": 1
    ##   .. ..$ WBDEBT    : Factor w/ 1 level "Severely indebted": 1
    ##   .. ..$ WBOTHER   : Factor w/ 0 levels: NA
    ##   .. ..$ CEEAC     : num 0
    ##   .. ..$ CEMAC     : num 0
    ##   .. ..$ CEPLG     : num 0
    ##   .. ..$ COMESA    : num 0
    ##   .. ..$ EAC       : num 0
    ##   .. ..$ ECOWAS    : num 0
    ##   .. ..$ IGAD      : num 0
    ##   .. ..$ IOC       : num 0
    ##   .. ..$ MRU       : num 0
    ##   .. ..$ SACU      : num 0
    ##   .. ..$ UEMOA     : num 0
    ##   .. ..$ UMA       : num 0
    ##   .. ..$ PALOP     : num 0
    ##   .. ..$ PARTA     : num 0
    ##   .. ..$ CACM      : num 0
    ##   .. ..$ EurAsEC   : num 0
    ##   .. ..$ Agadir    : num 0
    ##   .. ..$ SAARC     : num 0
    ##   .. ..$ ASEAN     : num 0
    ##   .. ..$ NAFTA     : num 0
    ##   .. ..$ GCC       : num 0
    ##   .. ..$ CSN       : num 1
    ##   .. ..$ CARICOM   : num 0
    ##   .. ..$ EU        : num 0
    ##   .. ..$ CAN       : num 0
    ##   .. ..$ ACP       : num 0
    ##   .. ..$ Landlocked: num 0
    ##   .. ..$ AOSIS     : num 0
    ##   .. ..$ SIDS      : num 0
    ##   .. ..$ Islands   : num 0
    ##   .. ..$ LDC       : num 0
    ##   .. ..$ Shape_Leng: num 25.7
    ##   .. ..$ Shape_Area: num 17.1
    ##   .. ..- attr(*, "data_types")= chr [1:70] "N" "C" "C" "C" ...
    ##   ..@ polygons   :List of 1
    ##   .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
    ##   .. .. .. ..@ Polygons :List of 26
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -56 -32.8
    ##   .. .. .. .. .. .. ..@ area   : num 17.1
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:25138, 1:2] -53.9 -53.9 -53.9 -53.9 -53.9 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -53.5 -33.9
    ##   .. .. .. .. .. .. ..@ area   : num 2.17e-05
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:75, 1:2] -53.5 -53.5 -53.5 -53.5 -53.5 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -58.3 -34
    ##   .. .. .. .. .. .. ..@ area   : num 1.28e-05
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:43, 1:2] -58.3 -58.3 -58.3 -58.3 -58.3 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -58.3 -34
    ##   .. .. .. .. .. .. ..@ area   : num 8.72e-06
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:37, 1:2] -58.3 -58.3 -58.3 -58.3 -58.3 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -58.3 -34.1
    ##   .. .. .. .. .. .. ..@ area   : num 0.000174
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:155, 1:2] -58.3 -58.3 -58.3 -58.3 -58.3 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -58.3 -34.1
    ##   .. .. .. .. .. .. ..@ area   : num 9.03e-06
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:29, 1:2] -58.3 -58.3 -58.3 -58.3 -58.3 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -58.3 -34.1
    ##   .. .. .. .. .. .. ..@ area   : num 2.18e-05
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:77, 1:2] -58.3 -58.3 -58.3 -58.3 -58.3 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -58.3 -34.1
    ##   .. .. .. .. .. .. ..@ area   : num 3.09e-06
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -58.3 -58.3 -58.3 -58.3 -58.3 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -53.7 -34.3
    ##   .. .. .. .. .. .. ..@ area   : num 5.55e-06
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:19, 1:2] -53.7 -53.7 -53.7 -53.7 -53.7 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -53.8 -34.4
    ##   .. .. .. .. .. .. ..@ area   : num 4.93e-06
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:17, 1:2] -53.8 -53.8 -53.8 -53.8 -53.8 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -53.7 -34.4
    ##   .. .. .. .. .. .. ..@ area   : num 4.32e-06
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:19, 1:2] -53.7 -53.7 -53.7 -53.7 -53.7 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -53.8 -34.4
    ##   .. .. .. .. .. .. ..@ area   : num 4.32e-06
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] -53.8 -53.8 -53.8 -53.8 -53.8 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -53.8 -34.4
    ##   .. .. .. .. .. .. ..@ area   : num 4.86e-06
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -53.8 -53.8 -53.8 -53.8 -53.8 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -57.9 -34.4
    ##   .. .. .. .. .. .. ..@ area   : num 1.13e-05
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:37, 1:2] -57.9 -57.9 -57.9 -57.9 -57.9 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -57.9 -34.4
    ##   .. .. .. .. .. .. ..@ area   : num 5.25e-06
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] -57.9 -57.9 -57.9 -57.9 -57.9 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -57.9 -34.4
    ##   .. .. .. .. .. .. ..@ area   : num 4.01e-06
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:19, 1:2] -57.9 -57.9 -57.9 -57.9 -57.9 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -57.9 -34.5
    ##   .. .. .. .. .. .. ..@ area   : num 5.4e-06
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -57.9 -57.9 -57.9 -57.9 -57.9 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -57.9 -34.5
    ##   .. .. .. .. .. .. ..@ area   : num 3.67e-05
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:55, 1:2] -57.9 -57.9 -57.9 -57.9 -57.9 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -54.1 -34.7
    ##   .. .. .. .. .. .. ..@ area   : num 6.25e-06
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:28, 1:2] -54.1 -54.1 -54.1 -54.1 -54.1 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -56.4 -34.8
    ##   .. .. .. .. .. .. ..@ area   : num 0.000225
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:169, 1:2] -56.4 -56.4 -56.4 -56.4 -56.4 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.6 -34.8
    ##   .. .. .. .. .. .. ..@ area   : num 7.95e-06
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:43, 1:2] -55.6 -55.6 -55.6 -55.6 -55.6 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -53.9 -34.8
    ##   .. .. .. .. .. .. ..@ area   : num 1.1e-05
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:31, 1:2] -53.9 -53.9 -53.9 -53.9 -53.9 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -56.2 -34.9
    ##   .. .. .. .. .. .. ..@ area   : num 4.7e-06
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:33, 1:2] -56.2 -56.2 -56.2 -56.2 -56.2 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -56.1 -34.9
    ##   .. .. .. .. .. .. ..@ area   : num 5.55e-06
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:31, 1:2] -56.1 -56.1 -56.1 -56.1 -56.1 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.9 -34.9
    ##   .. .. .. .. .. .. ..@ area   : num 5.3e-05
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:119, 1:2] -55.9 -55.9 -55.9 -55.9 -55.9 ...
    ##   .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
    ##   .. .. .. .. .. .. ..@ labpt  : num [1:2] -55 -35
    ##   .. .. .. .. .. .. ..@ area   : num 8.26e-05
    ##   .. .. .. .. .. .. ..@ hole   : logi FALSE
    ##   .. .. .. .. .. .. ..@ ringDir: int 1
    ##   .. .. .. .. .. .. ..@ coords : num [1:126, 1:2] -55 -55 -55 -55 -55 ...
    ##   .. .. .. ..@ plotOrder: int [1:26] 1 20 5 26 25 18 7 2 3 14 ...
    ##   .. .. .. ..@ labpt    : num [1:2] -56 -32.8
    ##   .. .. .. ..@ ID       : chr "0"
    ##   .. .. .. ..@ area     : num 17.1
    ##   ..@ plotOrder  : int 1
    ##   ..@ bbox       : num [1:2, 1:2] -58.4 -35 -53.1 -30.1
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : chr [1:2] "x" "y"
    ##   .. .. ..$ : chr [1:2] "min" "max"
    ##   ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
    ##   .. .. ..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

    # class(URY_adm0)
    # str(URY_adm0@data)
     
    # convert country level data to data frame for mapping
    URY_adm0_df <- fortify(URY_adm0)

    ## Error in eval(expr, envir, enclos): could not find function "fortify"

    str(URY_adm0_df)

    ## Error in str(URY_adm0_df): object 'URY_adm0_df' not found
 
Retain only GBIF observations contained within the country level polygon, using sp::over().
I did this because GBIF observations do not always contain accurate stateProvince information. Also, reading in Uruguay roads as shapefiles. 

    # create SpatialPoints object of monk parakeet observations
    lon <- mymon$decimalLongitude
    lat <- mymon$decimalLatitude
    mat <- as.matrix(cbind(lon, lat))
    p <- SpatialPoints(mat, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
     
    # determine if each sighting is inside or outside of Uruguay country polygon
    URY <- !is.na(over(p, as(URY_adm0, "SpatialPolygons")))
    length(URY)

    ## [1] 32644

    lon.URY.in <- lon[which(URY == TRUE)]
    lat.URY.in <- lat[which(URY == TRUE)]
     
    # create new SpatialPoints object to convert all Uruguay observations to the 
    # same coordinate system as gadm data
    mat2 <- as.matrix(cbind(lon.URY.in, lat.URY.in))
    p2 <- SpatialPoints(mat2, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    # str(p2@coords)
    conv.lon <- p2@coords[, 1]
    conv.lat <- p2@coords[, 2]
    points <- data.frame(lon = conv.lon, lat = conv.lat)
     
    # read in Uruguay roads data
    # from Natural Earth http://www.naturalearthdata.com/downloads/10m-cultural-vectors/
    URYroadsOGR <- readOGR("URY_roads.shp", "URY_roads")

    ## Error in ogrInfo(dsn = dsn, layer = layer, encoding = encoding, use_iconv = use_iconv, : Cannot open file

    str(URYroadsOGR)

    ## Error in str(URYroadsOGR): object 'URYroadsOGR' not found

    URYroads <- spTransform(URYroadsOGR, CRS("+proj=longlat +datum=WGS84"))

    ## Error in spTransform(URYroadsOGR, CRS("+proj=longlat +datum=WGS84")): error in evaluating the argument 'x' in selecting a method for function 'spTransform': Error: object 'URYroadsOGR' not found

    URYroads_df <- fortify(URYroads)

    ## Error in eval(expr, envir, enclos): could not find function "fortify"

    str(URYroads_df)

    ## Error in str(URYroads_df): object 'URYroads_df' not found
 
Read in population and land cover raster layers

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
     
    # checking crop and disaggregation
    # levelplot(pop2, zscaleLog=10, par.settings=BTCTheme)
     
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
 
 
Creating a lattice map object, with land cover layer now scaled by population numbers

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

    ## Error in compareRaster(x, mask): different number or columns

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

    ## Error in pList[[i]]: subscript out of bounds

    # create list of legends
    legendList <- lapply(pList, function(x){
      lg <- x$legend$right
      clKey <- eval(as.call(c(as.symbol(lg$fun), lg$args)))
      clKey
    })

    ## Error in as.symbol(legend$fun): invalid type/length (symbol/0) in vector allocation

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

    ## Error in eval(expr, envir, enclos): object 'legendList' not found

    dev.off()

    ## null device 
    ##           1
 
 
Adding other layers to the trellis object p: country border, roads and GBIF sightings

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

    ## Error: inherits(object, "trellis") is not TRUE

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

    ## Error in arrangeGrob(lGrob, bGrob, mainGrob, nrow = 3, heights = unit(c((h/h) * : object 'mainGrob' not found

    dev.off()

    ## quartz_off_screen 
    ##                 2
