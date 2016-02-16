---
title: "Make a time lapse map"
layout: post
comments: true
---

# Time lapse maps in R

{% raw %}

Interactive graphics are an excellent way to show off your research without being too wordy. A former labmate, [Marcelo Araya Salas](http://marceloarayasalas.weebly.com), had been playing around with making movies or gifs in R. I thought it would be neat to create a time lapse map of the species I study (yes, monk parakeets again). I wasn't able to finish this map in time for a poster I presented in January, but I'm happy to be posting the code here and now.  

In this post, I'm making use of the `maps` package. I chose `maps` because I wanted to demonstrate the global extent of monk parakeet invasions, relative to their native range. I prefer the aestethics of `ggmap` whenever possible, but `ggmap` doesn't support maps on a global scale. It should be easy to replace the `maps` code with some from any preferred mapping package. 




```r
library(maps)
library(ggmap)
library(mapdata)
library(maptools) 
library(scales)  
library(RColorBrewer)
library(rgeos)
library(rgdal)
library(sp)
library(animation)
library(pbapply)
library(shapefiles)
library(ggplot2)
library(animation)
```

Let's start by intializing all the information that will be overlaid on the map, including:

* Global Biodiversity Information Facility (GBIF) observations, grouped by time period

* Native distribution polygon

* Genomic research sampling sites

### Prepare GBIF data




```r
mp <- read.csv(paste("Myiopsitta_monachus_filtered_GBIF_", Sys.Date(), ".csv", sep = ""), header = TRUE)
```

Which years are represented in the GBIF data?

```r
table(mp$year)
```

```
## 
## 1909 1920 1923 1924 1925 1926 1928 1936 1937 1940 1945 1957 1960 1961 1965 
##    2    1    2    4   18    6    6    3    7    1    2    9   27   28    1 
## 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 
##    1    2    2    5    4    7    4    4    4    6   10    9  159  133    9 
## 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 
##   12   15   26   36   42   68   76   40   73  115  121   98   68  102  242 
## 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 
##  246  327  321  543  709  697  761 1128 1597 1726 2533 3576 4081 5483 6809 
## 2014 2015 
##   82   78
```

```r
length(table(mp$year))
```

```
## [1] 62
```

I'm going to move forwards by keeping the original years as time periods, rather than grouping observations into a smaller subset of time periods. This means the final gif will be composed of 62 frames, which sounds like a lot of files to create. Fortunately, we can customize the code to delete these intermediate frames once they've been incorporated into the final gif. Here, the GBIF data have to be ordered by year. As you'll see below, the variable `time.periods` will be used as the indexing variable to create each map within a loop. 

```r
time.periods <- unique(mp$year[!is.na(mp$year)])[order(unique(mp$year[!is.na(mp$year)]), decreasing = FALSE)] 
time.periods
```

```
##  [1] 1909 1920 1923 1924 1925 1926 1928 1936 1937 1940 1945 1957 1960 1961
## [15] 1965 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981
## [29] 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995
## [43] 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009
## [57] 2010 2011 2012 2013 2014 2015
```

I'm also creating latitude and longitude vectors that will be used to intialize the map prior to overlaying data. This is a good trick when using the `maps` package for mapping, which requires a map intialization step prior to the actual mapping. 

```r
map.lat <- mp$decimalLatitude
map.lon <- mp$decimalLongitude
```

### Prepare *Myiopsitta monachus* species distribution polygon

This code creates an object of class `SpatialPolygonsDataFrame`, which is easy to overlay on most maps. The tricky part here is that this species distribution contains polygons in the introduced range, which I want to exlcude. In a previous post (`ggmapping`), these introduced range polygons were automatically excluded by simply creating a map in the Southern hemisphere. Since I'm creating a global map, I have to manually filter only the Souther hemisphere polygon.

I got lucky here - it was easy to exclude the introduced range polygons based on size. The code below will be unfamiliar if you've never heard of S4 objects. If so, check out [Hadley Wickham's useful explanation](http://adv-r.had.co.nz/S4.html). 

Most objects you manipulate in R are S3 objects. S4 objects are more rigid and have specific methods for indexing, etc. The structure of the following S4 polygon seems extremely complex (slots within lists and vice versa), and it can be a pain when figuring out how to index these things for the first time. For the `SpatialPolygonsDataFrame` object below, it helps if you think about it as nothing more than a data frame with 2 dimensions (rows and columns).



```r
mp.distr <- readShapePoly("Myiopsitta_monachus_1608_NS.shp") 
# str(mp.distr)
class(mp.distr)
```


```r
str(mp.distr@data) # this data frame contains metadata, but no regional information
```

```
## 'data.frame':	3 obs. of  14 variables:
##  $ SPCRECID : int  1608 1608 1608
##  $ DATE_    : Factor w/ 1 level "23/11/2006": 1 1 1
##  $ SCINAME  : Factor w/ 1 level "Myiopsitta monachus": 1 1 1
##  $ SOURCE   : Factor w/ 3 levels "Cramp, 1997 ; Ridgley, 2003 ; del Hoyo 1997; Juniper & Parr, 1998",..: 1 3 2
##  $ PRESENCE : int  1 4 1
##  $ ORIGIN   : int  3 3 1
##  $ SEASONAL : int  1 1 1
##  $ DATA_SENS: Factor w/ 0 levels: NA NA NA
##  $ SENS_COMM: Factor w/ 0 levels: NA NA NA
##  $ COMPILER : Factor w/ 0 levels: NA NA NA
##  $ TAX_COM  : Factor w/ 0 levels: NA NA NA
##  $ DIST_COM : Factor w/ 0 levels: NA NA NA
##  $ REVIEWERS: Factor w/ 1 level "Ridgely, 2002": NA NA 1
##  $ CITATION : Factor w/ 1 level "Ridgely et al. and BirdLife International (2012) Digital Distribution Maps of the Birds of the Western Hemisphere, version 5.0 "| __truncated__: 1 1 1
##  - attr(*, "data_types")= chr  "N" "C" "C" "C" ...
```

```r
str(mp.distr@polygons) # yup, it's complicated
```

```
## List of 3
##  $ :Formal class 'Polygons' [package "sp"] with 5 slots
##   .. ..@ Polygons :List of 26
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -61.7 16.2
##   .. .. .. .. ..@ area   : num 0.0735
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:248, 1:2] -61.7 -61.7 -61.7 -61.7 -61.7 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -61.4 16.3
##   .. .. .. .. ..@ area   : num 0.0507
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:213, 1:2] -61.5 -61.5 -61.5 -61.5 -61.4 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -66.5 18.2
##   .. .. .. .. ..@ area   : num 0.75
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:1021, 1:2] -67.1 -67.1 -67.1 -67.1 -67.1 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -66.5 18.2
##   .. .. .. .. ..@ area   : num 0.334
##   .. .. .. .. ..@ hole   : logi TRUE
##   .. .. .. .. ..@ ringDir: int -1
##   .. .. .. .. ..@ coords : num [1:26, 1:2] -67 -67 -67 -66.9 -66.8 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -81.4 19.3
##   .. .. .. .. ..@ area   : num 0.00177
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:32, 1:2] -81.4 -81.4 -81.4 -81.4 -81.4 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -80.9 25.2
##   .. .. .. .. ..@ area   : num 1.56e-05
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:6, 1:2] -80.9 -80.9 -80.9 -80.9 -80.9 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -81.6 25.9
##   .. .. .. .. ..@ area   : num 4.8e-05
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:5, 1:2] -81.6 -81.6 -81.6 -81.6 -81.6 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -81.6 25.9
##   .. .. .. .. ..@ area   : num 5.53e-05
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:6, 1:2] -81.6 -81.6 -81.6 -81.6 -81.6 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -82.2 26.9
##   .. .. .. .. ..@ area   : num 1.79e-05
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:7, 1:2] -82.2 -82.2 -82.2 -82.2 -82.2 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -82.6 27.9
##   .. .. .. .. ..@ area   : num 5.41e-05
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:7, 1:2] -82.6 -82.6 -82.6 -82.6 -82.6 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -81.3 27.1
##   .. .. .. .. ..@ area   : num 6.08
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:1176, 1:2] -82.7 -82.1 -82 -81.9 -81.8 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -95.3 29.5
##   .. .. .. .. ..@ area   : num 0.0608
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:398, 1:2] -95.3 -95.3 -95.3 -95.3 -95.3 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -97.7 29.7
##   .. .. .. .. ..@ area   : num 0.208
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:361, 1:2] -97.7 -97.7 -97.7 -97.7 -97.7 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -81.6 30
##   .. .. .. .. ..@ area   : num 0.246
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:27, 1:2] -81.3 -81.2 -81.3 -81.5 -81.7 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -90.2 30.2
##   .. .. .. .. ..@ area   : num 0.142
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:396, 1:2] -90 -90 -90 -89.9 -89.9 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -97.7 31.3
##   .. .. .. .. ..@ area   : num 0.0616
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:361, 1:2] -97.7 -97.7 -97.7 -97.7 -97.7 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -102.4 31.7
##   .. .. .. .. ..@ area   : num 0.0616
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:361, 1:2] -102 -102 -102 -102 -102 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -105.8 31.7
##   .. .. .. .. ..@ area   : num 0.124
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:361, 1:2] -106 -106 -106 -106 -106 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -97.5 32.5
##   .. .. .. .. ..@ area   : num 0.0466
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:361, 1:2] -97.5 -97.5 -97.5 -97.5 -97.5 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -89.6 34.4
##   .. .. .. .. ..@ area   : num 0.0716
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:361, 1:2] -89.6 -89.6 -89.6 -89.6 -89.6 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -76.2 36.8
##   .. .. .. .. ..@ area   : num 0.124
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:255, 1:2] -76.2 -76.2 -76.2 -76.2 -76.2 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -75.2 38.7
##   .. .. .. .. ..@ area   : num 0.0118
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:365, 1:2] -75.2 -75.2 -75.2 -75.2 -75.2 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -73.7 41
##   .. .. .. .. ..@ area   : num 1.4e-05
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:5, 1:2] -73.7 -73.7 -73.7 -73.7 -73.7 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -73.8 40.8
##   .. .. .. .. ..@ area   : num 0.367
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:271, 1:2] -73.7 -73.7 -73.7 -73.7 -73.7 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -71.8 41.5
##   .. .. .. .. ..@ area   : num 0.0389
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:361, 1:2] -71.8 -71.8 -71.8 -71.8 -71.8 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -87.9 41.9
##   .. .. .. .. ..@ area   : num 0.226
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:33, 1:2] -87.9 -88 -88 -88.1 -88.1 ...
##   .. ..@ plotOrder: int [1:26] 11 3 24 4 14 26 13 15 18 21 ...
##   .. ..@ labpt    : num [1:2] -81.3 27.1
##   .. ..@ ID       : chr "0"
##   .. ..@ area     : num 8.75
##  $ :Formal class 'Polygons' [package "sp"] with 5 slots
##   .. ..@ Polygons :List of 1
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -76.3 25.1
##   .. .. .. .. ..@ area   : num 0.0405
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:634, 1:2] -76.7 -76.7 -76.7 -76.7 -76.7 ...
##   .. ..@ plotOrder: int 1
##   .. ..@ labpt    : num [1:2] -76.3 25.1
##   .. ..@ ID       : chr "1"
##   .. ..@ area     : num 0.0405
##  $ :Formal class 'Polygons' [package "sp"] with 5 slots
##   .. ..@ Polygons :List of 14
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -63.6 -42.4
##   .. .. .. .. ..@ area   : num 0.000254
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:13, 1:2] -63.6 -63.6 -63.6 -63.6 -63.6 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -64.1 -41
##   .. .. .. .. ..@ area   : num 4.38e-05
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:7, 1:2] -64.1 -64.1 -64.1 -64.1 -64.1 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -62.3 -39
##   .. .. .. .. ..@ area   : num 0.000138
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:12, 1:2] -62.3 -62.3 -62.3 -62.3 -62.3 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -62.3 -39
##   .. .. .. .. ..@ area   : num 8.35e-05
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:7, 1:2] -62.3 -62.3 -62.3 -62.3 -62.3 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -62.3 -39
##   .. .. .. .. ..@ area   : num 9.08e-05
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:6, 1:2] -62.3 -62.3 -62.3 -62.3 -62.3 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -62.3 -38.8
##   .. .. .. .. ..@ area   : num 4.72e-05
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:7, 1:2] -62.3 -62.3 -62.3 -62.3 -62.3 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -58.5 -33.9
##   .. .. .. .. ..@ area   : num 7.21e-05
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:8, 1:2] -58.5 -58.5 -58.5 -58.5 -58.5 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -58.3 -33.1
##   .. .. .. .. ..@ area   : num 0.000116
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:9, 1:2] -58.2 -58.2 -58.3 -58.3 -58.3 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -58 -32.9
##   .. .. .. .. ..@ area   : num 7.09e-06
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:5, 1:2] -58 -58 -58 -58 -58 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -50.7 -30.4
##   .. .. .. .. ..@ area   : num 3.62e-07
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:4, 1:2] -50.7 -50.7 -50.7 -50.7 -30.4 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -68.2 -29.7
##   .. .. .. .. ..@ area   : num 0.000463
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:4, 1:2] -68.2 -68.2 -68.2 -68.2 -29.6 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -68.1 -29.5
##   .. .. .. .. ..@ area   : num 0.000465
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:4, 1:2] -68.2 -68.1 -68.1 -68.2 -29.6 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -65.7 -18.1
##   .. .. .. .. ..@ area   : num 6.29
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:51, 1:2] -65.2 -65.4 -65.6 -65.8 -66 ...
##   .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
##   .. .. .. .. ..@ labpt  : num [1:2] -60.8 -29.9
##   .. .. .. .. ..@ area   : num 258
##   .. .. .. .. ..@ hole   : logi FALSE
##   .. .. .. .. ..@ ringDir: int 1
##   .. .. .. .. ..@ coords : num [1:4845, 1:2] -56.2 -56.3 -56.5 -56.6 -56.6 ...
##   .. ..@ plotOrder: int [1:14] 14 13 12 11 1 3 8 5 4 7 ...
##   .. ..@ labpt    : num [1:2] -60.8 -29.9
##   .. ..@ ID       : chr "2"
##   .. ..@ area     : num 264
```

I resorted to extracting the largest polygon in the `SpatialPolygonsDataFrame`, because regional information isn't available per polygon. Again, it was just lucky that I happened to be interested in the largest polygon. You may run into situations that require more customization, and if so, hopefully those polygons will contain more information that can be used for indexing. Note the use of `@` indexing to extract specific slots of the S4 object. 

The largest polygon happens to the the third in the list, as we learn from using `sapply`. The third polygon can be pulled out by simple square bracket indexing.

```r
sapply(slot(mp.distr, "polygons"), function(x) slot(x, "area"))

class(mp.distr[3, ]) # correct class for plotting

# filtering South American polygon
mp.distr2 <- fortify(mp.distr[3, ])
mp.distr2 <- mp.distr2[mp.distr2$lat <= 0, ] # removing any points in the Eastern hemisphere
```

### Download sampling site coordinates

Here's a cool function from `ggmap`, called `geocode`. This function allows you to search for locations by name in Google databases, and returns geographic coordinates that you can use for mapping. I've included sites where invasive or native samples were collected and used in restriction enzyme associated DNA sequencing (RAD-seq). 

```r
RAD_seq <- c("Algarrobo, Argentina", "Entre Rios, Argentina", "Buenos Aires, Argentina", "Parque Luro, Argentina","Madrid, Spain", "Zaragoza, Spain", "Barcelona, Spain", "Sevilla, Spain", "Mallorca, Spain", "Gran Canaria, Canary Islands", "Connecticut", "Illinois", "Washington State", "Florida")

coords <- geocode(RAD_seq, source = "google")

RAD_locs <- data.frame(RAD_seq, coords)
```

### Create the time lapse map 

The `maps` code below (using functions `map`, `rect`, `abline` amd `mtext`) is modified from my former labmate's code in [warbleR](https://cran.r-project.org/web/packages/warbleR/index.html), a package we co-developed for bioacoustic analyses (see the function `xcmaps`). Marcelo has a [great blog](http://marceloarayasalas.weebly.com) for anyone interested in bioacoustic analyses in R.  

An important disclaimer: as I'm interested in making a map for visualization purposes, I'm not being super careful about projections and coordinate systems of the different data used here (see argument `proj4string` in `SpatialPoints`). If you use similar data for quantitative purposes, you absolutely need to be sure that your coordinate systems/projections are all the same. Nearly all the points I'm mapping generally fall where expected. However, if the majority of GBIF sightings were being plotted in the Atlantic Ocean, that would be an indication for major changes to the coordinate system/projection. 

Moving on, the basic idea below is to:

- write a loop to create a map per desired time period

- wrap this expression in a function from package `animation` to create a gif 

I've included some tricks below that you might find useful:

+ grouping GBIF sightings by native or introduced status, using `sp::over` to determine which South American sightings fall _outside_ the native distributions

+ colors conditional on native or invasive status

+ map border color conditional on the year monk parakeets invade the Northern hemisphere

+ `alpha` argument inside color palettes controls transparency, to better visualize build-up of plotted points

+ removal of intermediate maps once incorporated into the final gif

+ pbsapply adds a progress bar


```r
ext <- 5 # extra space around map limits specified below

saveGIF(expr = 
          
pbsapply(1:length(time.periods), function(x){

# subset datset such that data by year builds up as gif progresses
tmp <- mp[which(mp$year <= time.periods[x]), ]
lat <- tmp$decimalLatitude
lon <- tmp$decimalLongitude

# divide lat and lon by invasive / South American status
lat.inv <- lat[lat >= 0]
lon.inv <- lon[which(lat >= 0)]

lat.SA <- lat[lat <= 0]
lon.SA <- lon[which(lat <= 0)]

# South American sightings outside the native distribution should be colored as invasive
# need SpatialPoints object, rather than polygon
mat <- as.matrix(cbind(lon.SA, lat.SA))
p <- SpatialPoints(mat, proj4string = CRS(as.character(NA)))

nat.status <- !is.na(over(p, as(mp.distr[3, ], "SpatialPolygons")))

lon.SA.in <- lon.SA[which(nat.status == TRUE)]
lat.SA.in <- lat.SA[which(nat.status == TRUE)]
lon.SA.out <- lon.SA[which(nat.status == FALSE)]
lat.SA.out <- lat.SA[which(nat.status == FALSE)]  
  
# the package maps requires that you create an empty map first 
map("world", xlim = c(min(map.lon) - ext, max(map.lon) + ext),
    ylim = c(min(map.lat) - ext, max(map.lat) + ext), interior = FALSE, fill = FALSE)

# par("usr") specifies plot boundaries using the limits of the data itself
# here we create a gridded rectangle as the background for the map
rect(par("usr")[1], par("usr")[3], par("usr")[2], 
     par("usr")[4], col = cm.colors(20)[7])
abline(h = seq(-90, 90, 5), col = "white", lwd = 0.9)
abline(h = seq(-90, 90, 5), col = "white", lwd = 1.1)
abline(v = seq(-180, 180, 5), col = "white", 
       lwd = 0.9)
abline(v = seq(-180, 180, 5), col = "white", 
       lwd = 1.1)

map("world", xlim = c(min(map.lon) - ext, max(map.lon) + ext), add = TRUE,
    ylim = c(min(map.lat) - ext, max(map.lat) + ext), fill = TRUE,
    col = terrain.colors(12)[8]) 

mtext("Longitude (DD)", side = 1, line = 0.5, cex = 1.5)
mtext("Latitude (DD)", side = 2, line = 0.5, cex = 1.5)
mtext(paste(c("Myiopsitta monachus", "\n", min(time.periods), "-", time.periods[x]), collapse = ""), side = 3, line = 0.5, cex = 2.5)

# plot species distribution
plot(mp.distr[3, ], add = TRUE, xlim = c(min(lon.SA) - ext, max(lon.SA) + ext), 
     ylim = c(min(lat.SA) - ext, max(lat.SA) + ext), 
     col = terrain.colors(10, alpha = 0.5)[4], border = terrain.colors(10)[2],
     lwd = 2)

# color points by status, if present for time period
if(length(lon.inv) != 0 & length(lat.inv) != 0){
points(lon.inv, lat.inv, pch = 21, cex = 1.5, col = "black", lwd = 0.25,
       bg = heat.colors(10, alpha = 0.8)[3])
}

if(length(lon.SA.out) != 0 & length(lat.SA.out) != 0){
points(lon.SA.out, lat.SA.out, pch = 21, cex = 1.5, col = "black", lwd = 0.25,
       bg = heat.colors(10, alpha = 0.8)[3])
}
  
if(length(lon.SA.in) != 0 & length(lat.SA.in) != 0){
points(lon.SA.in, lat.SA.in, pch = 21, cex = 1.5, col = "black", lwd = 0.25,
       bg = topo.colors(10, alpha = 0.8)[3])
}

# add points for RAD sampling regions
if(time.periods[x] == 2015){
points(RAD_locs$lon, RAD_locs$lat, pch = 21, cex = 4, 
       col = heat.colors(10)[7], lwd = 2,
       # bg = heat.colors(10, alpha = 0.5)[8])
       bg = alpha("goldenrod1", 0.7))
}

# color border by time period in which monks were introduced
if(time.periods[x] >= 1961){
  box(which = "plot", lty = "solid", col = heat.colors(10)[3], lwd = 6)
} else {
  box(which = "plot", lty = "solid", col = topo.colors(10)[3], lwd = 6)
}

# add legend
if(time.periods[x] >= 1961){
  box.col <- heat.colors(10)[3]
} else {
  box.col <- topo.colors(10)[3]
}

ind <- mp$year <= time.periods[2]

legend(x = max(mp$decimalLongitude[ind], na.rm = TRUE) + 40, y = min(mp$decimalLatitude[ind], na.rm = TRUE) + ext/10, 
       legend = c("Introduced sighting", "Native sighting", "Native distribution", "RAD-seq sampling site"), 
       pt.bg = c(heat.colors(10, alpha = 0.8)[3], topo.colors(10, alpha = 0.8)[3], 
               terrain.colors(10, alpha = 0.5)[4], alpha("goldenrod1", 0.7)),
       col = c("black", "black", terrain.colors(10)[2], heat.colors(10)[7]),
       pch = rep(21, 4), bty = "o", bg = topo.colors(12, alpha = 0.2)[3], box.lty = "solid", box.lwd = 3, box.col = box.col, cex = 1, ncol = 2, pt.cex = 2)

# add line for equator
abline(h = 0, col = "grey", lwd = 2.5, lty = "dashed")

# add scale to map
# map.scale(ratio = FALSE, relwidth = 0.2, x = max(lon.inv) - ext*2, 
#           y = min(lat.inv) + ext*2, cex = 0.5)

}), movie.name = "mymon-time-lapse.gif", img.name = "Mymon_TL_images", clean = TRUE, ani.height = 1000, ani.width = 800, interval = 1)

dev.off()
```

<img src="/images/mymon-time-lapse.gif" width="1200" height ="1200" />

{% endraw %}

