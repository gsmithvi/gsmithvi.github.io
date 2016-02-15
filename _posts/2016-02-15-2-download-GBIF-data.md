---
title: "Download GBIF data for mapping"
layout: post
comments: true
---

{% raw %}

# Download and filter GBIF data for mapping

The [Global Biodiversity Information Facility (GBIF)](http://www.gbif.org)is a compilation of presence data for a broad range of species (absence data may be included). If you're interested in birds, data is funneled into GBIF from eBird, VertNet and other datasets. You can search widely across taxa, but you may be out of luck if you study viruses or microorganisms.  

Accessing data from GBIF only takes creating an account and writing a few lines of code. You can download by species, geographical area or other query words. In the code below, I'll take you through downloading monk parakeet observations and filtering the data prior to mapping. To run the following code, you need to create your own account on GBIF, and install the `rgbif` package.


```r
library(rgbif)

# obtain taxon key for species of interest
key <- name_suggest(q = "Myiopsitta monachus", rank = "species")$key[1]
key # 2479407
```

Since we'll be making maps in later posts, it makes sense to ensure that we're downloading sufficient data. How many Myiopsitta monachus obsvervations have coordinates?

```r
occ_search(taxonKey = key, limit = 20, hasCoordinate = TRUE)
```

Now you can use the taxon key to download GBIF data. The following chunk of code may look funny, since the function names don't correspond to their actual functions. The logic goes like this: 
* once you start the download query, you can check its status
* continue checking until the query status shows up as `SUCCEEDED` 
* then download the zip file pulled by the query 
* import the data to R

You don't have to worry about saving the zip file. Once you import data to R, you can save it at any point as a .csv file in your working directory.  

```r
# insert your GBIF account info to start a query
mp <- occ_download('taxonKey = 2479407', user = "", pwd = "",
                     email = "")

# check query status
occ_download_meta(mp)

# when query is done running, download data
mp <- occ_download_get(mp, overwrite = TRUE)

# import into R and begin manipulating
# ignore warning messages about changing class of certain variables
suppressPackageStartupMessages(library("dplyr"))
mp <- tbl_df(occ_download_import(mp))

# save data to Desktop to avoid downloading from GBIF again
write.csv(mp, paste("Myiopsitta_monachus_GBIF_", Sys.Date(), ".csv", sep = ""))
```

The data is in your hands! It's time to familiarize yourself with the data and prune out any fishy observations. 




```r
mp <- read.csv(paste("Myiopsitta_monachus_GBIF_", Sys.Date(), ".csv", sep = ""), header = TRUE)
```

Here are some basic functions to explore data.

```r
names(mp) # column names
str(mp) # information about variable types and length
head(mp) # display the head of the data frame
View(mp) # viewing data frame in new RStudio tab
```

Some observations may belong to other species entirely, but that isn't the case here.

```r
unique(mp$vernacularName) 
```

```
## [1] Monk Parakeet                                                            
## [2]                                                                          
## [3] Monk Parakeet; Cockatoos; Parrots; Birds; vertebrates; chordates; animals
## [4] Monk Parakeet, Quaker Parrot                                             
## [5] Monk Parakeet (Cliff)                                                    
## [6] MONK PARAKEET                                                            
## [7] munkpapagoi; Monk Parakeet                                               
## 7 Levels:  Monk Parakeet MONK PARAKEET ... munkpapagoi; Monk Parakeet
```

You can use regular expression to remove any observations that may belong to a different species

```r
table(mp$vernacularName)
```

```
## 
##                                                                           
##                                                                      3524 
##                                                             Monk Parakeet 
##                                                                     47267 
##                                                             MONK PARAKEET 
##                                                                         1 
##                                                     Monk Parakeet (Cliff) 
##                                                                         7 
##                                              Monk Parakeet, Quaker Parrot 
##                                                                         2 
## Monk Parakeet; Cockatoos; Parrots; Birds; vertebrates; chordates; animals 
##                                                                        79 
##                                                munkpapagoi; Monk Parakeet 
##                                                                         1
```

```r
mp1 <- mp[grep("luchsi", mp$scientificName, ignore.case = TRUE, invert = TRUE), ]
nrow(mp1) 
```

```
## [1] 50839
```

Coordinate system, necessary to determine points are plotting correctly

```r
table(mp1$verbatimCoordinateSystem)
```

```
## 
##                                 decimal degrees          deg. min. sec. 
##                   50573                      16                      26 
## Degrees Minutes Seconds                     UTM 
##                       5                     219
```

Do observations without a common name have the correct scientific name? 

```r
sn <- mp1[grep("monk parakeet", mp1$vernacularName, ignore.case = TRUE, invert = TRUE), 
               grep("scientificName$", names(mp1))]
unique(sn)
```

```
## [1] Myiopsitta monachus (Boddaert, 1783)              
## [2] Myiopsitta monachus cotorra (Vieillot, 1818)      
## [3] Myiopsitta monachus subsp. monachus               
## [4] Myiopsitta monachus calita (Jardine & Selby, 1830)
## 6 Levels: Myiopsitta luchsi (Finsch, 1868) ...
```

Once you're sure all observations have at least been recorded as the species in question, make the data ready for mapping. 

```r
# remove observations without coordinates (e.g., with NAs)
mp2 <- mp1[which(!is.na(mp1$decimalLatitude) & !is.na(mp1$decimalLongitude)), ]

# quality control
# length(which(is.na(mp1$decimalLatitude) & is.na(mp1$decimalLongitude))) 
# nrow(mp1) - nrow(mp2)

# remove sightings down by Antarctica
mp3 <- mp2[!mp2$decimalLatitude < -50,]

# more quality control
# length(which(mp2$decimalLatitude < -50)) == nrow(mp2) - nrow(mp3)
```


Around fall 2014, GBIF uploaded a whole lot of absence data, which looks really strange when plotted on a map. I was assured by a GBIF data scientist that most absence data will have an `absent` tag associated with the `occurrenceStatus` variable, and should thus be easy to remove. 

```r
table(mp3$occurrenceStatus)
```

```
## 
##                                absent confirmed breeding 
##              47334               2589                 11 
##            present 
##                108
```

```r
# remove data that are actually absences
mp4 <- mp3[grep("absent", mp3$occurrenceStatus, invert = TRUE, ignore.case = TRUE), ]

# quality control
# length(grep("absent", mp3$occurrenceStatus, ignore.case = TRUE))
# nrow(mp3) - nrow(mp4)

# manipulate the data to retain columns useful for mapping
mp5 <- mp4[, grep(paste(c("decimalLatitude", "decimalLongitude", "^genus$", "speciesKey", "vernacularName" , "year", 
                              "taxonID", "scientificName$", "verbatimLocality", "stateProvince", 
                              "gbifID", "verbatimCoordinateSystem", "occurrenceStatus"), 
                            collapse = "|"), names(mp4))]
```

Use the basic functions mentioned above to examine the structure of mp6.

```r
str(mp5)
```

```
## 'data.frame':	47453 obs. of  13 variables:
##  $ gbifID                  : int  593683661 593689298 593701262 593713668 593722845 593748880 593748887 593752425 593755094 593760948 ...
##  $ decimalLatitude         : num  25.7 29.7 32.8 41.2 28 ...
##  $ decimalLongitude        : num  -80.3 -95.4 -96.7 -73.1 -82.7 ...
##  $ genus                   : Factor w/ 1 level "Myiopsitta": 1 1 1 1 1 1 1 1 1 1 ...
##  $ occurrenceStatus        : Factor w/ 4 levels "","absent","confirmed breeding",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ scientificName          : Factor w/ 6 levels "Myiopsitta luchsi (Finsch, 1868)",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ stateProvince           : Factor w/ 257 levels "","A Coruña",..: 94 233 233 71 94 233 233 169 233 149 ...
##  $ taxonID                 : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ verbatimCoordinateSystem: Factor w/ 5 levels "","decimal degrees",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ verbatimLocality        : Factor w/ 329 levels "","; ; ; ; ; ; ;",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ vernacularName          : Factor w/ 7 levels "","Monk Parakeet",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ year                    : int  2011 2011 2011 2011 2011 2011 2011 2011 2011 2011 ...
##  $ speciesKey              : int  2479407 2479407 2479407 2479407 2479407 2479407 2479407 2479407 2479407 2479407 ...
```

```r
names(mp5)
```

```
##  [1] "gbifID"                   "decimalLatitude"         
##  [3] "decimalLongitude"         "genus"                   
##  [5] "occurrenceStatus"         "scientificName"          
##  [7] "stateProvince"            "taxonID"                 
##  [9] "verbatimCoordinateSystem" "verbatimLocality"        
## [11] "vernacularName"           "year"                    
## [13] "speciesKey"
```

```r
nrow(mp5)
```

```
## [1] 47453
```

We are done! After creating a .csv file in the working directory, we can move on to use the data for mapping purposes.

```r
write.csv(mp5, paste("Myiopsitta_monachus_filtered_GBIF_", Sys.Date(), ".csv", sep = ""))
```


{% endraw %}
