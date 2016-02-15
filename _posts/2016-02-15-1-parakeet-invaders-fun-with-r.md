---
title: "Parakeet invaders and fun with R"
layout: post
comments: true
---

# Biological invasions, monk parakeets and coding 

How do species evolve invasive traits? Do species appropriate pre-existing traits to become successful invaders? What roles do human human disturbance and phenotypic plasticity play in successful biological invasions? These are long-standing questions in the field of invasion biology. In my PhD research, I'm addressing these questions by studying an invasive parakeet species.

<img src="/images/mp1.jpg" width="300" height ="300" />

Photo cred: brooklynparrot @ brooklynparrots.com

The order *Psittaciformes* contains some of the highest numbers of endangered avian species. On the other hand, we see more parrot invaders than expected by chance. Monk parakeets (*Myiopsitta monachus*), native to South America, have been dispersed across the Northern hemisphere by the global pet trade. 

We don't often think of parrots as successful invasive species, but monk parakeets are widely successful across their introduced range, even in areas colder than native habitat. Native populations are thought to have expanded their range in the late 1800s, when wide swathes of Argentina were converted for agricultural purposes. Introduced populations live largely in cities, and this affinity for human-disturbed habitat may have arisen in the native range. Recently, SEO/BirdLife volunteers estimated around 20,000 monk parakeets in Spanish cities. 

<img src="/images/Nativemap.tiff" width="300" height ="300" />




I'm using genomic tools to determine if genetic differentiation patterns across monk parakeet populations are a product of adaptation to human-induced selection pressures. I'm complementing genomic markers with candidate immune genes to determine if human disturbance influences pathogenic selection pressures. I'm also interested in cold adaptations and phenotypic plasticity, including learning, in monk parakeets that evolved much earlier than their contemporary invasions.

In my following posts, I'll use monk parakeets as a case study for playing around in: 

<img src="/images/R.png" width="200" height ="200" />

The codes I'll post are intended for biologists interested in learning to create graphics or run genomic analyses in R. In all posts, I'm assuming you know how to install and load packages, search for help in RStudio and online, and the different classes of variables / object types in R. If you're unfamiliar with regular expression or logical statements in R, run the simplest unit of the nested expressions to get accustomed to the logic.

For instance:


```r
x <- c(rep("Myiopsitta", 10), "Hylocichla")
```

Use regular expression functions to filter data 

```r
z <- x[grep("Myiopsitta", x)]
z
```

```
##  [1] "Myiopsitta" "Myiopsitta" "Myiopsitta" "Myiopsitta" "Myiopsitta"
##  [6] "Myiopsitta" "Myiopsitta" "Myiopsitta" "Myiopsitta" "Myiopsitta"
```

If you don't know how `grep` works, run the expression inside the square brackets, which is the simplest unit of the nested expression. 

```r
grep("Myiopsitta", x)
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10
```

Do the same for the function `grepl`.

```r
z <- x[grepl("Myiopsitta", x)]
grepl("Myiopsitta", x)
```

```
##  [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
```

```r
z
```

```
##  [1] "Myiopsitta" "Myiopsitta" "Myiopsitta" "Myiopsitta" "Myiopsitta"
##  [6] "Myiopsitta" "Myiopsitta" "Myiopsitta" "Myiopsitta" "Myiopsitta"
```


Feel free to get in touch if you find any bugs in posted code! 



