Remote sensing classification of saltmarshes in the Ria Formosa
================

## Summary

This project is a collection of the methods used during my work at
mapping the saltmarsh communities in the Ria Formosa lagoon (South
Portugal).

For this project, I took transects performed along saltmarsh fringes,
where species presence/absence was recorded. This information was
combined with the transect’s start/end points to estimate a position for
all quadrats along the transect. These quadrats were clustered into one
of 3 possible saltmarsh communities.

To do so, unsupervised clustering methods were used to select saltmarsh
areas. These areas were then classified into one of the 3 mentioned
vegetation communites, using machine learning methods that relied on the
transect data.

This was possible due to the availability of remote sensing data (3m
resolution, R G B and NIR bands) as well as a LIDAR digital elevation
model.

The project was fully implemented in open source, via a combination of R
and QGIS.

## Including Code

You can include R code in the document as follows:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](README_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
