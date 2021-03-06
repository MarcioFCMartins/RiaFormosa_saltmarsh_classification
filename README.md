Remote sensing classification of saltmarshes in the Ria Formosa
================

## TODO

Add some images to display the intermediate and final results Finish
explaining the whole process Re-factor code and add some extra comments

## Summary

This repository summarizes the methods used during my work at mapping
the saltmarsh communities in the Ria Formosa lagoon (South Portugal).

Satellite imagery and LIDAR data were combined and used to extract the
spectral signatures and preferred elevation of each saltmarsh community.
This information was used to determine which community exists in each
site of the Ria.

The project was fully implemented in open source, via a combination of R
and QGIS (working on moving it fully to R, and making it fully
compatible with the newest versions of rspatial packages). Because some
of the code required is quite extensive, I do not include all of the
code in this README. In particular, the data pre-processing has been
glossed over. However, I refer to the script files that go over that
specific task, and you can find them in the *code* folder.

Additionally, there is a whole section on methods that can assist me in
future work to improve speed or capacity to handle larger than memory
data.

## Settings and global variables required

The entire project uses a wide variety of R packages, especially for all
spatial data handling.

``` r
library(readxl)   # Read xlsx files
library(raster)   # Tools to handle rasters - to be replaced with terra
library(dplyr)    # Data wrangling
library(tidyr)    # Data wrangling
library(sf)       # Simple features - spatial vector data 
library(vegan)    # Ecology analysis functions - for bray curtis distances
library(purrr)    # Functional programing
library(ggplot2)  # Grammar of graphics - plotting
library(scales)   # Function to format scales for visualization
library(forcats)  # Tools to modifying factor levels (plotting purposes)
library(RColorBrewer) # Color palletes
```

I also need the references for WGS84 and PT-TM06/ETRS89 coordinate
reference systems, since part of our information was recorded in WGS84.
However, working with a projected CRS will be better for accurate
distances. So, all of our data will be converted to PT-TM06/ETRS89.

``` r
# Coordinate Reference Systems - EPSG integers
# https://spatialreference.org/
pt_crs <- 3763     # ETRS89/ PT-TM06
wgs84_crs <- 4326  # WGS84

# Bounding box for example area that is used in plots
plot_ext <- raster::extent(
  matrix(c(20424.21, 22854.12,-300130.5, -298653.6),
         nrow = 2, byrow = TRUE)
)
```

## Extracting training points’ positions

Transects were performed along saltmarsh fringes. A 1x1 m quadrat was
overlaid along the entire transect and species presences were recorded.
During field work, we recorded the starting and ending points of the
transects in the GPS. However, for properly extracting the visual and
terrain properties, we must have centroids for each quadrat.

To do so, I defined custom functions to: 1. Calculate the orientation of
a transect (relative to North) 2. Break a transect into k points, where
k = number of quadrats 3. Draw a square with defined size and
orientation, so that one corner of the quadrat is at the given point

> You can find them in helper\_functions.R

With these tools I can estimate the center position for all recorded
quadrats from the start and end points of each transect (after some data
wrangling).

> Script 1\_draw\_quadrats.R

The output of this was a shapefile where each individual quadrat is a
polygon. Those polygons have all the identifying information required to
connect them to the transect where they belong.

## Creating vegetation community clusters

Using individual species data to classify the saltmarshes would lead to
very noisy estimates. However, we know that saltmarsh species tend to
form communities (mostly based around their hydroperiod). I decided to
use hierarchical clustering, based on the Sørensen index (AKA the binary
Bray-Curtis index). These clusters (or communities) will be our target
variable.

> Scrit 2\_community\_cluster.R

I ended up opting for using 4 clusters (Figure 1), as it seemed to make
the most sense based on what we had observed in the field. One community
comprised only of *Spartina maritima*, a pioneer species, which tended
to form the first fringe near the water (cluster 1). Oftentimes, *S.
maritima* is found with *Sarcocornia perennis*, forming the cluster 2.

Cluster 3 is formed by both *Sarcocornia* species, *Halimione
portulacoides* and, to a lesser extent, *Arthrocnemum macrostachyum*.

Cluster 4 is composed of the more bush-like species and was generally
found in the supratidal area, when saltamarsh started to transition into
dunes.

``` r
spp_quadrats <- st_read("./outputs/quadrats_species/quadrats_species.gpkg") %>%
  filter(cluster != "unvegetated")
```

    ## Reading layer `quadrats_species' from data source `C:\Users\marci\OneDrive - Universidade do Algarve\03_others\RiaFormosa_saltmarsh_classification\outputs\quadrats_species\quadrats_species.gpkg' using driver `GPKG'
    ## Simple feature collection with 1538 features and 36 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 9770.353 ymin: -299877.8 xmax: 46398.95 ymax: -281468.5
    ## projected CRS:  unnamed

``` r
presence_in_cluster <- spp_quadrats[,c(8:34, 36)] %>%
  st_drop_geometry() %>%
  gather(key = "species", value = "presence", 1:27) %>%
  group_by(cluster, species) %>%
  summarise(prop_presences = sum(presence)/n()) %>%
  ungroup() 

# Get list of species which are present in more than 10% of any cluster
common_sp <- presence_in_cluster %>%
  filter(prop_presences > 0.1) %>%
  pull(species) %>%
  unique()

presence_in_cluster %>%
  filter(species %in% common_sp) %>%
  mutate(species = fct_reorder2(species, cluster, prop_presences)) %>%
  ggplot(aes(x = cluster, 
             y = species, 
             fill = prop_presences)) +
    geom_raster() +
    geom_text(aes(label = 
      ifelse(prop_presences > 0.1, 
             percent(prop_presences, accuracy = 1),
             NA)),
      color = "white",
      fontface = "bold") +
    scale_fill_viridis_c(guide = FALSE) +
    labs(title = "Species presence per cluster",
         subtitle = "For species with presence above 10% in any cluster",
         y = NULL,
         x = "Cluster") +
    theme_minimal()
```

![Figure 1](README_files/figure-gfm/plot_clusters-1.png)

## Community predictor features

We now have the coordinates for the quadrats and our target variable. To
create a predictive model we just need features that help separate our
target. These will be our predictor features.

Feature selection and engineering can (and often is) a laborsome task
and of extreme importance for state of the art model performance.
However, for this work I did not dwell into this component and instead
opted to let our model training handle all our features.

> Notes: In the future, proper feature engeniering is one of my goals,
> especially since the work we have planned will not have such good data
> availability. <https://bookdown.org/max/FES/> has a nice overview of
> typical approaches; <https://nowosad.github.io/post/lsm-bp2/> has
> implemented many common landscape metrics;
> <http://www.seascapemodels.org/rstats/2020/02/08/calculating-distances-in-R.html>
> distances can be used as proxies for elevation (?) when LIDAR data is
> not available

The available data for this was: a LIDAR digital elevation model with a
10 meter resolution; multispectral raster data (bands: Near infrared,
Red, Green, Blue) with a 3 meter resolution (obtained from planet.com).
The one feature engineering I did was calculating the normalized
difference vegetation index (NDVI). This is the most basic index used
for analyzing vegetation and is also correlated to water content in the
soil, both of which are properties I except to help separate these
communities.

The variables at the centroid of each quadrat were extracted from these
rasters, and upsampled using bilinear interpolation. The interpolation
is important, as the quadrats are at a distance of \~ 1 m, lower than
the available resolution. Allowing interpolation to be used should allow
for a better estimation of the quadrat’s true properties.

``` r
quadrat_features <- st_read("./outputs/quadrats_train_features/quadrats_train_features.gpkg") %>%
  st_drop_geometry() %>%
  mutate(station = group_indices(., water_body, site_nr)) %>%
  # Station 10 has some quadrats that do not onverlay our raster
  filter(station != 10) %>%
  select(6:13) %>%
  pivot_longer(
    cols = c("elev", "slope", "ndvi", "nir", "r", "g", "b"),
    names_to = "var",
    values_to = "value"
  ) %>%
  mutate(var = factor(var, levels = c("elev", "slope", "ndvi", "nir", "r", "g", "b")))
```

    ## Reading layer `quadrats_train_features' from data source `C:\Users\marci\OneDrive - Universidade do Algarve\03_others\RiaFormosa_saltmarsh_classification\outputs\quadrats_train_features\quadrats_train_features.gpkg' using driver `GPKG'
    ## Simple feature collection with 1491 features and 13 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 9770.353 ymin: -299877.8 xmax: 46398.95 ymax: -281468.5
    ## projected CRS:  unnamed

``` r
ggplot(quadrat_features) +
  geom_density(
    aes(x = value, fill = cluster),
    alpha = 0.7) +
  labs(
    title = "Density distribution of predictors by vegetation cluster"
  ) +
  scale_fill_brewer(
    type = "qual",
    palette = "Set1"
  ) +
  facet_wrap(
    vars(var),
    scales = "free",
    ncol = 2) +
  theme_bw()
```

![Figure 2](README_files/figure-gfm/features-1.png)

We can see from figure 2 that there are differences in elevation, slope,
NDVI and blue between our target classes. Separation is, obviously, not
perfect but note that we are looking at the univariate distributions of
these properties, while some of the more complex models will consider
interactions between them. It’s also quite clear that cluster 1 and 2
are very similar and might be hard to distinguish.

## Saltmarsh masking

While we have features for the desired saltmarsh communities, we do not
have any data for non-saltmarsh zones. This means that if we apply our
model to our full data-set, all of the Ria Formosa will be classified as
a saltmarsh community (which is not true, as there are dunes, water
channels, seagrass meadows, etc).

There were 2 broad approaches to working around this issue: 1.
Supervised - Select areas to collect training data for non-saltmarsh
areas and add those classes to our model 2. Unsupervised - Classify all
of the Ria into similar areas and then, manually, exclude those which
are clearly not saltmarsh.

Approach 1 would probably have been more rigorous, but I decided that
approach 2 could be easier and quicker.

So, to create the saltmarsh mask, I first used an NDVI threshhold to
exclude water channels. Then, I applied k-means to the individual
pixels, using all available predictors. I then loaded that information
into QGIS and went over my study area, noting which clusters represented
saltmarsh. There were some “loose” pixels, especially near the edge of
the channels. I drew polygons covering them and used those polygons as
an additional mask. After all cleanup, I had a single-layer raster with
1 values for pixels over saltmarsh and 0 for non-saltmarsh. This mask
was then applied to the full raster file, and all data outside of
marshes was removed.

``` r
clusters <- raster::raster("./outputs/saltmarsh_mask/ria_clusters-20all.tif")
mask2 <- raster::raster("./outputs/saltmarsh_mask/saltmarsh_clusters.tif")

par(mfrow = c(2,1), 
    mai = c(0.5, 0.5, 0.2,0))
raster::plot(
  clusters, 
  col = rainbow(20), 
  ext = plot_ext,
  main = "All clusters created by k-means",
  legend = FALSE)
raster::plot(
  mask2, 
  col = "black", 
  ext = plot_ext,
  main = "Areas classified as saltmarsh (black = saltmarsh)",
  legend = FALSE)
```

![Figure 3](README_files/figure-gfm/mask-1.png)

## Model training and selection

This section will be light on details, but basically: 1. Data was split
into training and testing datasets 2. Training data was split using
spatial k-fold cross validation. One fold was created per sampling zone,
with that zone being excluded. 3. Models were trained

Before selecting any model, we have to decide on a performance measure.
As seen in Figure 3, the classes are not balanced. Cluster 3 represents
approximately 50% of the samples, despite the existence of 4 classes. I
could oversample or undersample to deal with this. However, this would
leave to lost information. Since the imbalance is not extreme, I instead
opted by a performance measure that accounts for that: the kappa
statistic.

The best model ended up being a support vector machine (using a linear
kernel).

## Creating predictions map

After having a trained model, the full raster was split into horizontal
segments. These segments were loaded into memory individually, converted
to a data.frame and fed into *caret*’s predict function. These
predictions were then converted back into a raster, resulting in a
prediction map of the expected vegetation communities (Figure 4).

``` r
predictions <- raster::raster("./outputs/saltmarsh_classified/saltmarsh_classification_v2.tif")

raster::plot(
  predictions, 
  col = brewer.pal(4, "Set1"), 
  ext = plot_ext,
  main = "Predicted vegetation community",
  legend = TRUE,
  breaks = c(0,1,2,3,4))
```

![Figure 4](README_files/figure-gfm/classification_map-1.png)

## Improvements for future works

### New machine learning frameworks

The initial work was performed using *caret* for model training and
performance checking. However, *caret* is now a deprecated (albeit still
functional) package. Its replacement is the *tidymodels* ecosystem and
packages therin. Another promising package ecosystem that has been
released recently is the *mlr3* (which replaces *mlr*). I have been
wanting to learn both of these to decide which framework to learn, and
decided this project is the best time for that. Both of these are still
in development.

Learning the *tidymodels* ecosystem seems easier for me. It’s based on
tidy data format, functional programming and much in line with other
tidyverse packages, which has been the workhorse of my R usage.

However, *mlr3* seems quite interesting as well, even it only because I
have wanted to learn object-oriented programming for a while now. I have
(or will have) alternative scripts which try to implement the data
splitting and model training/selection using both of these ecosystems.

### Tiling system

Iiling our image can allow to speed up work with larger-than-memory
rasters. Breaking up a single raster into smaller ones that are all
processable in memory, or simply processed in parallel can speed up your
workflow tremendously.

Implementing custom tiling solutions is relatively simple, but the
package
[*satelliteTools*](https://github.com/environmentalinformatics-marburg/satelliteTools)
already implemented tools to do so, which might be more sofisticated
than a custom implementation.

> Add an example of the result of this tiling system below.

#### Parallelization

Processing a raster in parallel can make a massive difference in speed.
As an example, running a linear contrast enhancement on the full Ria
Raster used here takes approximately 15 minutes. When ran it parallel (7
cores), time went down to under 1.5 minute (this sounded too good to be
true, but testing differences between rasters returned zero for all
cells - results are equal)

**There are 3 main ways to parallelize raster operations:** 1. Let
raster do it for you. Check the raster documentation (section cluster)
to see which funtions have this function implemented. 2. The
raster::clusteR function, which allows you to pass a function that will
be ran in parallel. This is very easy to use, for example, to
parallelize cell-wise operations (e.g. calculate the sum of all cells or
divide all cells by fixes values). 3. The foreach package, which allows
you to loop over the layers of a raster in parallel. I prefer this
method for raster-wise or layer-wise operations. In contrast
enhancement, for example, I need the quantiles and maximums of each
layer, which can’t be obtained when looping over individual cells with
clusteR (or I could not figure it out, not even with pre-calculating
them and passing them as arguments).

Parallel processing, however, can’t fix every slow task. As a general
rule, if your raster is very large and your operations are very
repeatable, it will probably benefit from it.

``` r
library(raster)         # Raster data methods
library(tidyverse) 
library(sf)
library(doParallel)
library(foreach)

# Custom linear stretch function - the one implemented in raster
# fails when applied to large rasters
source("./code/obsolete/contrast_stretch.R")

# Load raw raster
raw <- raster::brick("./data/raster_data/raster_satellite/sat_clip.tif")
names(raw) <- c("r", "g", "b", "nir")


# Run stretching in single core, ~ 15 minutes
raster <- contrast_stretch(
    raw,
    filename = "D:/Desktop/testrasters/custom_sc.tif"
)


# Run stretching in parallel - ~1.5 minutes
cl <- makeCluster(detectCores() -1)
registerDoParallel(cl)
Sys.sleep(1)
message("Custom MC")
raster <- foreach(i=1:nlayers(raw),
             .packages = c("raster")) %dopar%
    contrast_stretch(
        x = raw[[i]])
raster <- stack(raster)
writeRaster(
    raster,
    filename = "D:/Desktop/testrasters/custom_mc.tif"
)
```

#### Other resources

More examples and resources on this:
<https://www.gis-blog.com/increasing-the-speed-of-raster-processing-with-r-part-13/>
<https://www.gis-blog.com/increasing-the-speed-of-raster-processing-with-r-part-23-parallelisation/>
<https://www.gis-blog.com/increasing-the-speed-of-raster-processing-with-r-part-33-cluster/>
[Computing with large rasters in R: tiling, parallelization,
optimization](https://opengeohub.org/lesson/computing-large-rasters-r-tiling-parallelization-optimization)
[Machine learning for spatial
data](https://opengeohub.org/machine-learning-spatial-data)

### Object oriented classification

``` r
source("./code/other/object_based_image_analysis_example.R")
```

### Satellite data sources

  - <http://earsc.org/news/irs-data-now-available-free-of-charge-to-scientific-users>
    provides high quality data for scientific purposes, for free, if a
    grant is submited (2017 announcement, check if still true)
