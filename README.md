# antarctica_climatic_similarity
repository with the R code behind the manuscript about antarctic climatic similarity
# Script Files
- `eucl_clean.r` contains the R code to compute the euclidean distances and look into different thresholds.
- `download_gbif_data.R` R code to download the species data from GBIF
- `ResW_mergeResults.r` contains the R code that puts together the outputs from `eucl_clean.r` for the world.
- `fig World eucl dist.r` creates the figure showing the minimum euclidean distance between Antarctica and the rest of the world.
- `figureS1_histSOIthreshold.r` contains the R code that identifies the threshold to use to differentiate similar climate from dissimilar climate.
- `fig_SOI_eucl_antarctica.r` contains the R code to build the mini maps of the Southern Ocean Islands
- `sinkMap.r` contains the R code using `ggplot` to map the Antarctic sinks. 
- `world_byACBRs.r` contains the R code to see the similary between each ACBR and the rest of the world
- `Indices plot.r` contains the R code to create the plot with the species indices
- `figureS3_classify_world_byACBRcount.R` contains the code to see the relationship between ACBR counts and the minimum euclidean distance of each point in the world.

# Result Files
These files can be accessed via: https://doi.org/10.5281/zenodo.5812047
