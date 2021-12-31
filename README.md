# antarctica_climatic_similarity
repository with the R code behind the manuscript about antarctic climatic similarity
# Files
- `eucl_clean.r` contains the R code to compute the euclidean distances and look into different thresholds.
- `download_gbif_data.R` R code to download the species data from GBIF
- `ResW_mergeResults.r` contains the R code that puts together the outputs from `eucl_clean.r` for the world.
- `figureS1_histSOIthreshold.r` contains the R code that identifies the threshold to use to differentiate similar climate from dissimilar climate.
- `sinkMap.r` contains the R code using `ggplot` to map the Antarctic sinks. 
- `Indices plot.r` contains the R code to create the plot with the species indices
- `figureS3_classify_world_byACBRcount.R` contains the code to see the relationship between ACBR counts and the minimum euclidean distance of each point in the world.

