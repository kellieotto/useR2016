# NSGK example

library(permuter)
library(dplyr)
library(ggplot2)

data(nsgk)
time_stamps <- c(36, 32, 35, 37, 31, 35, 40, 32)

# nsgk_mat is a list of lists:
# first level indexes the tag
# second level indexes the video
# For how to obtain nsgk_mat from data(nsgk), see the vignette nsgk.Rmd

# Example of how we analyze a single tag.
tag1 <- nsgk_mat[[1]]

# Find the distribution of the IRR test statistic for each video
video_specific_results <- lapply(tag1, function(x){
  res <- irr_ts_distribution(x, num_perm = 1000,    
                            keep_dist = TRUE, seed = 101)
})
tag_distribution <- sapply(video_specific_results, function(x) x$dist)
tag_pvalues      <- sapply(video_specific_results, function(x) x$pvalue)

# Combine across videos with NPC
tag_npc_res <- irr_npc_distribution(tag_distribution, size = time_stamps, pvalues = tag_pvalues)
