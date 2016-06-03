library(dplyr)
library(ggplot2)

setwd("Documents/nsgk/nsgk/")
nsgk = read.csv("results.csv", header=TRUE)
head(nsgk)
dim(nsgk)


ggplot(nsgk, aes(x = overall_pvalue)) + geom_bar()

# Many have a p-value of 1 -- perfect concordance in every video.
# Likely, nobody used that tag.

sum(nsgk$overall_pvalue == 1)
# 74
no_tag <- sapply(1:nrow(nsgk), function(x) all(nsgk[x,27:34] == 0))
sum(no_tag)
# 50

# The remaining ones have very little variance, like one person used the tag once.
nsgk_filt <- nsgk %>% filter(!no_tag)

ggplot(nsgk_filt, aes(x = overall_pvalue)) + geom_bar()


## Look at the ones with <=0.05 pvalue
nsgk_significant <- nsgk %>% filter(overall_pvalue <= 0.05)
nrow(nsgk_significant)

## Among these, how many had meaningful concordance?
nsgk_significant %>% 
  mutate(med_concordance = 
           apply(nsgk_significant[, 11:18], 1, median)) %>%
  ggplot(aes(x = med_concordance)) + 
  geom_bar()

## Overall, what's the relationship between concordance and P?
nsgk_filt %>%
  mutate(avg_concordance = 
               apply(nsgk_filt[, 11:18], 1, mean)) %>%
  ggplot(aes(x = avg_concordance, y = overall_pvalue)) + 
  geom_point() +
  xlab("Average Concordance") +
  ylab("Overall P-value") +
  ggtitle("Average Concordance Across 8 Videos vs P-value") +
  theme_bw()

