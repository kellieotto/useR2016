library(dplyr)
library(ggplot2)


nsgk <- read.csv("https://raw.githubusercontent.com/statlab/nsgk/master/nsgk/results.csv")
head(nsgk)
dim(nsgk)


ggplot(nsgk, aes(x = overall_pvalue)) + geom_bar()

# Many have a p-value of 1 -- perfect concordance in every video.
# Likely, nobody used that tag.

sum(nsgk$overall_pvalue == 1)
# 74
no_tag <- (apply( nsgk[,27:34], 1, sum) == 0)
sum(no_tag)
# 50

# Remove the tags where nobody observed them at all.
nsgk_filt <- nsgk %>% filter(!no_tag)
ggplot(nsgk_filt, aes(x = overall_pvalue)) + geom_bar()
# P-values are highly non-uniform -- many at 0 and 1.

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
  geom_point(color = "#629e1f", alpha = 0.6, size = 3) +
  xlab("Average Concordance") +
  ylab("Overall P-value") +
  ggtitle("Average Concordance Across 8 Videos vs P-value") +
  theme(
    panel.background = element_rect(fill = "#E8EBEF"),
    axis.text = element_text(size = 20, color = "#143264"),
    axis.title = element_text(size = 22, color = "#143264"),
    title = element_text(color = "#143264", size = 22)
  )
ggsave("slides/fig/nsgk.png",height=9,width=12,dpi=72)