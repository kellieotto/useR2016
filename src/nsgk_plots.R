library(dplyr)
library(ggplot2)

nsgk_res <- read.csv("nsgk_results.csv")
nsgk_res %>%
  as.data.frame() %>%
  mutate(avg_concordance = 
           apply(nsgk_res[, 3:10], 1, mean)) %>%
  ggplot(aes(x = avg_concordance, y = global_pvalue)) + 
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

ggsave("../slides/fig/nsgk.png",height=9,width=12,dpi=72)
