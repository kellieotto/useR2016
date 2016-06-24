# Load data
library(permuter)
data(macnell)


# Calculate the observed test statistic
tst_stat <- function(x) {
  mean(x[macnell$taidgender == 0], na.rm = TRUE) -
  mean(x[macnell$taidgender == 1], na.rm = TRUE)
  }
observed <- tst_stat(macnell$overall)

# Ratings vs reported instructor gender (difference in means)
distr <- stratified_two_sample(response=macnell$overall, 
                               group=macnell$taidgender, 
                               stratum=macnell$tagender,
                               stat="mean", reps=10**5)


# Calculate two-sided p-value
p <- t2p(observed, distr, alternative = "two-sided")



# Initialize placeholders
categories <- colnames(macnell)[2:16]
test_distr <- matrix(0, nrow = 10**5, ncol = length(categories))
pvalues <- rep(0, length(categories))
colnames(test_distr) <- names(pvalues) <- categories

# Loop over rating categories
for(col in categories){
  set.seed(678)
  observed <- tst_stat(macnell[ ,col])
  distr <- stratified_two_sample(response=macnell[ ,col], 
                                 group=macnell$taidgender, 
                                 stratum=macnell$tagender,
                                 stat="mean", reps=10**5)
  test_distr[, col] <- distr
  pvalues[col] <- t2p(observed, distr, alternative = "two-sided")
}

# NPC 
omnibus_pvalue <- npc(pvalues, test_distr, combine="fisher", 
                     alternatives="two-sided")
