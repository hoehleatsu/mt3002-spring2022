## Exercise 4.2

##Load data
library(tidyverse)
zeger <- read.csv(file.path("Data","zeger_etal89-tab2.csv"),sep="\t", row.names=1)

colnames(zeger) <- c(0:12)
zeger

## Extract row from the 4.2 exercise
q0187 <- zeger["01-87",]
q0187

###############################
## 4.2.1 compute unadjusted estimate
###############################

## base R style
rowsum <- apply(zeger, 2, sum, na.rm=TRUE)
total <- sum(rowsum)
p <- rowsum / total
F <- cumsum(p)

## Total count so far
NtT <- sum(q0187, na.rm=TRUE)
NtT

##Biased estimate of F(3)
FTmt <- F["3"]
FTmt

## Compute simple unadjusted estimate
NtInf_hat <- ceiling(NtT/FTmt)
NtInf_hat

## Make a grid of possible N values to evaluate
grid <- tibble(NtInf = seq(NtT, NtT + 2*(NtInf_hat - NtT))) %>% 
  mutate(lik = dbinom(NtT, size=NtInf, FTmt))

## Max lik estimate using the binomial
grid %>% filter(lik == max(lik))

ggplot(grid, aes(x=NtInf, ymin=0, ymax=lik)) + geom_linerange() +
  theme_minimal()


## 4.1.2 compute adjusted estimate

# g(x) = P( D=x| D <= x), x=0,1,2
g <- function(x) {
  head(zeger, n=-(x-1)) %>% select(str_c(0:x)) %>% 
    summarise(across(.fns=sum)) %>% 
    prop.table() %>% 
    as.numeric() %>% 
    tail(n=1)
}

FTmt_unbiased <- prod(1 - sapply(4:12,g))
NtInf_hat_unbiased <- ceiling(NtT/FTmt_unbiased)
NtInf_hat_unbiased
