## -----------------------------------------------------------------------------
library(RCT2)
data(jd)

## -----------------------------------------------------------------------------
data_LTFC <- data.frame(jd$assigned, jd$pct0, jd$cdd6m, jd$anonale)
colnames(data_LTFC) <- c("Z", "A", "Y", "id")
test <- CalAPO(data_LTFC)
print(CalAPO(data_LTFC))

## -----------------------------------------------------------------------------
data_perm <- data.frame(jd$assigned, jd$pct0, jd$cdi, jd$anonale)
colnames(data_perm) <- c("Z", "A", "Y", "id")
CalAPO(data_perm)

## -----------------------------------------------------------------------------
Test2SRE(data_LTFC, effect="MDE", alpha=0.05)

## -----------------------------------------------------------------------------
# calculate variances for permanent contract
var.perm <- calpara(data_perm)

# calculate variances for long term fixed contract
var.LTFC <- calpara(data_LTFC)

## -----------------------------------------------------------------------------
sigma.perm <- var.perm$sigma.tot
sigma.LTFC <- var.LTFC$sigma.tot
print(sigma.perm)

## -----------------------------------------------------------------------------
### effect size and assignment mechanism
mu <- 0.03
qa <- rep(1/3,3)

# calculate sample size for the permanent contract
print("Permanent Contract:")
print(Calsamplesize(data_LTFC, 0.03, qa, 0.05, 0.2))


# calculate sample size for the long term fixed contract
print("Long Term Fixed Contract:")
print(Calsamplesize(data_perm, 0.03, qa, alpha=0.05, beta=0.2))

