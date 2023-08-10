library(brglm2)
library(logistf)
test_df <- data.frame(phenotype=c(rep(1, 80), rep(0, 1900), rep(1, 20)),
                      SNP=c(rep(0, 1900), rep(1, 100)))
table(test_df)
fisher.test(as.matrix(table(test_df)))
summary(glm(phenotype ~ SNP, data = test_df, family = binomial))
summary(glm(phenotype ~ SNP, family = binomial(logit), data = test_df, method = "brglmFit", type = "AS_mean"))
summary(logistf::logistf(phenotype ~ SNP, data=test_df))

