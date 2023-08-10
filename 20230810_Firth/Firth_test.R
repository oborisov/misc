library(brglm2)
library(logistf)
brglm_types <- c("AS_mixed", "AS_mean", "AS_median", "MPL_Jeffreys", "ML", "correction") 
# print
timestamp <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
fileConn_name <- paste0(timestamp, "_Firth_log.txt")
fileConn <- file(fileConn_name, "w")
sink(fileConn)
# code
test_df <- data.frame(phenotype=c(rep(1, 80), rep(0, 1900), rep(1, 20)),
                      SNP=c(rep(0, 1900), rep(1, 100)))
print(" ####### Test data ######## ")
head(test_df)
print(" ####### Count table of the test data ######## ")
table(test_df)
print(" ####### fisher.test ######## ")
fisher.test(as.matrix(table(test_df)))
print(" ####### Standard logistic regression ######## ")
summary(glm(phenotype ~ SNP, data = test_df, family = binomial))
for (brglm_type in brglm_types) {
  print(paste0(" ####### brglmFit, type = ", brglm_type, " ######## "))
  print(summary(glm(phenotype ~ SNP, family = binomial(logit), data = test_df, method = "brglmFit", type = brglm_type)))
}
print(" ####### logistf ######## ")
summary(logistf::logistf(phenotype ~ SNP, data=test_df))

print(" ####### Test running time ######## ")
# code
test_df <- data.frame(phenotype=c(rep(1, 8000), rep(0, 190000), rep(1, 2000)),
                      SNP=c(rep(0, 190000), rep(1, 10000)))
print(" ####### Count table of the test data ######## ")
table(test_df)
print(" ####### Standard logistic regression ######## ")
system.time(summary(glm(phenotype ~ SNP, data = test_df, family = binomial)))
for (brglm_type in brglm_types) {
  print(paste0(" ####### brglmFit, type = ", brglm_type, " ######## "))
  print(system.time(summary(glm(phenotype ~ SNP, family = binomial(logit), data = test_df, method = "brglmFit", type = brglm_type))))
}
print(" ####### logistf ######## ")
system.time(summary(logistf::logistf(phenotype ~ SNP, data=test_df)))


# close connectoin
sink(); close(fileConn)
