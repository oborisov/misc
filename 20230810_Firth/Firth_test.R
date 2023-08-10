library(brglm2)
library(logistf)
# print
timestamp <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
fileConn_name <- paste0(timestamp, "_Firth_log.txt")
fileConn <- file(fileConn_name, "w")
sink(fileConn)
# code
test_df <- data.frame(phenotype=c(rep(1, 8000), rep(0, 190000), rep(1, 2000)),
                      SNP=c(rep(0, 190000), rep(1, 10000)))
head(test_df)
table(test_df)
print(" ####### fisher.test ######## ")
system.time(fisher.test(as.matrix(table(test_df))))
fisher.test(as.matrix(table(test_df)))
print(" ####### Standard logistic regression ######## ")
system.time(summary(glm(phenotype ~ SNP, data = test_df, family = binomial)))
summary(glm(phenotype ~ SNP, data = test_df, family = binomial))
print(" ####### brglmFit, type = AS_mean ######## ")
system.time(summary(glm(phenotype ~ SNP, family = binomial(logit), data = test_df, method = "brglmFit", type = "AS_mean")))
summary(glm(phenotype ~ SNP, family = binomial(logit), data = test_df, method = "brglmFit", type = "AS_mean"))
print(" ####### logistf ######## ")
system.time(summary(logistf::logistf(phenotype ~ SNP, data=test_df)))

# close connectoin
sink(); close(fileConn)
