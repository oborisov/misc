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


# close connection
sink(); close(fileConn)


if (F) {

  ##
  fit <- glm(ICD ~ variant, input_df, family = binomial)
  summary(fit)
  fit2 <- glm(ICD ~ variant, family = binomial(logit),
              data = input_df,
              method = "glm.fit")
  summary(fit2)
  fit3 <- glm(ICD ~ variant, family = binomial(logit),
              data = input_df,
              method = "brglmFit", type = "AS_mixed")
  summary(fit3)

  data("lizards")
  # Fit the model using maximum likelihood
  lizardsML <- glm(cbind(grahami, opalinus) ~ height + diameter +
                     light + time, family = binomial(logit), data = lizards,
                   method = "glm.fit")
  # Mean bias-reduced fit:
  lizardsBR_mean <- glm(cbind(grahami, opalinus) ~ height + diameter +
                          light + time, family = binomial(logit), data = lizards,
                        method = "brglmFit")
  # Median bias-reduced fit:
  lizardsBR_median <- glm(cbind(grahami, opalinus) ~ height + diameter +
                            light + time, family = binomial(logit), data = lizards,
                          method = "brglmFit", type = "AS_median")


  ##

}
