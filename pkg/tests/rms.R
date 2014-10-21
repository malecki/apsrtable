# # Test rms
# library(rms)
# 
# data(attitude)
# 
# attitude <- rbind(attitude, attitude)
# attitude$grp <- sample(c("A", "B", "C"), nrow(attitude), replace = TRUE)
# 
# d <- datadist(attitude)
# options(datadist = "d")
# 
# mod1 <- ols(rating ~ complaints + privileges + learning + 
#                   raises + critical + advance, data = attitude, x = TRUE)
# mod1$se <- NULL
# mod2 <- ols(rating ~ complaints + privileges + learning + raises + critical + 
#               advance + grp, data = attitude, x = TRUE)
# 
# mod2$se <- sqrt(diag(vcov(robcov(mod2, cluster = attitude$grp))))
# 
# 
# apsrtable(mod1, mod2, se="robust")
