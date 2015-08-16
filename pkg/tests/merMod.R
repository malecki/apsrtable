# # Test merMod
# # 
library(lme4)

(fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy))
summary(fm1)# (with its own print method)

apsrtable(fm1)
# 
(fm2 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy))

(fm3 <- lmer(Reaction ~ 0 + (1 | Subject) + (1 | Days), sleepstudy))

(fm3 <- lmer(Reaction ~ 0 + (1 | Subject) + (1 | Days), sleepstudy))

sleepstudy$Noise <- rnorm(nrow(sleepstudy))
sleepstudy$Wave <- sample(c("A", "B", "C"), nrow(sleepstudy), replace = TRUE)


(fm4 <- lmer(Reaction ~ 0 + (1 | Subject) + (Noise | Days) + (1|Wave), sleepstudy))

apsrtable(fm1, fm2, fm3, fm4)

apsrtable(fm1, fm2, fm3)


sleepstudy$DV2 <- ifelse(sleepstudy$Reaction > 285, 1, 0)
sleepstudy$DV2 <- factor(sleepstudy$DV2)

(gm2 <- glmer(DV2 ~ Days + (1 | Subject), sleepstudy, family = "binomial"))

(gm3 <- glmer(DV2 ~ 0 + (1 | Subject) + (1 | Days), sleepstudy,  family = "binomial"))

(gm3 <- glmer(DV2 ~ 0 + (1 | Subject) + (1 | Days), sleepstudy,  family = "binomial"))
(gm4 <- glmer(DV2 ~ 0 + (1 | Subject) + (Noise | Days) + (1|Wave), sleepstudy, 
              family = "binomial"))


apsrtable(gm2, gm3, gm4, lev = 0)

# 
# ## Regular
# 
# gmA <- glm(DV2 ~ Days, data = sleepstudy, family = "binomial")
# lmA <- lm(Reaction ~ Days, data = sleepstudy)
# 
# apsrtable(gmA)
# apsrtable(lmA)
# 
# apsrtable(gm2, gm3, gm4, float = "sidewaystable", label= "duh", caption = "please", 
#           notes = list(stars.note, "My note."), 
#           omitcoef=list("distid", 
#                         expression(grep("distid", coefnames, value=TRUE)), 
#                         "year", 
#                         expression(grep("year", coefnames, value = TRUE))), 
#           Sweave=FALSE)
# # summod <- summary(fm4)
# # 

