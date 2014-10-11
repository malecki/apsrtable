# Test merMod

library(lme4)

(fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy))
summary(fm1)# (with its own print method)

apsrtable(fm1, lev = 0)

(fm2 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy))

(fm3 <- lmer(Reaction ~ 0 + (1 | Subject) + (1 | Days), sleepstudy))

(fm3 <- lmer(Reaction ~ 0 + (1 | Subject) + (1 | Days), sleepstudy))

sleepstudy$Noise <- rnorm(nrow(sleepstudy))
sleepstudy$Wave <- sample(c("A", "B", "C"), nrow(sleepstudy), replace = TRUE)


(fm4 <- lmer(Reaction ~ 0 + (1 | Subject) + (Noise | Days) + (1|Wave), sleepstudy))

apsrtable(fm1, fm2, fm3, fm4, lev = 0)

apsrtable(fm1, fm2, fm3, lev = 0)


sleepstudy$DV2 <- ifelse(sleepstudy$Reaction > 285, 1, 0)
sleepstudy$DV2 <- factor(sleepstudy$DV2)

(gm2 <- glmer(DV2 ~ Days + (1 | Subject), sleepstudy, family = "binomial"))

(gm3 <- glmer(DV2 ~ 0 + (1 | Subject) + (1 | Days), sleepstudy,  family = "binomial"))

(gm3 <- glmer(DV2 ~ 0 + (1 | Subject) + (1 | Days), sleepstudy,  family = "binomial"))
(gm4 <- glmer(DV2 ~ 0 + (1 | Subject) + (Noise | Days) + (1|Wave), sleepstudy, 
              family = "binomial"))


apsrtable(gm2, gm3, gm4, lev = 0)


## Regular

gmA <- glm(DV2 ~ Days, data = sleepstudy, family = "binomial")
lmA <- lm(Reaction ~ Days, data = sleepstudy)

apsrtable(gmA)
apsrtable(lmA)

apsrtable(gm2, gm3, gm4, float = "sidewaystable", label= "duh", caption = "please", 
          notes = list(stars.note, "My note."), 
          omitcoef=list("distid", 
                        expression(grep("distid", coefnames, value=TRUE)), 
                        "year", 
                        expression(grep("year", coefnames, value = TRUE))), 
          Sweave=FALSE)
# summod <- summary(fm4)
# 
# GroupList <- vector("list", length(summod$varcor) * 2)
# nams <- paste0("Group:", names(summod$varcor))
# names(GroupList) <- paste0(rep(nams, times = 2), rep(c(" Effs.", " Var."), 
#                                              each = length(summod$varcor)))
# for(i in 1:length(summod$varcor)){
#   GroupList[[i]] <- paste(names(attr(summod$varcor[[i]], "stddev")), " | ")
#   GroupList[[i + length(summod$varcor)]] <- paste(round(attr(summod$varcor[[i]], "stddev"), 
#                                      digits = 3), collapse = " | ")
# }
# GroupList["Sigma"] <- attr(summod$varcor, "sc")
# 
# 
# model.info <- list(
#   "$N$"=formatC(as.numeric(summod$devcomp$dims['n']),format="d"),
#   "AIC"=formatC(as.numeric(summod$AICtab),
#                 format="f",digits=3),
#   "N Groups"=as.numeric(summod$ngrps), 
#   "Group Names" = names(summod$varcor))
