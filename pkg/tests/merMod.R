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

summod <- summary(fm4)

GroupList <- vector("list", length(summod$varcor) * 2)
nams <- paste0("Group:", names(summod$varcor))
names(GroupList) <- paste0(rep(nams, times = 2), rep(c(" Effs.", " Var."), 
                                             each = length(summod$varcor)))
for(i in 1:length(summod$varcor)){
  GroupList[[i]] <- paste(names(attr(summod$varcor[[i]], "stddev")), " | ")
  GroupList[[i + length(summod$varcor)]] <- paste(round(attr(summod$varcor[[i]], "stddev"), 
                                     digits = 3), collapse = " | ")
}
GroupList["Sigma"] <- attr(summod$varcor, "sc")


model.info <- list(
  "$N$"=formatC(as.numeric(summod$devcomp$dims['n']),format="d"),
  "AIC"=formatC(as.numeric(summod$AICtab),
                format="f",digits=3),
  "N Groups"=as.numeric(summod$ngrps), 
  "Group Names" = names(summod$varcor))
