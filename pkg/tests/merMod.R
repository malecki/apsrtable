# Test merMod

library(lme4)

(fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy))
summary(fm1)# (with its own print method)

apsrtable(fm1, lev = 0)

(fm2 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy))

(fm3 <- lmer(Reaction ~ 0 + (1 | Subject) + (1 | Days), sleepstudy))

apsrtable(fm1, fm2, lev = 0)

summod <- summary(fm3)

GroupList <- vector("list", length(summod$varcor) * 2)
nams <- paste0("Group:", names(summod$varcor))
names(GroupList) <- paste0(rep(nams, 2), rep(c(" Effs.", " Var."), 
                                             length(summod$varcor)))
for(i in 1:length(summod$varcor)){
  GroupList[[i]] <- 
  
}
