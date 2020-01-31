### R code from vignette source 'Categorical-FDA-Tutorial.Rnw'

###################################################
### code chunk number 1: Categorical-FDA-Tutorial.Rnw:14-15
###################################################
	library(Actigraphy)


###################################################
### code chunk number 2: Categorical-FDA-Tutorial.Rnw:21-26
###################################################
	data(clinic_29pt_ahi)
	data(act_29pt)
	
	covariate <- clinic_29pt_ahi
	activity <- act_29pt


###################################################
### code chunk number 3: Categorical-FDA-Tutorial.Rnw:32-37
###################################################
	covariate$ahicat <- as.factor(
		ifelse(covariate$AHI >= 0 & covariate$AHI <= 5, 1, 
		ifelse(covariate$AHI > 5 & covariate$AHI <= 15, 2,
		ifelse(covariate$AHI > 15 & covariate$AHI <= 30, 3,
		ifelse(covariate$AHI > 30, 4, 0)))))


###################################################
### code chunk number 4: Categorical-FDA-Tutorial.Rnw:43-44
###################################################
	matchid <- fda.matchid(activity, covariate[,-2], "factor", c("normal", "mild", "moderate", "severe"))


###################################################
### code chunk number 5: Categorical-FDA-Tutorial.Rnw:52-55
###################################################
	L <- nrow(activity)
	FDinterest <- fda.smoothdata(matchid)
	ts.plot(predict(FDinterest$fd$fd, 1:L), main="Smoothed Activity Data")


###################################################
### code chunk number 6: Categorical-FDA-Tutorial.Rnw:60-61
###################################################
	geftinterest <- flm_cate(FDinterest)


###################################################
### code chunk number 7: Categorical-FDA-Tutorial.Rnw:71-75
###################################################
	ypred <- as.vector(geftinterest$freg$yhatfdobj$y)
	ylim <- c(0, max(ypred) + 100)
	lb <- c("Midnight", "6AM", "Noon", "6PM", "Midnight") 
	xat <- c(0, L/4, L/2, 3*L/4, L)


###################################################
### code chunk number 8: Categorical-FDA-Tutorial.Rnw:81-82
###################################################
	cat.flm.results <- cat_flm_plot(FDinterest, matchid, geftinterest, TRUE, 5, lb, xat, "AHI", 1:4, ylim, L)


