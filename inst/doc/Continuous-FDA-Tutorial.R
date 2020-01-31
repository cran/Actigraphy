### R code from vignette source 'Continuous-FDA-Tutorial.Rnw'

###################################################
### code chunk number 1: Continuous-FDA-Tutorial.Rnw:14-15
###################################################
	library(Actigraphy)


###################################################
### code chunk number 2: Continuous-FDA-Tutorial.Rnw:21-26
###################################################
	data(clinic_29pt_ahi)
	data(act_29pt)
	
	covariate <- clinic_29pt_ahi
	activity <- act_29pt


###################################################
### code chunk number 3: Continuous-FDA-Tutorial.Rnw:31-32
###################################################
	matchid <- fda.matchid(activity, covariate, "contin")


###################################################
### code chunk number 4: Continuous-FDA-Tutorial.Rnw:40-43
###################################################
	L <- nrow(activity)
	FDinterest <- fda.smoothdata(matchid)
	ts.plot(predict(FDinterest$fd$fd, 1:L), main="Smoothed Activity Data")


###################################################
### code chunk number 5: Continuous-FDA-Tutorial.Rnw:48-49
###################################################
	geftFDcont <- flm_cate(FDinterest)


###################################################
### code chunk number 6: Continuous-FDA-Tutorial.Rnw:64-73
###################################################
	predy <- as.vector(geftFDcont$freg$yhatfdobj$y)
	
	xlim <- c(0, L) 
	ylim <- c(min(predy), max(predy) + 100) 
	lb <- c("Midnight", "6AM", "Noon", "6PM", "Midnight") 
	xat <- c(0, L/4, L/2, 3*L/4, L)
	
	legendx <- 0
	legendy <- max(predy) - 100


###################################################
### code chunk number 7: Continuous-FDA-Tutorial.Rnw:79-80
###################################################
	cont.flm.results <- cont_flm_plot(FDinterest, matchid, geftFDcont, xlim, ylim, TRUE, 10, lb, xat, legendx, legendy, L)


