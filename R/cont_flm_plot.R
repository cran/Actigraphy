cont_flm_plot <-
function(smoothdata, matchresults, flmresults, xlim, ylim, ftest, 
nperm, lb, xat, yboxlow, yboxhigh, ytext, labelp, L){

colort <- factor(matchresults$cov[,3])
ucont <- length(unique(colort))
covname <- names(matchresults$cov[3])
maintitle <- paste("Activity~", covname, sep="")

par(mfrow=c(2,1), mar=c(4,4,3,1))
plot(0, 0, xlim=xlim, ylim=ylim, xaxt="n", xlab='(a)',
ylab='Acitivity', type='n', main=maintitle)

for(i in 1:length(colort)) 
lines(predict(flmresults$freg$yhatfdobj, c(1:L))[,i], col=topo.colors(ucont)[colort[i]])

axis(1, at=xat, labels=lb)
colorsSamples <- topo.colors(ucont*10)
for(i in seq(0, ucont*10, 10))
rect(i, yboxlow, i+10, yboxhigh, col=colorsSamples[i], border=NA)

minmedmax <- function(contvar){
contvar <- contvar
mincont <- signif(min(contvar), 3)
medcont <- signif(median(contvar), 3)
maxcont <- signif(max(contvar), 3)
contlab <- list(mincont, medcont, maxcont)
return(contlab)
}
contlabels <- minmedmax(matchresults$cov[,3])

text(c(1, length(colorsSamples)/2, length(colorsSamples)/3*3), 
rep(ytext, 3), labels=c(contlabels[[1]], contlabels[[2]], contlabels[[3]]), cex=.6)
text(length(colorsSamples)/2, labelp, paste(covname, "value", sep=" "), cex=.6)

geftFtestresults <- flm_ftest(smoothdata, nbasis=smoothdata$fd$fd$basis$nbasis, basistype="Fourier", 
ftest=ftest, nperm=nperm, lb=lb, mul=1.5, xat=xat)

return(geftFtestresults)
}
