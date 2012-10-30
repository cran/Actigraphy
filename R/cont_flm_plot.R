cont_flm_plot <-
function(smoothdata, matchresults, flmresults, xlim, ylim, ftest, 
nperm, lb, xat, legendx, legendy, L, xlab="Time", ylab="Activity"){

colort <- factor(matchresults$cov[,3])
ucont <- length(unique(colort))
outcome <- ylab
covname <- names(matchresults$cov[3])
maintitle<- paste(ylab, "~", covname, sep="")

LofLegend <-ifelse(length(levels(colort)) <= 100, 10, 1)

minmedmax <- function(contvar){
contvar <- contvar
mincont <- signif(min(contvar), 3)
medcont <- signif(median(contvar), 3)
maxcont <- signif(max(contvar), 3)
contlab <- list(mincont, medcont, maxcont)
return(contlab)
}
contlabels <- minmedmax(matchresults$cov[,3])

par(mfrow=c(2,1), mar=c(4,4,3,1))
plot(0, 0, xlim=xlim, ylim=ylim, xaxt="n", xlab=xlab, 
ylab=ylab, type='n', main=maintitle)
        
for(i in 1:length(colort)) 
lines(predict(flmresults$freg$yhatfdobj, c(1:L))[,i], col=topo.colors(ucont)[colort[i]])
        
axis(1, at=xat, labels=lb)
colorsSamples <- topo.colors(ucont*LofLegend)
pnts <- cbind(x=c(legendx, (legendx + xlim[2]/20), (legendx + xlim[2]/20), legendx), y=c(legendy-ylim[2]/5, legendy, legendy, legendy-ylim[2]/5))
legend.gradient(pnts, colorsSamples, c(contlabels[[1]], contlabels[[3]]), paste(covname, "Value"))
        
geftFtestresults <- flm_ftest(smoothdata, nbasis=smoothdata$fd$fd$basis$nbasis, 
basistype="Fourier",  ftest=ftest, nperm=nperm, lb=lb, mul=1.5, xat=xat)
        
return(geftFtestresults)
}
