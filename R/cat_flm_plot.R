cat_flm_plot <-
function(smoothdata, matchresults, flmresults, ftest, nperm, lb, xat, varname, col, ylim, L){
geft <- flmresults
maintitle <- paste("Activity~", varname, sep="")
leg <- names(matchresults$cov)[-1]

par(mfrow=c(2,1), mar=c(4,4,3,1))

if(missing(geft) || missing(lb) || missing(leg) || missing(col) || missing(ylim) || missing(maintitle )) 
stop("Error: Missing geft, lb, leg, col, ylim, and/or main")

color4cat <- col
l <- length(geft$freg$betaestlist)
beta <- geft$freg$betaestlist
std <- geft$fregstd$betastderrlist

plot(0, 0, xlim=c(0, L), ylim=ylim, xlab="(a)", 
ylab="Activity", type="n", main=maintitle, xaxt="n", 
cex.main=0.8)
lines(beta[[1]]$fd, col=col[1], lwd=2)

for(i in 2:l) 
lines(beta[[1]]$fd + beta[[i]]$fd, col=col[i], lwd=2)

axis(1, at=xat, labels=lb)
legend("topleft", c(leg), col=color4cat, lty=rep(1,length(beta)), cex=0.8, lwd=2)

geftFtestresults <- flm_ftest(smoothdata, nbasis=smoothdata$fd$fd$basis$nbasis, basistype=smoothdata$fd$fd$basis[[2]], 
ftest=ftest, nperm=nperm, lb=lb, mul=1.5, xat=xat)

return(geftFtestresults)
}
