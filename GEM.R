library(zoo)
library(car)

## daily W5000 data starts 1979-11-30

## sharpe.down = window(x, start="Oct 1990", end="Oct 2000")
## Sharpe down performance due to switch to AGG from Oct 1990 (highest spread btw Sharpe ratios for the period) to Oct 2000 (lowest)
## Only 16 months were GEM was in AGG, AGG mean and median returns not different than the rest of the data
## Premium leading to AGG significantly lower

## start="Jan 1970", end="Sep 1990"
## start="Oct 1990", end="Oct 2000"
## start="Nov 2000", end="Dec 2017"

xnext = function(v, k=1) { if( k > length(v) ) k = length(v); c(tail(v, -k), rep(NA, k)) }
xprev = function(v, k=1) { if( k > length(v) ) k = length(v); c(rep(NA, k), head(v, -k)) }
ret = function(t) { (t[2] - t[1]) / t[1] }
ema = function(t, p) { a = 2 / (p + 1); (t[2] * a) + t[1] * (1 - a) }

## Annual Yield to monthly IRR
monthlyIRR = function(yield) { (1 + yield ^ 1 / 12) - 1 }
## Monthly returns Sharpe
sharpeMonthly = function(series) { r = series[,1]; rf = series[,2]; mean(r-rf) / sd(r-rf) * sqrt(12) }

loadMSCI = function(filename) {
    msci = read.csv(filename, header=T, colClasses=c("character", "character"), col.names=c("Date", "Index", NA, NA, NA))
    msci = msci[,1:2]
    msci[,1] = as.Date(msci[,1], format="%b %d, %Y")
    msci[,2] = as.numeric(gsub("\\,", "", msci[,2]))
    msci = msci[!is.na(msci$Index),] # remove NA index obs
    msci = aggregate(msci, list(Month = as.yearmon(msci$Date)), tail, 1) # reduce to last entry of each month
    msci = transform(msci, Ret1M = rollapplyr(Index, 2, ret, fill=NA, partial=FALSE))
    msci = transform(msci, Ret12M = rollapplyr(1 + Ret1M, 12, prod, fill=NA, partial=FALSE) - 1) # calculate rolling 12M returns

    msci.num = subset(msci, select=-c(Month,Date)) # Remove non-numerical variables
    msci = zoo(msci.num, frequency=12, order.by=msci$Month)
    
    return(msci)
}

loadW5000 = function(filename) {
    w5000 = read.csv(filename, header=T, colClasses=c("Date", "character"), col.names=c("Date", "Index"))
    w5000[,2] = as.numeric(w5000[,2])
    w5000 = w5000[!is.na(w5000$Index),]
    w5000 = aggregate(w5000, list(Month = as.yearmon(w5000$Date)), tail, 1) # add Month only column
    w5000 = transform(w5000, Ret1M = rollapplyr(Index, 2, ret, fill=NA, partial=FALSE)) # calculate 1M returns
    w5000 = transform(w5000, Ret12M = rollapplyr(1 + w5000$Ret1M, 12, prod, fill=NA, partial=FALSE) - 1) # calculate 12M returns

    w5000.num = subset(w5000, select=-c(Month,Date)) # Remove non-numerical date variables that will be replaced by zoo index
    w5000 = zoo(w5000.num, frequency=12, order.by=w5000$Month)
    
    return(w5000)
}

loadTB3M = function(filename) {
    tb3m = read.csv(filename, header=T, colClasses=c("Date", "numeric"), col.names=c("Date", "Yield1Y.tb3m"))
    tb3m = aggregate(tb3m, list(Month = as.yearmon(tb3m$Date)), tail , 1) # last of each month
    tb3m = transform(tb3m, Yield1Y.tb3m = Yield1Y.tb3m / 100, Yield1M.tb3m = monthlyIRR(Yield1Y.tb3m / 100)) # Add monthly IRR

    tb3m.num = subset(tb3m, select=-c(Month, Date)) # Remove non-numerical date variables, will be replaced by zoo date index
    tb3m = zoo(tb3m.num, frequency=12, order.by=tb3m$Month)

    return(tb3m)
}

loadAGG = function(filename) {
    agg = read.csv(filename, header=T, colClasses=c("Date", "character"), col.names=c("Date", "Index"))
    agg[,2] = as.numeric(agg[,2])
    agg = agg[!is.na(agg$Index),]
    agg = aggregate(agg, list(Month = as.yearmon(agg$Date)), tail, 1)
    agg = transform(agg, Ret1M = rollapplyr(Index, 2, ret, fill=NA, partial=FALSE))
    agg = transform(agg, Ret12M = rollapplyr(1 + agg$Ret1M, 12, prod, fill=NA, partial=FALSE) - 1)
    
    agg.num = subset(agg, select=-c(Month, Date))
    agg = zoo(agg.num, frequency=12, order.by=agg$Month)

    return(agg)
}

## Market excess returns
addPremium = function(series) {
    series = merge(series, PrevYield1Y.tb3m = lag(series$Yield1Y.tb3m, -12))
    series = transform(series, Prem1M = Ret1M.w5000 - Yield1M.tb3m, Prem12M = Ret12M.w5000 - PrevYield1Y.tb3m)

    return(series[complete.cases(series),])
}

## GEM abs mom/dual mom instrument selection
addGEM = function(series) {
    ## 0 = AGG, 1 = W5000, 2 = MSCI
    GEM.abs = with(series, ifelse(Prem12M < 0, 0, 1))
    GEM.dm = with(series, ifelse(Prem12M < 0, 0, ifelse(Ret12M.w5000 > Ret12M.msci, 1, 2)))
    ## Instrument selection must be lagged by 1 month based on previous 12M returns
    series = merge(series, GEM.abs = lag(GEM.abs, k=-1))
    series = merge(series, GEM.dm = lag(GEM.dm, k=-1))
    ## Add selection return
    series = transform(series,
        Ret1M.gem.abs = ifelse(GEM.abs == 0, Ret1M.agg, Ret1M.w5000),
        Ret1M.gem.dm = ifelse(GEM.dm == 0, Ret1M.agg, ifelse(GEM.dm == 1, Ret1M.w5000, Ret1M.msci)))
    
    return(series)
}

## next period returns
addNext = function(series) {
    series = merge(series, NextRet1M.w5000 = lag(series$Ret1M.w5000), NextRet1M.agg = lag(series$Ret1M.agg), NextRet1M.msci = lag(series$Ret1M.msci))
    series = transform(series, NextW5000AGG = NextRet1M.w5000 > NextRet1M.agg)

    return(series)
}

addSharpe = function(series) {
    series = transform(series,
        Sharpe.w5000 = rollapplyr(subset(series, select=c(Ret1M.w5000, Yield1M.tb3m)), 12*10, sharpeMonthly, by.column=FALSE, fill=NA, partial=FALSE),
        Sharpe.gem.abs = rollapplyr(subset(series, select=c(Ret1M.gem.abs, Yield1M.tb3m)), 12*10, sharpeMonthly, by.column=FALSE, fill=NA, partial=FALSE),
        Sharpe.gem.dm = rollapplyr(subset(series, select=c(Ret1M.gem.dm, Yield1M.tb3m)), 12*10, sharpeMonthly, by.column=FALSE, fill=NA, partial=FALSE))

    return(series)
}

## Rolling 10Y average returns
addAR10Y = function(series) {
    series = transform(series,
        AR10Y.w5000 = rollapplyr(Ret1M.w5000, 12*10, mean, fill=NA, partial=FALSE),
        AR10Y.gem.abs = rollapplyr(Ret1M.gem.abs, 12*10, mean, fill=NA, partial=FALSE),
        AR10Y.gem.dm = rollapplyr(Ret1M.gem.dm, 12*10, mean, fill=NA, partial=FALSE))
}

quarterDecompCol = function() {
    paste("Q", as.character(seq(1:4)), sep="")
}

quarterDecomp = function(series) {
    targetW5000Ret = series[13, "Ret1M.w5000"]
    targetAGGRet = series[13, "Ret1M.agg"]
    PremQ1 = prod(1 + series[1:3, "Prem1M"]) - 1
    PremQ2 = prod(1 + series[4:6, "Prem1M"]) - 1
    PremQ3 = prod(1 + series[7:9, "Prem1M"]) - 1
    PremQ4 = prod(1 + series[10:12, "Prem1M"]) - 1
    
    v = as.integer(c(
        (PremQ1 > 0 & targetW5000Ret > targetAGGRet) | (PremQ1 < 0 & targetW5000Ret < targetAGGRet),
        (PremQ2 > 0 & targetW5000Ret > targetAGGRet) | (PremQ2 < 0 & targetW5000Ret < targetAGGRet),
        (PremQ3 > 0 & targetW5000Ret > targetAGGRet) | (PremQ3 < 0 & targetW5000Ret < targetAGGRet),
        (PremQ4 > 0 & targetW5000Ret > targetAGGRet) | (PremQ4 < 0 & targetW5000Ret < targetAGGRet)))
    
    names(v) = quarterDecompCol()
    
    return(v)
}

quarterProb = function(series) {
    Q4 = subset(series, select=quarterDecompCol())
    Q4 = Q4[complete.cases(Q4),]
    apply(Q4, 2, function(x) binom.test(sum(x), length(x))$conf.int[1:2])
}

addQuarterDecomp = function(series) {
    series = cbind(series, rollapplyr(subset(series, select=c(Prem1M, Ret1M.w5000, Ret1M.agg)), 13, quarterDecomp, by=12, by.column=FALSE, fill=NA, partial=FALSE, coredata=TRUE))

    return(series)
}

testCases = function(series) {
    by.year = aggregate(index(series), list(Year = as.integer(as.yearmon(index(series)))), length)
    if( nrow((incomplete = subset(by.year, x<12))) > 0 )
        warning(print(incomplete))
}

buildAll = function() {
    w5000 = loadW5000("WILL5000IND.csv")
    msci = loadMSCI("MSCI_WORLD_ex_US.csv")
    agg = loadAGG("BAMLCC0A0CMTRIV.csv")
    tb3m = loadTB3M("TB3MS.csv")
    
    series = addQuarterDecomp(addAR10Y(addNext(addSharpe(addGEM(addPremium(merge(w5000, msci, agg, tb3m, all=TRUE)))))))
    return(series[!is.na(series$Prem12M),])
}

## Plot 10 years GEM - Wilshire 5000 rolling performance
plot10YRolling = function(series) {
    sharpe10 = na.omit(subset(series, select=c(Sharpe.w5000, Sharpe.gem.abs, AR10Y.gem.abs, AR10Y.w5000)))
    sharpe10 = transform(sharpe10, diff=Sharpe.gem.abs - Sharpe.w5000)
    first.year = as.integer(head(index(sharpe10),1))
    last.year = as.integer(tail(index(sharpe10),1))
    plot(cbind(sharpe10, base=rep(0, nrow(sharpe10))), # cbind Sharpe difference 0 horizontal line
         screens=c(1,1,3,3,2,2), col=c("gray","red","red","gray","black","gray"),
         main="Rolling 10 Years GEM vs. Wilshire 5000", ylab=c("Sharpe", "Excess Sharpe", "Avg. Return"), cex.lab=0.75,
         xaxt = "n", panel=plotMulti)
    legend("bottomleft", c("GEM", "Wilshire 5000"), col=c("red", "gray"), lty=1, cex=0.75)
}

plotMulti = function(x, y, ..., pf = parent.frame()) {
    lines(x, y, ...)
    ix = seq(1, length(x), 12)
    abline(v=x[ix], col="gray", lty=3)
    ## if bottom panel
    if( with(pf, length(panel.number) == 0 || panel.number %% nr == 0 || panel.number == nser)) {
        labs = format(x, "%Y")
        axis(side = 1, at = x[ix], labels = labs[ix], las=3)
    }
}

plotPrem12MNextW5000AGG = function(series) {
#    series = cbind(data.frame(coredata(series)),
#        Period = as.factor(ifelse(index(series) > "Oct 1990" & index(series) < "Oct 2000", "1990-2000", "Remaining")))

    scatterplot(NextW5000AGG ~ Prem12M | Period, data=coredata(series), subset=(abs(Prem12M) < 0.3),
                smoother=loessLine, smoother.args=list(familiy="binomial"), span=0.5,
                reg.line=FALSE, boxplot="x",
                main="Next Month Return Probability WS5000 > AGG", cex.main=0.8,
                axes=FALSE, reset.par=FALSE, yaxp=c(0.1, 10, 10), grid=FALSE,
                legend.plot=TRUE, legend.coords="bottomleft", cex.lab=0.75)
    ## Grid
    xticks = sort(c(-1*seq(0.1, 0.4, 0.1), seq(0.1, 0.4, 0.1), seq(-0.06, 0.06, 0.02)))
    yticks = seq(0.1, 1.0, 0.1) 
    axis(1, xticks, las=3, cex.axis=0.8)
    axis(2, yticks, cex.axis=0.8)
    abline(h=yticks, v=xticks, col="gray", lty=3)
    abline(v=0.0, col="green", lty=2)
}
