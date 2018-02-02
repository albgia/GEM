library(zoo)
library(car)

## Vectorial returns
ret = function(t) { (t[2] - t[1]) / t[1] }
## Annual yield to monthly irr
monthlyIRR = function(yield) { (1 + yield ^ 1 / 12) - 1 }
## Sharpe from monthly returns
sharpeMonthly = function(series) { r = series[,1]; rf = series[,2]; mean(r-rf) / sd(r-rf) * sqrt(12) }

loadMSCI = function(filename) {
    msci = read.csv(filename, header=T, colClasses=c("character", "character"), col.names=c("Date", "Index", NA, NA, NA))
    msci = msci[,1:2]
    msci[,1] = as.Date(msci[,1], format="%b %d, %Y")
    msci[,2] = as.numeric(gsub("\\,", "", msci[,2]))
    msci = msci[!is.na(msci$Index),] # remove NA index obs
    msci = aggregate(msci, list(Month = as.yearmon(msci$Date)), tail, 1) # reduce to last entry of each month
    msci = transform(msci, Ret1M = rollapplyr(Index, 2, ret, fill=NA))
    msci = transform(msci, Ret12M = rollapplyr(1 + Ret1M, 12, prod, fill=NA) - 1) # calculate rolling 12M returns

    msci.num = subset(msci, select=-c(Month,Date)) # Remove non-numerical variables
    msci = zoo(msci.num, frequency=12, order.by=msci$Month)
    
    return(msci)
}

loadW5000 = function(filename) {
    w5000 = read.csv(filename, header=T, colClasses=c("Date", "character"), col.names=c("Date", "Index"))
    w5000[,2] = as.numeric(w5000[,2])
    w5000 = w5000[!is.na(w5000$Index),]
    w5000 = aggregate(w5000, list(Month = as.yearmon(w5000$Date)), tail, 1) # add Month only column
    w5000 = transform(w5000, Ret1M = rollapplyr(Index, 2, ret, fill=NA)) # calculate 1M returns
    w5000 = transform(w5000, Ret12M = rollapplyr(1 + w5000$Ret1M, 12, prod, fill=NA) - 1) # calculate 12M returns

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
    agg = transform(agg, Ret1M = rollapplyr(Index, 2, ret, fill=NA))
    agg = transform(agg, Ret12M = rollapplyr(1 + agg$Ret1M, 12, prod, fill=NA) - 1)
    
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

## GEM equities allocation
allocGEM = function(series, thresh) {
    prem = series[1, "Prem12M"]
    ret12m.w5000 = series[1, "Ret12M.w5000"]
    ret12m.msci = series[1, "Ret12M.msci"]
    
    # Classic allocation
    if( thresh == 0 )
        return(allocGEM.class(prem, ret12m.w5000, ret12m.msci))

    return(allocGEM.thresh(prem, thresh, ret12m.w5000, ret12m.msci))
}

allocGEM.class = function(prem, ret12m.w5000, ret12m.msci) {
    alloc = c(.0, .0, .0)
    names(alloc) = c("w5000", "agg", "msci")
    
    if( prem > 0 ) {
        if( ret12m.w5000 > ret12m.msci )
            alloc["w5000"] = 1
        else
            alloc["msci"] = 1
    } else {
        alloc["agg"] = 1
    }

    return(alloc)
}

allocGEM.thresh = function(prem, thresh, ret12m.w5000, ret12m.msci) {
    alloc = c(.0, .0, .0)
    names(alloc) = c("w5000", "agg", "msci")
    mscimom = ret12m.msci - ret12m.w5000

    if( abs(prem) < thresh ) {
        if( mscimom > 0.05 ) {
            alloc["msci"] = .5
        } else {
            alloc["w5000"] = .5
        }
        alloc["agg"] = .5
    } else if( prem > thresh ) {
        if( mscimom > 0.05 ) {
            alloc["msci"] = 1
        } else {
            alloc["w5000"] = 1
        }
    } else {
        alloc["agg"] = 1
    }

    return(alloc)
}

## GEM abs mom/dual mom instruments allocation
addGEM = function(series, thresh) {
    ## Remove previous allocation weights to avoid generating extra columns in rollapply
    series$GEM.w5000 = NULL
    series$GEM.agg = NULL
    series$GEM.msci = NULL
    ## Allocate
    series = transform(series, GEM = rollapplyr(subset(series, select=c(Prem1M, Prem12M, Ret12M.w5000, Ret12M.msci)), 2, allocGEM, thresh, by.column=FALSE, fill=NA))
    ## Calculate portfolio returns
    series = transform(series,
        Ret1M.gem.abs = (GEM.w5000+GEM.msci) * Ret1M.w5000 + GEM.agg * Ret1M.agg,
        Ret1M.gem.dm = GEM.w5000 * Ret1M.w5000 + GEM.agg * Ret1M.agg + GEM.msci * Ret1M.msci)
    
    return(series)
}

addNext = function(series) {
    series = merge(series, NextRet1M.w5000 = lag(series$Ret1M.w5000), NextRet1M.agg = lag(series$Ret1M.agg), NextRet1M.msci = lag(series$Ret1M.msci))
    series = transform(series, NextW5000AGG = NextRet1M.w5000 > NextRet1M.agg)

    return(series)
}

addSharpe = function(series) {
    series = transform(series,
        Sharpe.w5000 = rollapplyr(subset(series, select=c(Ret1M.w5000, Yield1M.tb3m)), 12*10, sharpeMonthly, by.column=FALSE, fill=NA),
        Sharpe.gem.abs = rollapplyr(subset(series, select=c(Ret1M.gem.abs, Yield1M.tb3m)), 12*10, sharpeMonthly, by.column=FALSE, fill=NA),
        Sharpe.gem.dm = rollapplyr(subset(series, select=c(Ret1M.gem.dm, Yield1M.tb3m)), 12*10, sharpeMonthly, by.column=FALSE, fill=NA))

    return(series)
}

## Rolling 10Y average returns
addAR10Y = function(series) {
    series = transform(series,
        AR10Y.w5000 = rollapplyr(Ret1M.w5000, 12*10, mean, fill=NA),
        AR10Y.gem.abs = rollapplyr(Ret1M.gem.abs, 12*10, mean, fill=NA),
        AR10Y.gem.dm = rollapplyr(Ret1M.gem.dm, 12*10, mean, fill=NA))
}

buildAll = function() {
    w5000 = loadW5000("WILL5000IND.csv")
    msci = loadMSCI("MSCI_WORLD_ex_US.csv")
    agg = loadAGG("BAMLCC0A0CMTRIV.csv")
    tb3m = loadTB3M("TB3MS.csv")

    series = addPremium(merge(w5000, msci, agg, tb3m, all=TRUE))
    series = addGEM(series, 0.025)
    series = addNext(series)
    series = addSharpe(series)
    series = addAR10Y(series)

    return(series[!is.na(series$Prem12M),])
}

##testCases = function(series) {
##    by.year = aggregate(index(series), list(Year = as.integer(as.yearmon(index(series)))), length)
##    if( nrow((incomplete = subset(by.year, x<12))) > 0 )
##        warning(print(incomplete))
##}

## Plot 10 years GEM - Wilshire 5000 rolling performance
plot10YRolling = function(series) {
    sharpe10 = na.omit(subset(series, select=c(Sharpe.w5000, Sharpe.gem.dm, AR10Y.gem.dm, AR10Y.w5000)))
    sharpe10 = transform(sharpe10, diff=Sharpe.gem.dm - Sharpe.w5000)
    first.year = as.integer(head(index(sharpe10),1))
    last.year = as.integer(tail(index(sharpe10),1))
    plot(cbind(sharpe10, base=rep(0, nrow(sharpe10))), # cbind Sharpe difference 0 horizontal line
         screens=c(1,1,3,3,2,2), col=c("gray","red","red","gray","black","gray"),
         main="Rolling 10 Years Dual Momentum vs. Wilshire 5000", ylab=c("Sharpe", "Excess Sharpe", "Avg. Return"), cex.lab=0.75,
         xaxt = "n", panel=plotMulti)
    legend("bottomleft", c("Dual Momentum", "Wilshire 5000"), col=c("red", "gray"), lty=1, cex=0.75)
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
    scatterplot(NextW5000AGG ~ Prem12M, data=coredata(series), subset=(abs(Prem12M) < 0.3),
                smoother=gamLine, smoother.args=list(family=binomial, link="probit"),
                reg.line=FALSE, boxplot="x",
                main="Next Month Return Probability W5000 > AGG", cex.main=0.9,
                axes=FALSE, reset.par=FALSE, yaxp=c(0.1, 10, 10), grid=FALSE)
    ## Grid
    xticks = sort(c(-1*seq(0.1, 0.4, 0.1), seq(0.1, 0.4, 0.1), seq(-0.06, 0.06, 0.02)))
    yticks = seq(0.1, 1.0, 0.1) 
    axis(1, xticks, las=3, cex.axis=0.8)
    axis(2, yticks, cex.axis=0.8)
    abline(h=yticks, v=xticks, col="gray", lty=3)
    abline(v=0.0, col="green", lty=2)
}

plotMom12NextMSCIW5000 = function(series) {
    series = subset(series, Prem12M > -0.05, select=c(Ret1M.w5000, Ret1M.msci, Ret12M.msci, Ret12M.w5000, NextRet1M.w5000, NextRet1M.msci))
    series = transform(series,
        NextMSCIW5000 = NextRet1M.msci > NextRet1M.w5000,
        MomMSCI = Ret12M.msci - Ret12M.w5000)
    scatterplot(NextMSCIW5000 ~ MomMSCI, data=coredata(series), subset=(abs(MomMSCI) < 0.4),
                smoother=gamLine, smoother.args=list(family=binomial, link="probit"),
                reg.line=FALSE, boxplot="x",
                main="Next Month Return Probability MSCI > W5000", cex.main=0.9,
                axes=FALSE, reset.par=FALSE, yaxp=c(0.1, 10, 10), grid=FALSE)
    ## Grid
    xticks = sort(c(-1*seq(0.1, 0.4, 0.1), seq(0.1, 0.4, 0.1), seq(-0.06, 0.06, 0.02)))
    yticks = seq(0.1, 1.0, 0.1) 
    axis(1, xticks, las=3, cex.axis=0.8)
    axis(2, yticks, cex.axis=0.8)
    abline(h=yticks, v=xticks, col="gray", lty=3)
    abline(v=0.0, col="green", lty=2)
}

## performance metrics from monthly returns
## series parameter must include instrument monthly returns and risk-free monthly rate
perf = function(series) {
    tr = prod(1 + series[,1]) - 1 # total return
    mr = mean(series[,1]) # average returns
    years.time = nrow(series) / 12 # time in years
    ar = tr ^ (1 / years.time) - 1 # annualized return
    asd = sd(series[,1]) * sqrt(12) # annualized volatility
    sr = sharpeMonthly(series)
    um = sum(series[,1] > 0) / nrow(series)

    eqt = cumprod(1 + series[,1])
    peaks = cummax(eqt)
    mdd = min(eqt / peaks - 1)

    p = c(ar, asd, sr, mdd, um)
    names(p) = c("AR", "ASD", "SR", "MDD", "UM")
    
    return(p)
}

perf.gem.abs = function(series) {
    perf(na.omit(subset(series, select=c(Ret1M.gem.abs, Yield1M.tb3m))))
}

perf.gem.dm = function(series) {
    perf(na.omit(subset(series, select=c(Ret1M.gem.dm, Yield1M.tb3m))))
}

perf.ws5000 = function(series) {
    perf(na.omit(subset(series, select=c(Ret1M.w5000, Yield1M.tb3m))))
}

perf.agg = function(series) {
    perf(na.omit(subset(series, select=c(Ret1M.agg, Yield1M.tb3m))))
}

## daily W5000 data starts 1979-11-30

## w1 = window(series, start="Jan 1970", end="Sep 1990")
## w2 = window(series, start="Oct 1990", end="Oct 2000")
## w3 = window(series, start="Nov 2000", end="Dec 2017")
##
## summary(subset(w2, Prem12M < 0, select=Prem12M))
## summary(subset(rbind(w1,w3), Prem12M < 0, select=Prem12M))

## png export parameters
## png(filename="10YRolling.png", width=800, height=800, type="cairo", pointsize=18)
