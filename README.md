Data sources
============
https://www.msci.com/end-of-day-data-search (MSCI World ex-USA, Gross)

https://fred.stlouisfed.org/series/TB3MS

https://fred.stlouisfed.org/series/WILL5000IND

https://fred.stlouisfed.org/series/BAMLCC0A0CMTRIV

Usage
=====
Download GEM.R and the data sets in the same directory you're running R.

R> source('GEM.R')

R> series = buildAll() # assumes current directory contains all .csv data sets

R> plotPrem12MNextW5000AGG(series)

R> plot10YRolling(series)

R> perf(subset(series, select=c(Ret1M.w5000, Yield1M.tb3m)))

R> perf(subset(series, select=c(Ret1M.agg, Yield1M.tb3m)))

R> perf(subset(series, select=c(Ret1M.gem.abs, Yield1M.tb3m)))

To change the allocation threshold to the original model, look for addGEM(series, 0.025) and replace 0.025 with 0
