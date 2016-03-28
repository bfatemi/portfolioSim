#######################################################################
#   Run script
#######################################################################

#getReturns calculates daily returns starting from Jan 1st 2010
# s      <- getReturns(ticker=symbols,freq=freq,start=Sys.Date()-365*years)

# dt.ret <- data.table(s$R, keep.rownames=T)
# setnames(dt.ret, "rn", "Date")

# dt.ret.w <- melt(dt.ret, id.vars = "Date", 
#                  value.name = "Return",
#                  variable.name = "Ticker")
# 
# Anno <- gvisAnnotationChart(dt.ret.w, 
#                             datevar="Date",
#                             numvar="Return", 
#                             idvar="Ticker",
#                             options=list(
#                               width=600, height=350,
#                               fill=10, displayExactValues=TRUE,
#                               colors="['#0000ff','#00ff00']"))
# plot(Anno)
#aveReturns <- s$ticker