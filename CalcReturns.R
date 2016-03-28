
CheckClass <- function(checkObj, classType){
    if(class(checkObj) != classType){
        tryCatch({
            e        <- expression(paste0("as.",classType,"(",x,")"))
            checkObj <- eval(e,list(x=checkObj))
            
        },error= function(e){
            stop(paste0(checkObj," must be class ",
                        classType," Unable to convert."))
        })
        return(checkObj)
    }
}

GetReturns <- 
    function (ticker, 
              freq = c("month", "week", "day"), 
              get = c("overlapOnly", "all"), 
              start = "1970-01-01",
              end = NULL) {
        
        URL      <- list()
        full     <- list()
        r        <- list()
        dates    <- list()
        n        <- length(ticker)
        
        if (!is.null(end))
            end <- paste0("&d=", month(end) - 1, "&e=", mday(end), "&f=", year(end))
        start <- paste0("&a=", month(start) - 1, "&b=", mday(start), "&c=", year(start))
        
        N        <- rep(-1, n)
        startURL <- "http://ichart.finance.yahoo.com/table.csv?s="
        endURL   <- paste0(start, end, "&g=", substr(freq, 1, 1), "&ignore=.csv")
        
        minDate <- as.Date("2499-12-31")
        
        URL        <- paste0(startURL, ticker, endURL)
        names(URL) <- ticker
        
        full <- sapply(URL, 
                       fread, 
                       showProgress = F, 
                       drop = c("High","Low","Volume"),
                       simplify = F)
        
        Returns <- lapply(full, 
                          function(i){
                              a <- i[-.N,`Adj Close`]
                              b <- i[-1, `Adj Close`]
                              i[, Return := c(NA, (a-b)/b)]
                          })
        
        for (i in 1:n) {

            
            full[[ticker[i]]] <- d
            head(d)
            
            i=full[["BAC"]]
            

            
            a <- d[-dim(d)[1], 7]
            head(a)
            b <- d[-1, 7]
            c <- d[-1, 7]
            
            r[[i]]     <- (a - b)/c
            dates[[i]] <- as.Date(d[-dim(d)[1], 1])
            minDate    <- min(c(minDate, d[, 1]))
            
            N[i] <- length(r[[i]])
        }
        
        uDates      <- rev(sort(unique(c(dates, recursive = TRUE) - 1)))
        R           <- matrix(NA, length(uDates), n)
        rownames(R) <- as.character(as.Date(uDates, origin = minDate))
        
        for (i in 1:n) {
            inR <- match(as.character(dates[[i]]), rownames(R))
            R[inR, i] <- r[[i]]
        }
        
        if (get[1] == "overlapOnly") {
            toRemove <- which(apply(is.na(R), 1, any))
            if (all(diff(toRemove) == 1) | substr(freq, 1, 1) != "m") {
                if (length(toRemove) > 0) {
                    R <- R[-toRemove, ]
                }
            }
            else {
                keep <- rep(0, length(toRemove))
                theDates <- as.Date(rownames(R)[toRemove], "%Y-%m-%d")
                theMonths <- months(theDates)
                theYears <- format(theDates, "%Y")
                toCombine <- 0
                for (i in 1:(length(toRemove) - 1)) {
                    cond1 <- theMonths[i] == theMonths[i + 1]
                    cond2 <- theYears[i] == theYears[i + 1]
                    cond3 <- abs(as.numeric(theDates[i] - theDates[i + 1])) < 7
                    if ((cond1 & cond2) | cond3) {
                        if (keep[i] > 0) {
                            keep[i + 1] <- keep[i]
                        }
                        else {
                            toCombine   <- toCombine + 1
                            keep[i]     <- toCombine
                            keep[i + 1] <- toCombine
                        }
                    }
                }
                if (any(keep == 0)) {
                    R <- R[-toRemove[keep == 0], ]
                }
                nRemoved <- 0
                for (i in 1:toCombine) {
                    combineThese <- toRemove[keep == i]
                    inThisRow    <- combineThese[1]
                    for (k in 2:length(combineThese)) {
                        thisRow <- combineThese[k]
                        for (j in 1:ncol(R)) {
                            if (!is.na(R[thisRow - nRemoved, j])) {
                                R[inThisRow - nRemoved, j] <- R[thisRow - nRemoved, j]
                            }
                        }
                    }
                    R        <- R[-(combineThese[-1] - nRemoved), ]
                    nRemoved <- nRemoved + length(combineThese[-1])
                }
            }
        }
        
        if (!is.matrix(R)) {
            R           <- matrix(R, ncol = 1)
            rownames(R) <- as.character(as.Date(uDates, origin = minDate))
        }
        
        colnames(R) <- ticker
        start       <- rownames(R)[dim(R)[1]]
        end         <- rownames(R)[1]
        
        temp <- list(R      = R, 
                     ticker = ticker, 
                     period = period, 
                     start  = start, 
                     end    = end, 
                     full   = full)
        class(temp) <- "stockReturns"
        return(temp)
    }