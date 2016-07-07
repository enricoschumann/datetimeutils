## -*- truncate-lines: t; comment-column: 50; -*-

                                        # DATES

previousBusinessDay <- function (x, holidays = NULL, shift = -1) {
    if (!all(inherits(x, "Date") | inherits(x, "POSIXt")))
        stop("input must inherit from classes ",
             sQuote("Date"), " or ", sQuote("POSIXt"))
    x <- as.Date(x)
    if (shift == -1L || shift == 0L) {
        x <- x + shift
        tmp <- as.POSIXlt(x)
        tmpi <- tmp$wday == 6L
        x[tmpi] <- x[tmpi] - 1L
        tmpi <- tmp$wday == 0L
        x[tmpi] <- x[tmpi] - 2L
    } else {
        for (i in 1:(-shift)) {
            x <- x - 1
            tmp <- as.POSIXlt(x)
            tmpi <- tmp$wday == 6L
            x[tmpi] <- x[tmpi] - 1L
            tmpi <- tmp$wday == 0L
            x[tmpi] <- x[tmpi] - 2L
        }
    }
    x
}

nextBusinessDay <- function(x, holidays = NULL, shift = 1) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    x <- as.Date(x)
    if (shift == 1L || shift == 0L) {
        x <- x + shift
        tmp <- as.POSIXlt(x)
        tmpi <- tmp$wday == 6L
        x[tmpi] <- x[tmpi] + 2L
        tmpi <- tmp$wday == 0L
        x[tmpi] <- x[tmpi] + 1L
        
    } else {
        for (i in 1:shift) {
            x <- x + 1
            tmp <- as.POSIXlt(x)
            tmpi <- tmp$wday == 6L
            x[tmpi] <- x[tmpi] + 2L
            tmpi <- tmp$wday == 0L
        x[tmpi] <- x[tmpi] + 1L
        }
    }
    x
}

isWeekend <- function(x) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$wday == 0L | tmp$wday == 6L
}

isLeapyear <- function(x)
    x %% 4 == 0 & (x %% 100 != 0 | x %% 400 == 0)

## isBusinessDay <- function(x, holidays = NULL) {
##     if (!is.null(holidays))
##         .NotYetUsed("holidays", FALSE)
##     !isWeekend(x)
## }

firstOfMonth <- function (x) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mday <- 1L
    as.Date(tmp)
}

firstOfYear <- function (x) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mday <- 1L
    tmp$mon <- 0L
    as.Date(tmp)
}

endOfMonth <- function(x, shift = 0L) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mon <- tmp$mon + 1L + shift
    tmp$mday <- 1L
    as.Date(tmp) - 1L
}

endOfYear <- function(x, shift = 0L) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mon <- 12+shift
    tmp$mday <- 1L
    as.Date(tmp) - 1L
}

endOfPreviousMonth <- function(x) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mday <- 1L
    as.Date(tmp) - 1L
}

endOfPreviousYear <- function(x) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mday <- 31L
    tmp$mon <- 11L
    tmp$year <- tmp$year - 1L
    as.Date(tmp)
}

mday <- function(x)
    dayOfMonth(x)

`mday<-` <- function(x, value){
    dayOfMonth(x) <- value
    x
}

dayOfMonth <- function(x) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    as.POSIXlt(x)$mday
}

`dayOfMonth<-` <- function(x, value) {
    if (!all(inherits(x, "Date") | inherits(x, "POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    cl <- class(x)
    tmp <- as.POSIXlt(x)
    tmp$mday <- value
    if (cl == "Date")
        as.Date(tmp)
    else if (cl == "POSIXct")
        as.POSIXct(tmp)
    else 
        tmp
}

lastWeekday <- function(weekday, x, shift = 0L,
                        period = "month", before, inclusive = TRUE) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mon <- tmp$mon + 1L
    tmp$mday <- 1L
    ldate <- as.Date(tmp) - 1L
    lweekday <- as.POSIXlt(ldate)$wday
    ldate - (lweekday - weekday)%%7L + (shift*7L)
}

nthWeekday <- function(weekday, x, n = 1L) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mday <- 1L
    tmp <- as.POSIXlt(as.Date(tmp)) ## 'wday' is not recomputed
    weekday1 <- tmp$wday            ## when 'mday' is changed
    as.Date(tmp) + (weekday - weekday1) %% 7L + 7L*(n - 1L)
}
                                        # TIMES
makeHHMMSS <- function(x, label = "time specification (HHMMSS)") {
    x <- as.character(x)
    if (nchar(x) == 1L)
        x <- paste("0", x, "0000", sep = "")
    if (nchar(x) == 2L)
        x <- paste(x, "0000", sep = "")
    if (nchar(x) == 4L)
        x <- paste(x, "00", sep = "")

    ss <- substr(x, 1, 2)
    if (ss > "24" || ss < "00")
        stop("check ", label)
    ss <- substr(x, 3, 4)
    if (ss > "60" || ss < "00")
        stop("check ", label)
    ss <- substr(x, 5, 6)
    if (ss > "60" || ss < "00")
        stop("check ", label)
    x
}

timegrid <- function(from, to, interval,
                     excludeWeekends = TRUE,
                     holidays   = NULL,
                     fromHHMMSS = "080000",
                     toHHMMSS   = "220000") {

    fromHHMMSS <- makeHHMMSS(fromHHMMSS)
    toHHMMSS   <- makeHHMMSS(toHHMMSS)
    
    if (!inherits(from, "POSIXt") || !inherits(to, "POSIXt"))
        stop(sQuote("from"), " and ", sQuote("to"), " must inherit from POSIXt")
    grd <- seq(from, to, by = interval)
    if (!is.null(holidays)) {
        if (!inherits(holidays, "Date")) {
            holidays <- as.Date(holidays)
        }
        grd <- grd[!(as.Date(grd) %in% holidays)]
    }
    lt <- as.POSIXlt(grd)
    tmp <- lt$hour*10000 + lt$min*100 + lt$sec
    grd <- grd[lt$wday > 0L & lt$wday < 6L &
               as.numeric(fromHHMMSS) <= tmp & as.numeric(toHHMMSS) >= tmp] 
    as.POSIXct(grd)
}

roundPOSIXt <- function(t, interval, up = FALSE) {

    if (!inherits(t, "POSIXct"))
        t <- as.POSIXct(t)
    tz <- attr(t, "tzone")
    n <- as.integer(strsplit(interval, " ", fixed = TRUE)[[c(1L,1L)]])

    if (grepl("sec", interval, ignore.case = TRUE)) {
        ## Default unit in 'POSIXct' is 'sec': do nothing.
    } else if (grepl("min", interval, ignore.case = TRUE)) {
        n <- 60L * n
    } else if (grepl("hour", interval, ignore.case = TRUE)) {
        n <- 60L * 60L * n
    } else if (grepl("day", interval, ignore.case = TRUE)) {
        n <- 24L * 60L * 60L * n
    } else {
        stop(sQuote("interval"), " not clear")
    }
    if (up)
        .POSIXct(n*ceiling(unclass(t)/n), tz = tz)
    else
        .POSIXct(n*floor(unclass(t)/n), tz = tz)
}

ssm <- function(time, tz = "") {
    if (tz == "UTC" || tz == "GMT")
        as.numeric(time) %% 86400
    else {
        pt <- as.POSIXlt(time, tz)        
        3600 * pt$hour + 60 * pt$min + pt$sec
    }
}

convertDate <- function(x, type, fraction = FALSE, tz = "") {
    type <- tolower(type)
    if (type == "excel" && !fraction){
        as.Date(x, origin = "1899-12-30")
    } else if (type == "excel") {
        tmp <- as.POSIXct(x * 86400, origin = "1899-12-30", tz = "UTC")
        as.POSIXct(strptime(format(tmp), format = "%Y-%m-%d %H:%M:%S", tz = tz))        
    } else if (type == "matlab" && !fraction) {
        as.Date(x, origin = "1970-01-01") - 719529
    } else if (type == "matlab" && fraction) {
        tmp <- as.POSIXct((x - 719529) * 86400, origin = "1970-01-01")
        as.POSIXct(strptime(format(tmp), format = "%Y-%m-%d %H:%M:%S", tz = tz))        
    } else
        stop("unknown type")
}


## timestamps <- seq(as.Date("2015-1-1"), as.Date("2015-12-15"), by = "1 day")
## ultimo, firstofmonth, 
reftimestamp <- function(what, when = Sys.Date(), timestamps, index = FALSE) {
    what <- tolower(what)
    if (!is.null(when) && what == "mtd") {
        ii <- suppressWarnings(max(which(as.Date(timestamps) <=
                                         endOfPreviousMonth(as.Date(when)))))
        ii[ii == -Inf] <- 0
        
    } else if (!is.null(when) && what == "ytd") {
        ii <- suppressWarnings(max(which(as.Date(timestamps) <=
                                         endOfPreviousYear(as.Date(when)))))
        ii[ii == -Inf] <- 0
    }
    if (index)
        timestamps[ii] else ii
}

rfc822t <- function(x, include.dow = TRUE) {

    days <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    mons <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

    xx <- as.POSIXlt(x)
    if (include.dow)
        paste0(days[xx$wday+1], ", ", xx$mday, " ", mons[xx$mon + 1], " ",
               format(xx, "%Y %H:%M:%S %z"))
    else
        paste0(xx$mday, " ", mons[xx$mon + 1], " ",
               format(xx, "%Y %H:%M:%S %z"))
}

convertTZ <- function(datetime, from = "", to = "") {
    if (inherits(datetime, "POSIXt"))
        datetime <- format(datetime, tz = from,
                           format = "%Y-%m-%d %H:%M:%S")
    ans <- as.POSIXct(datetime, tz = from)
    attr(ans, "tzone") <- to
    ans
}


