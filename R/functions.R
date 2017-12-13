## -*- truncate-lines: t; -*-

                                        # DATES

prev_bday  <- previous_businessday <- function (x, holidays = NULL, shift = -1) {
    if (!is.null(holidays))
        .NotYetUsed("holidays", FALSE)
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

next_bday  <- next_businessday <- function(x, holidays = NULL, shift = 1) {
    if (!is.null(holidays))
        .NotYetUsed("holidays", FALSE)
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

is_weekend <- function(x) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$wday == 0L | tmp$wday == 6L
}

is_leapyear <- function(x)
    x %% 4 == 0 & (x %% 100 != 0 | x %% 400 == 0)

is_businessday <- function(x, holidays = NULL) {
    if (!is.null(holidays))
        .NotYetUsed("holidays", FALSE)
    !is_weekend(x)
}

first_of_month <- function (x) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mday <- 1L
    as.Date(tmp)
}

first_of_year <- function (x) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mday <- 1L
    tmp$mon <- 0L
    as.Date(tmp)
}

end_of_month <- function(x, shift = 0L) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mon <- tmp$mon + 1L + shift
    tmp$mday <- 1L
    as.Date(tmp) - 1L
}

end_of_year <- function(x, shift = 0L) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$year <- tmp$year + shift
    tmp$mon <- 12
    tmp$mday <- 1L
    as.Date(tmp) - 1L
}

end_of_previous_month <- function(x)
    end_of_month(x, -1L)

end_of_previous_year <- function(x)
    end_of_year(x, shift = -1L)

mday <- function(x)
    day_of_month(x)

`mday<-` <- function(x, value){
    day_of_month(x) <- value
    x
}

day_of_month <- function(x) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    as.POSIXlt(x)$mday
}

`day_of_month<-` <- function(x, value) {
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

last_weekday <- function(weekday, x, shift = 0L,
                        period = "month", before, inclusive = TRUE) {
    if (missing(before) && !all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    if (missing(before)) {
        tmp <- as.POSIXlt(x)
        tmp$mon <- tmp$mon + 1L
        tmp$mday <- 1L
        ldate <- as.Date(tmp) - 1L
    } else
        ldate <- if (inclusive) before else before - 1L
    lweekday <- as.POSIXlt(ldate)$wday
    ldate - (lweekday - weekday)%%7L + (shift*7L)
}

nth_weekday <- function(weekday, x, n = 1L) {
    if (!all(inherits(x,"Date") | inherits(x,"POSIXt")))
        stop("input must inherit from class Date or POSIXt")
    tmp <- as.POSIXlt(x)
    tmp$mday <- 1L
    tmp <- as.POSIXlt(as.Date(tmp)) ## 'wday' is not recomputed
    weekday1 <- tmp$wday            ## when 'mday' is changed
    as.Date(tmp) + (weekday - weekday1) %% 7L + 7L*(n - 1L)
}
                                        # TIMES
make_hhmmss <- function(x, label = "time specification (HHMMSS)") {
    x <- as.character(x)
    if (nchar(x) == 1L)
        x <- paste("0", x, "0000", sep = "")
    else if (nchar(x) == 2L)
        x <- paste(x, "0000", sep = "")
    else if (nchar(x) == 4L)
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

    if (missing(interval))
        stop(sQuote("interval"), " missing")
    if (!inherits(from, "POSIXt") || !inherits(to, "POSIXt"))
        stop(sQuote("from"), " and ", sQuote("to"),
             " must inherit from POSIXt")
    fromHHMMSS <- make_hhmmss(fromHHMMSS)
    toHHMMSS   <- make_hhmmss(toHHMMSS)
    
    grd <- seq(from, to, by = interval)
    if (!is.null(holidays)) {
        if (!inherits(holidays, "Date"))
            holidays <- as.Date(holidays)
        grd <- grd[!(as.Date(grd) %in% holidays)]
    }
    tz <- attr(from, "tzone")
    lt <- as.POSIXlt(grd, tz = if (is.null(tz)) "" else tz)
    tmp <- lt$hour*10000 + lt$min*100 + lt$sec
    if (excludeWeekends)
        grd <- grd[lt$wday > 0L & lt$wday < 6L &
                   as.numeric(fromHHMMSS) <= tmp &
                   as.numeric(toHHMMSS) >= tmp] 
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

convert_date <- function(x, type, fraction = FALSE, tz = "") {
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

rfc822t <- function(x, include.dow = TRUE) {

    days <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    xx <- as.POSIXlt(x)
    if (include.dow)
        paste0(days[xx$wday+1], ", ", xx$mday, " ", month.abb[xx$mon + 1], " ",
               format(xx, "%Y %H:%M:%S %z"))
    else
        paste0(xx$mday, " ", month.abb[xx$mon + 1], " ",
               format(xx, "%Y %H:%M:%S %z"))
}

convert_tz <- function(datetime, from = "", to = "") {
    if (inherits(datetime, "POSIXt"))
        datetime <- format(datetime, tz = from,
                           format = "%Y-%m-%d %H:%M:%S")
    ans <- as.POSIXct(datetime, tz = from)
    attr(ans, "tzone") <- to
    ans
}

## timestamps <- seq(as.Date("2015-1-1"), as.Date("2015-12-15"), by = "1 day")
## ultimo, firstofmonth, 
ref_timestamp <- function(what, when = Sys.Date(), timestamps, index = FALSE) {
    what <- tolower(what)
    if (!is.null(when) && what == "mtd") {
        ii <- suppressWarnings(max(which(as.Date(timestamps) <=
                                         end_of_previous_month(as.Date(when)))))
        ii[ii == -Inf] <- 0
        
    } else if (!is.null(when) && what == "ytd") {
        ii <- suppressWarnings(max(which(as.Date(timestamps) <=
                                         end_of_previous_year(as.Date(when)))))
        ii[ii == -Inf] <- 0
    }
    if (index)
        timestamps[ii] else ii
}

nth_day <- function(timestamps,
                    period = "month", n,
                    start, end, 
                    business.days = FALSE,
                    missing = "previous",
                    index = FALSE) {

    if (missing(timestamps)) {
        if (index)
            stop("timestamps must be supplied")
        timestamps <- seq(from = start,
                          to = end,
                          by = "1 day")

    } else if (is.unsorted(timestamps))
        stop("timestamps must be sorted")
        
    period <- tolower(period)
    if (period == "month") {
        by <- format(timestamps, "%Y-%m")
    } else if (period == "quarter") {
        by <- as.POSIXlt(timestamps)$mon %/% 3L + 1L
    } else
        stop("unknown period")
    if (n == "last") {
        lby <- length(by)
        rby <- by[lby:1]
        ii <- lby - match(unique(by), rby) + 1L
    } else if (n == "first") {
        ii <- match(unique(by), by)
    } else {
        stop("only keywords first and last are implemented")
    }
        
    if (index) {
        ii
    } else
        timestamps[ii]
}

.dt_patterns <- c(
    "[1-2][0-9][0-9][0-9]-[0-9]+-[0-9]+ +[0-9]+:[0-9]+:[0-9]+",           "%Y-%m-%d %H:%M:%S",
    "[0-9][0-9]+-[0-9]+-[0-9]+ +[0-9]+:[0-9]+",                           "%Y-%m-%d %H:%M",
    "[0-9]+/[0-9]+/[0-9][0-9] +[0-9]+:[0-9]+:[0-9]+",                     "%m/%d/%y %H:%M:%S",
    "[0-9]+/[0-9]+/[1-2][0-9][0-9][0-9] +[0-9]+:[0-9]+:[0-9]+",           "%m/%d/%Y %H:%M:%S",
    "[0-9]+/[0-9]+/[0-9][0-9] +[0-9]+:[0-9]+",                            "%m/%d/%y %H:%M",
    "[0-9]+/[0-9]+/[1-2][0-9][0-9][0-9] +[0-9]+:[0-9]+",                  "%m/%d/%Y %H:%M",
    "[0-9][0-9]*[.][0-9]+[.][1-2][0-9][0-9][0-9]* +[0-9]+:[0-9]+:[0-9]+", "%d.%m.%Y %H:%M:%S",
    "[0-9][0-9]*[.][0-9]+[.][1-2][0-9][0-9][0-9]* +[0-9]+:[0-9]+",        "%d.%m.%Y %H:%M"
)

guess_datetime <- function(s) {

    x <- as.character(s)
    ans <- .POSIXct(rep(NA_real_, length(x)))
    done <- logical(length(x))

    ii <- seq(1, length(.dt_patterns), by = 2L)

    for (t in ii) {

        i <- grepl(.dt_patterns[t], x) & !done
        ans[i] <- as.POSIXct(strptime(x[i], .dt_patterns[t + 1L]))
        done[i] <- TRUE

        if (all(done))
            break
    }

    ans
}
