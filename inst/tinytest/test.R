## --------- last_weekday ---------

x <- as.Date(c("1996-05-30","1996-05-20"))

expect_equal(last_weekday(6, x, 0),
             structure(c(9641, 9641), class = "Date"))

expect_equal(last_weekday(6, x, -1),
             structure(c(9641, 9641), class = "Date") - 7)

expect_equal(last_weekday(1:5, as.Date("2017-1-1")),
             last_weekday(1:5, before=as.Date("2017-1-31")))
expect_equal(last_weekday(1:5, as.Date("2017-2-28")),
             last_weekday(1:5, before=as.Date("2017-2-28")))

tmp <- seq(from = as.Date("2017-01-01"),
           to   = as.Date("2018-12-31"),
           by   = "1 day")

for (i in 1:5) {
    expect_equal(last_weekday(i, before = tmp[as.POSIXlt(tmp)$wday == i]),
                 tmp[as.POSIXlt(tmp)$wday == i]-0)
    expect_equal(last_weekday(i, before = tmp[as.POSIXlt(tmp)$wday == i],
                              inclusive = FALSE),
                 tmp[as.POSIXlt(tmp)$wday == i]-7)
}



## ---------------------

expect_equal(convert_date(41824, "excel"), as.Date("2014-07-04"))
expect_equal(convert_date(61, "excel"), as.Date("1900-03-01"))

## Excel is timezone agnostic, so convert to current
tfun <- function(x)
    strftime(x, "%Y-%m-%d %H%M%S")
expect_equal(tfun(convert_date(41824.625, "excel", fraction = TRUE)),
             tfun(as.POSIXct("2014-07-04 15:00:00", tz = "")))
expect_equal(tfun(convert_date(610.125, "excel", fraction = TRUE)),
             tfun(as.POSIXct("1901-09-01 03:00:00", tz = "")))

expect_equal(convert_date(41824.625, "excel", fraction = TRUE, tz = "GMT"),
             structure(1404486000, class = c("POSIXct", "POSIXt"), tzone = "GMT"))
expect_equal(convert_date(610.125, "excel", fraction = TRUE, tz = "GMT"),
             structure(-2156446800, class = c("POSIXct", "POSIXt"), tzone = "GMT"))


## Excel 1904 testcase

## https://stat.ethz.ch/pipermail/r-help/2021-July/471640.html
## 7/20/21 13:30
## 7/20/21 13:40
## 42935.5625
## 42935.56944

## times <- c(42935.5625,42935.5694444444)
## as.POSIXct((times*86400),origin="1904-01-01",tz="America/Chicago")
## [1] "2021-07-20 08:30:00 CDT" "2021-07-20 08:39:59 CDT"

expect_equal(as.character(convert_date(c(42935.5625,42935.5694444444),
                                       "excel1904")),
             c("2021-07-20", "2021-07-20"))
## convert_date(c(42935.5625,42935.5694444444), "excel1904", fraction = TRUE)
## convert_date(c(42935.5625,42935.5694444444), "excel1904", fraction = TRUE,
##              tz = "America/Chicago")
x <- convert_date(c(42935.5625,42935.5694444444), "excel1904", fraction = TRUE,
                  tz = "GMT")
expect_equal(x,
             structure(c(1626787800, 1626788399),
                       class = c("POSIXct", "POSIXt"), tzone = "GMT"))
## convert_tz(x, "GMT", "America/Chicago")

## ---------------------

tmp <- structure(1435589310.11177,
                 class = c("POSIXct", "POSIXt"),
                 tzone = "GMT")

expect_equal(roundPOSIXt(tmp, "1 hour"),
             structure(1435586400,
                       tzone = "GMT",
                       class = c("POSIXct", "POSIXt")))

expect_equal(roundPOSIXt(tmp, "1 hour", TRUE),
             structure(1435590000,
                       tzone = "GMT",
                       class = c("POSIXct", "POSIXt")))

## ---------------------


dates <- seq(as.Date("2012-1-1"), as.Date("2015-1-10"), by = "1 day")
should <- dates + 1
should[as.POSIXlt(dates)$wday == 5L] <- dates[as.POSIXlt(dates)$wday == 5L] + 3
should[as.POSIXlt(dates)$wday == 6L] <- dates[as.POSIXlt(dates)$wday == 6L] + 2
should[as.POSIXlt(dates)$wday == 0L] <- dates[as.POSIXlt(dates)$wday == 0L] + 1
expect_true(all(should == next_businessday(dates)))

should <- dates + 0
should[as.POSIXlt(dates)$wday == 6L] <- dates[as.POSIXlt(dates)$wday == 6L] + 2
should[as.POSIXlt(dates)$wday == 0L] <- dates[as.POSIXlt(dates)$wday == 0L] + 1
expect_true(all(should == next_businessday(dates, shift = 0)))

expect_true(all(
    next_businessday(next_businessday(dates)) ==
    next_businessday(dates, shift = 2)))
expect_true(all(
    next_bday(next_bday(dates)) == next_bday(dates, shift = 2)))

expect_true(all(
    next_businessday(next_businessday(next_businessday(dates))) ==
    next_businessday(dates, shift = 3)))
expect_true(all(
    next_bday(next_bday(next_bday(dates))) == next_bday(dates, shift = 3)))


## ---------------------


dates <- seq(as.Date("2012-1-1"), as.Date("2015-1-10"), by = "1 day")
should <- dates - 1
should[as.POSIXlt(dates)$wday == 1L] <- dates[as.POSIXlt(dates)$wday == 1L] - 3
should[as.POSIXlt(dates)$wday == 6L] <- dates[as.POSIXlt(dates)$wday == 6L] - 1
should[as.POSIXlt(dates)$wday == 0L] <- dates[as.POSIXlt(dates)$wday == 0L] - 2
expect_true(all(should == previous_businessday(dates)))

should <- dates + 0
should[as.POSIXlt(dates)$wday == 6L] <- dates[as.POSIXlt(dates)$wday == 6L] - 1
should[as.POSIXlt(dates)$wday == 0L] <- dates[as.POSIXlt(dates)$wday == 0L] - 2
expect_true(all(should == previous_businessday(dates, shift = 0)))

expect_true(all(
    previous_businessday(previous_businessday(dates)) ==
    previous_businessday(dates, shift = -2)))
expect_true(all(
    prev_bday(prev_bday(dates)) == prev_bday(dates, shift = -2)))

expect_true(all(
    previous_businessday(previous_businessday(previous_businessday(dates))) ==
    previous_businessday(dates, shift = -3)))
expect_true(all(
    prev_bday(prev_bday(prev_bday(dates))) == prev_bday(dates, shift = -3)))

## ---------------------

dates <- seq(as.Date("2022-07-28"), as.Date("2022-08-5"), by = "1 day")
data.frame(date = dates,
           weekday = weekdays(dates),
           next.bd = next_businessday(dates, holidays = "2022-08-01"))
##         date   weekday    next.bd
## 1 2022-07-28  Thursday 2022-07-29
## 2 2022-07-29    Friday 2022-08-02
## 3 2022-07-30  Saturday 2022-08-02
## 4 2022-07-31    Sunday 2022-08-02
## 5 2022-08-01    Monday 2022-08-02
## 6 2022-08-02   Tuesday 2022-08-03
## 7 2022-08-03 Wednesday 2022-08-04
## 8 2022-08-04  Thursday 2022-08-05
## 9 2022-08-05    Friday 2022-08-08
expect_equal(next_businessday(dates, holidays = "2022-08-01"),
             structure(c(19202, 19206, 19206, 19206, 19206, 19207, 19208,
                         19209, 19212), class = "Date"))

## ---------------------

data.frame(date = dates,
           weekday = weekdays(dates),
           next.bd = next_businessday(dates,
                                      holidays = as.Date("2022-08-01") + 0:1))
##         date   weekday    next.bd
## 1 2022-07-28  Thursday 2022-07-29
## 2 2022-07-29    Friday 2022-08-03
## 3 2022-07-30  Saturday 2022-08-03
## 4 2022-07-31    Sunday 2022-08-03
## 5 2022-08-01    Monday 2022-08-03
## 6 2022-08-02   Tuesday 2022-08-03
## 7 2022-08-03 Wednesday 2022-08-04
## 8 2022-08-04  Thursday 2022-08-05
## 9 2022-08-05    Friday 2022-08-08
expect_equal(next_businessday(dates, holidays = as.Date("2022-08-01") + 0:1),
             structure(c(19202, 19207, 19207, 19207, 19207, 19207, 19208,
                         19209, 19212), class = "Date"))

## ---------------------

dates <- seq(as.Date("2022-07-28"), as.Date("2022-08-5"), by = "1 day")
data.frame(date = dates,
           weekday = weekdays(dates),
           prev.bd = previous_businessday(dates,
                                          holidays = as.Date("2022-08-01") + 0:1))
##         date   weekday    prev.bd
## 1 2022-07-28  Thursday 2022-07-27
## 2 2022-07-29    Friday 2022-07-28
## 3 2022-07-30  Saturday 2022-07-29
## 4 2022-07-31    Sunday 2022-07-29
## 5 2022-08-01    Monday 2022-07-29
## 6 2022-08-02   Tuesday 2022-07-29
## 7 2022-08-03 Wednesday 2022-08-02
## 8 2022-08-04  Thursday 2022-08-03
## 9 2022-08-05    Friday 2022-08-04
expect_equal(previous_businessday(dates, holidays = as.Date("2022-08-01") + 0:1),
             structure(c(19200, 19201, 19202, 19202, 19202, 19202, 19202,
                         19207, 19208), class = "Date"))

## ---------------------

dates <- seq(as.Date("2022-07-28"), as.Date("2022-08-5"), by = "1 day")
data.frame(date = dates,
           weekday = weekdays(dates),
           prev.bd = previous_businessday(dates, holidays = "2022-08-01"))
##         date   weekday    prev.bd
## 1 2022-07-28  Thursday 2022-07-27
## 2 2022-07-29    Friday 2022-07-28
## 3 2022-07-30  Saturday 2022-07-29
## 4 2022-07-31    Sunday 2022-07-29
## 5 2022-08-01    Monday 2022-07-29
## 6 2022-08-02   Tuesday 2022-07-29
## 7 2022-08-03 Wednesday 2022-08-02
## 8 2022-08-04  Thursday 2022-08-03
## 9 2022-08-05    Friday 2022-08-04
expect_equal(previous_businessday(dates, holidays = "2022-08-01"),
             structure(c(19200, 19201, 19202, 19202, 19202, 19202, 19206,
                         19207, 19208), class = "Date"))



## ---------------------


expect_true(all(nth_weekday(1, as.Date("2016-1-1")+0:30, 1) ==
                as.Date("2016-1-4")))
expect_true(all(nth_weekday(2, as.Date("2016-1-1")+0:30, 1) ==
                as.Date("2016-1-5")))
expect_true(all(nth_weekday(3, as.Date("2016-1-1")+0:30, 1) ==
                as.Date("2016-1-6")))
expect_true(all(nth_weekday(4, as.Date("2016-1-1")+0:30, 1) ==
                as.Date("2016-1-7")))
expect_true(all(nth_weekday(5, as.Date("2016-1-1")+0:30, 1) ==
                as.Date("2016-1-1")))
expect_true(all(nth_weekday(6, as.Date("2016-1-1")+0:30, 1) ==
                as.Date("2016-1-2")))
expect_true(all(nth_weekday(7, as.Date("2016-1-1")+0:30, 1) ==
                as.Date("2016-1-3")))

## 2016 is a leap year
expect_true(all(nth_weekday(1, as.Date("2016-2-1")+0:28, 1) ==
                as.Date("2016-2-1")))
expect_true(all(nth_weekday(2, as.Date("2016-2-1")+0:28, 1) ==
                as.Date("2016-2-2")))
expect_true(all(nth_weekday(3, as.Date("2016-2-1")+0:28, 1) ==
                as.Date("2016-2-3")))
expect_true(all(nth_weekday(4, as.Date("2016-2-1")+0:28, 1) ==
                as.Date("2016-2-4")))
expect_true(all(nth_weekday(5, as.Date("2016-2-1")+0:28, 1) ==
                as.Date("2016-2-5")))
expect_true(all(nth_weekday(6, as.Date("2016-2-1")+0:28, 1) ==
                as.Date("2016-2-6")))
expect_true(all(nth_weekday(7, as.Date("2016-2-1")+0:28, 1) ==
                as.Date("2016-2-7")))

expect_true(all(nth_weekday(1, as.Date("2016-2-1")+0:28, 2) ==
                as.Date("2016-2-8")))
expect_true(all(nth_weekday(1, as.Date("2016-2-1")+0:28, 3) ==
                as.Date("2016-2-15")))
expect_true(all(nth_weekday(1, as.Date("2016-2-1")+0:28, 4) ==
                as.Date("2016-2-22")))
expect_true(all(nth_weekday(1, as.Date("2016-2-1")+0:28, 5) ==
                as.Date("2016-2-29")))

## 6th Monday of February == 1st Monday of March
expect_true(all(nth_weekday(1, as.Date("2016-2-1")+0:28, 6) ==
                as.Date("2016-3-7")))


## ---------------------


x <- timegrid(as.POSIXlt("2017-06-23 21:00:00", tz = "UTC"),
              as.POSIXlt("2017-06-26 10:00:00", tz = "UTC"),
              interval = "15 min")
expect_equal(x,
             structure(c(1498251600, 1498252500, 1498253400,
                         1498254300, 1498255200, 1498464000,
                         1498464900, 1498465800, 1498466700,
                         1498467600, 1498468500, 1498469400,
                         1498470300, 1498471200),
                       class = c("POSIXct", "POSIXt"), tzone = "UTC"))

x <- timegrid(as.POSIXlt("2017-06-23 21:00:00", tz = "Europe/Zurich"),
              as.POSIXlt("2017-06-26 10:00:00", tz = "Europe/Zurich"),
              interval = "15 min")
expect_equal(x,
             structure(c(1498244400, 1498245300, 1498246200,
                         1498247100, 1498248000, 1498456800,
                         1498457700, 1498458600, 1498459500,
                         1498460400, 1498461300, 1498462200,
                         1498463100, 1498464000),
                       class = c("POSIXct", "POSIXt"),
                       tzone = "Europe/Zurich"))


## ---------------------

s <- c("  1999-08-19     10:00:00   ",
       "1999-08-19 10:00",
       "19.8.1999 10:00",
       "8/19/99  10:00:00",
       "8/19/1999 10:00:00",
       "19.8.1999 10:00:00",
       " 19.08.99 10:00:00",
       "8/19/1999    10:00:00   ",
       "19.8.1999    10:00:00   "
       )

expect_true(all(as.character(guess_datetime(s)) ==
                "1999-08-19 10:00:00"))

expect_equal(guess_datetime("date 12.02.2021.",
                            date.only = TRUE, within = TRUE),
             as.Date("2021-02-12"))

### with additional patterns

s <- c("19-08-99")

expect_true(is.na(guess_datetime(s, date.only = TRUE)))

expect_equal(guess_datetime(
    s,
    try.patterns = c("[0-9]+-[0-9]+-[0-9]+", "%d-%m-%y"),
    date.only = TRUE),
    as.Date(s, "%d-%m-%y"))


expect_equal(guess_datetime(
    "88889911/20211010",
    date.only = TRUE,
    within = TRUE),
    as.Date("2021-10-10"))



## ---------------------

expect_true(all(
    end_of_year(as.Date(
        c("2017-01-01",
          "2017-12-31",
          "2017-08-01"))) == as.Date("2017-12-31")))

expect_true(all(
    end_of_year(as.Date(
        c("2017-01-01",
          "2017-12-31",
          "2017-08-01")), shift = 0) == as.Date("2017-12-31")))

expect_true(all(
    end_of_year(as.Date(
        c("2017-01-01",
          "2017-12-31",
          "2017-08-01")), shift = 1) == as.Date("2018-12-31")))

expect_true(all(
    end_of_year(as.Date(
        c("2017-01-01",
          "2017-12-31",
          "2017-08-01")), shift = -1) == as.Date("2016-12-31")))

expect_true(all(
    end_of_previous_year(as.Date(
        c("2017-01-01",
          "2017-12-31",
          "2017-08-01"))) == as.Date("2016-12-31")))

## ---------------------

expect_true(all(
    end_of_month(as.Date(
        c("2017-01-01",
          "2017-12-31",
          "2017-08-01"))) ==
    as.Date(
        c("2017-01-31",
          "2017-12-31",
          "2017-08-31"))))

expect_true(all(
    end_of_month(as.Date(
        c("2017-01-01",
          "2017-12-31",
          "2017-08-01")), shift = 0) ==
    as.Date(
        c("2017-01-31",
          "2017-12-31",
          "2017-08-31"))))

expect_true(all(
    end_of_month(as.Date(
        c("2017-01-01",
          "2017-12-31",
          "2017-08-01")), shift = 1) ==
    as.Date(
        c("2017-02-28",
          "2018-01-31",
          "2017-09-30"))))

expect_true(all(
    end_of_month(as.Date(
        c("2017-01-01",
          "2017-12-31",
          "2017-08-01")), shift = -1) ==
    as.Date(
        c("2016-12-31",
          "2017-11-30",
          "2017-07-31"))))

expect_true(all(
    end_of_previous_month(as.Date(
        c("2017-01-01",
          "2017-12-31",
          "2017-08-01"))) ==
    as.Date(
        c("2016-12-31",
          "2017-11-30",
          "2017-07-31"))))

## ---------------------


sq <- seq(as.Date("2000-1-1"), as.Date("2001-11-30"), by = "day")

expect_equal(nth_day(sq, "quarter", n = "first"),
             structure(c(10957, 11048, 11139, 11231,
                         11323, 11413, 11504, 11596),
                       class = "Date"))

expect_equal(nth_day(sq, "quarter", n = "last"),
             structure(c(11047, 11138, 11230, 11322,
                         11412, 11503, 11595, 11656),
                       class = "Date"))

expect_equal(nth_day(sq, "halfyear", n = "first"),
             structure(c(10957, 11139, 11323, 11504),
                       class = "Date"))

expect_equal(nth_day(sq, "halfyear", n = "last"),
             structure(c(11138, 11322, 11503, 11656),
                       class = "Date"))

expect_equal(nth_day(sq, period = c(2, 12), n = 5),
             structure(c(10992, 11296, 11358), class = "Date"))

expect_equal(nth_day(sq, period = 2, n = 5),
             structure(c(10992, 11358), class = "Date"))

expect_equal(nth_day(sq, period = "year", n = 5),
             structure(c(10961, 11327), class = "Date"))

expect_equal(nth_day(sq, period = "month", n = 6),
             structure(c(10962, 10993, 11022, 11053,
                         11083, 11114, 11144, 11175,
                         11206, 11236, 11267, 11297,
                         11328, 11359, 11387, 11418,
                         11448, 11479, 11509, 11540,
                         11571, 11601, 11632),
                       class = "Date"))

expect_equal(nth_day(sq, n = 1:2),
             structure(c(10957, 10958, 10988, 10989,
                         11017, 11018, 11048, 11049,
                         11078, 11079, 11109, 11110,
                         11139, 11140, 11170, 11171,
                         11201, 11202, 11231, 11232,
                         11262, 11263, 11292, 11293,
                         11323, 11324, 11354, 11355,
                         11382, 11383, 11413, 11414,
                         11443, 11444, 11474, 11475,
                         11504, 11505, 11535, 11536,
                         11566, 11567, 11596, 11597,
                         11627, 11628),
                       class = "Date"))



for (m in month.name)
    expect_equal(nth_day(sq, period = m, n = "first"),
                 nth_day(sq, period = which(m == month.name),
                         n = "first"))
for (m in month.name)
    expect_equal(nth_day(sq, period = m, n = 1),
                 nth_day(sq, period = which(m == month.name), n = 1))

for (m in month.name)
    expect_equal(nth_day(sq, period = m, n = 1:2),
                 nth_day(sq, period = which(m == month.name), n = 1:2))

expect_equal(nth_day(sq, period = c("April", "jan"), n = 2:3),
             structure(c(10958, 10959, 11049, 11050,
                         11324, 11325, 11414, 11415),
                       class = "Date"))
expect_equal(nth_day(sq, period = "Dec", n = "first"),
             as.Date("2000-12-01"))
expect_equal(nth_day(sq, period = "Nov", n = "first"),
             as.Date(c("2000-11-01", "2001-11-01")))

expect_equal(nth_day(n = 2, start = 2017, end = 2017),
             seq(as.Date("2017-1-2"), as.Date("2017-12-2"),
                 by = "1 month"))

expect_equal(nth_day(2017, n = 2),
             seq(as.Date("2017-1-2"), as.Date("2017-12-2"),
                 by = "1 month"))

expect_equal(nth_day(2017, period = "quarter", n = 2),
             seq(as.Date("2017-1-2"), as.Date("2017-12-2"),
                 by = "1 quarter"))


## ---------------------

expect_equal(year(as.Date("2018-1-1")+1:3),
             rep(2018, 3))
expect_equal(year(as.Date("2018-1-1")+1:3,
                  as.character = TRUE),
             rep("2018", 3))

## ---------------------


expect_equal(month(as.Date("2017-12-31")+0:1),
             c(12,1))
expect_equal(month(as.Date("2017-12-31")+0:1,
                   as.character = TRUE),
             c("12","1"))

## ---------------------

dates <- as.Date("1999-1-1") + 0:5000

dates2 <- end_of_month(as.Date(paste(
    year(dates),
    as.numeric(substr(quarters(dates, TRUE), 2, 2))*3,
    1), format = "%Y %m %d"))
expect_equal(end_of_quarter(dates), dates2)



## --------- last_weekday ---------



## --------- is_businessday ---------

expect_true(is_businessday(as.Date("2022-08-01")))
expect_false(is_businessday(as.Date("2022-08-01"),
                            holidays = "2022-08-01"))
