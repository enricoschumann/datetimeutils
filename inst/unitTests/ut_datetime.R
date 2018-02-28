test.last_weekday <- function() {

    x <- as.Date(c("1996-05-30","1996-05-20"))

    checkEquals(last_weekday(6, x, 0), structure(c(9641, 9641), class
                = "Date"))

    checkEquals(last_weekday(6, x, -1),
                structure(c(9641, 9641), class = "Date") - 7)

    checkEquals(last_weekday(1:5, as.Date("2017-1-1")),
                last_weekday(1:5, before=as.Date("2017-1-31")))
    checkEquals(last_weekday(1:5, as.Date("2017-2-28")),
                last_weekday(1:5, before=as.Date("2017-2-28")))
                 
    tmp <- seq(from = as.Date("2017-01-01"),
               to   = as.Date("2018-12-31"),
               by   = "1 day")

    for (i in 1:5) {
        checkEquals(last_weekday(i, before = tmp[as.POSIXlt(tmp)$wday == i]),
                    tmp[as.POSIXlt(tmp)$wday == i]-0)
        checkEquals(last_weekday(i, before = tmp[as.POSIXlt(tmp)$wday == i],
                                 inclusive = FALSE),
                    tmp[as.POSIXlt(tmp)$wday == i]-7)
    }    
}

test.convert_date <- function() {
    checkEquals(convert_date(41824, "excel"), as.Date("2014-07-04"))
    checkEquals(convert_date(61, "excel"), as.Date("1900-03-01"))
}

test.roundPOSIXt <- function() {
    tmp <- structure(1435589310.11177,
                     class = c("POSIXct", "POSIXt"),
                     tzone = "GMT")

    checkEquals(roundPOSIXt(tmp, "1 hour"),
                structure(1435586400,
                          tzone = "GMT",
                          class = c("POSIXct", "POSIXt")))

    checkEquals(roundPOSIXt(tmp, "1 hour", TRUE),
                structure(1435590000,
                          tzone = "GMT",
                          class = c("POSIXct", "POSIXt")))    
}


test.next_businessday <- function() {
    dates <- seq(as.Date("2012-1-1"), as.Date("2015-1-10"), by = "1 day")
    should <- dates + 1
    should[as.POSIXlt(dates)$wday == 5L] <- dates[as.POSIXlt(dates)$wday == 5L] + 3
    should[as.POSIXlt(dates)$wday == 6L] <- dates[as.POSIXlt(dates)$wday == 6L] + 2
    should[as.POSIXlt(dates)$wday == 0L] <- dates[as.POSIXlt(dates)$wday == 0L] + 1
    checkTrue(all(should == next_businessday(dates)))

    should <- dates + 0
    should[as.POSIXlt(dates)$wday == 6L] <- dates[as.POSIXlt(dates)$wday == 6L] + 2
    should[as.POSIXlt(dates)$wday == 0L] <- dates[as.POSIXlt(dates)$wday == 0L] + 1
    checkTrue(all(should == next_businessday(dates, shift = 0)))

    checkTrue(all(
        next_businessday(next_businessday(dates)) ==
            next_businessday(dates, shift = 2)))
    checkTrue(all(
        next_bday(next_bday(dates)) == next_bday(dates, shift = 2)))

    checkTrue(all(
        next_businessday(next_businessday(next_businessday(dates))) ==
        next_businessday(dates, shift = 3)))
    checkTrue(all(
        next_bday(next_bday(next_bday(dates))) == next_bday(dates, shift = 3)))
}

test.previous_businessday <- function() {
    dates <- seq(as.Date("2012-1-1"), as.Date("2015-1-10"), by = "1 day")
    should <- dates - 1
    should[as.POSIXlt(dates)$wday == 1L] <- dates[as.POSIXlt(dates)$wday == 1L] - 3
    should[as.POSIXlt(dates)$wday == 6L] <- dates[as.POSIXlt(dates)$wday == 6L] - 1
    should[as.POSIXlt(dates)$wday == 0L] <- dates[as.POSIXlt(dates)$wday == 0L] - 2
    checkTrue(all(should == previous_businessday(dates)))

    should <- dates + 0
    should[as.POSIXlt(dates)$wday == 6L] <- dates[as.POSIXlt(dates)$wday == 6L] - 1
    should[as.POSIXlt(dates)$wday == 0L] <- dates[as.POSIXlt(dates)$wday == 0L] - 2
    checkTrue(all(should == previous_businessday(dates, shift = 0)))

    checkTrue(all(
        previous_businessday(previous_businessday(dates)) ==
            previous_businessday(dates, shift = -2)))
    checkTrue(all(
        prev_bday(prev_bday(dates)) == prev_bday(dates, shift = -2)))

    checkTrue(all(
        previous_businessday(previous_businessday(previous_businessday(dates))) ==
            previous_businessday(dates, shift = -3)))    
    checkTrue(all(
        prev_bday(prev_bday(prev_bday(dates))) == prev_bday(dates, shift = -3)))    
}

test.nth_weekday <- function() {

    checkTrue(all(nth_weekday(1, as.Date("2016-1-1")+0:30, 1) ==
                  as.Date("2016-1-4")))
    checkTrue(all(nth_weekday(2, as.Date("2016-1-1")+0:30, 1) ==
                  as.Date("2016-1-5")))
    checkTrue(all(nth_weekday(3, as.Date("2016-1-1")+0:30, 1) ==
                  as.Date("2016-1-6")))
    checkTrue(all(nth_weekday(4, as.Date("2016-1-1")+0:30, 1) ==
                  as.Date("2016-1-7")))
    checkTrue(all(nth_weekday(5, as.Date("2016-1-1")+0:30, 1) ==
                  as.Date("2016-1-1")))
    checkTrue(all(nth_weekday(6, as.Date("2016-1-1")+0:30, 1) ==
                  as.Date("2016-1-2")))
    checkTrue(all(nth_weekday(7, as.Date("2016-1-1")+0:30, 1) ==
                  as.Date("2016-1-3")))

    ## 2016 is a leap year
    checkTrue(all(nth_weekday(1, as.Date("2016-2-1")+0:28, 1) ==
                  as.Date("2016-2-1")))
    checkTrue(all(nth_weekday(2, as.Date("2016-2-1")+0:28, 1) ==
                  as.Date("2016-2-2")))
    checkTrue(all(nth_weekday(3, as.Date("2016-2-1")+0:28, 1) ==
                  as.Date("2016-2-3")))
    checkTrue(all(nth_weekday(4, as.Date("2016-2-1")+0:28, 1) ==
                  as.Date("2016-2-4")))
    checkTrue(all(nth_weekday(5, as.Date("2016-2-1")+0:28, 1) ==
                  as.Date("2016-2-5")))
    checkTrue(all(nth_weekday(6, as.Date("2016-2-1")+0:28, 1) ==
                  as.Date("2016-2-6")))
    checkTrue(all(nth_weekday(7, as.Date("2016-2-1")+0:28, 1) ==
                  as.Date("2016-2-7")))

    checkTrue(all(nth_weekday(1, as.Date("2016-2-1")+0:28, 2) ==
                  as.Date("2016-2-8")))
    checkTrue(all(nth_weekday(1, as.Date("2016-2-1")+0:28, 3) ==
                  as.Date("2016-2-15")))
    checkTrue(all(nth_weekday(1, as.Date("2016-2-1")+0:28, 4) ==
                  as.Date("2016-2-22")))
    checkTrue(all(nth_weekday(1, as.Date("2016-2-1")+0:28, 5) ==
                  as.Date("2016-2-29")))

    ## 6th Monday of February == 1st Monday of March
    checkTrue(all(nth_weekday(1, as.Date("2016-2-1")+0:28, 6) ==
                  as.Date("2016-3-7")))    
}

test.timegrid <- function() {

    x <- timegrid(as.POSIXlt("2017-06-23 21:00:00", tz = "UTC"),
                  as.POSIXlt("2017-06-26 10:00:00", tz = "UTC"),
                  interval = "15 min")
    checkEquals(x,
                structure(c(1498251600, 1498252500, 1498253400,
                            1498254300, 1498255200, 1498464000,
                            1498464900, 1498465800, 1498466700,
                            1498467600, 1498468500, 1498469400,
                            1498470300, 1498471200),
                          class = c("POSIXct", "POSIXt"), tzone = "UTC"))

    x <- timegrid(as.POSIXlt("2017-06-23 21:00:00", tz = "Europe/Zurich"),
                  as.POSIXlt("2017-06-26 10:00:00", tz = "Europe/Zurich"),
                  interval = "15 min")
    checkEquals(x,
                structure(c(1498244400, 1498245300, 1498246200,
                            1498247100, 1498248000, 1498456800,
                            1498457700, 1498458600, 1498459500,
                            1498460400, 1498461300, 1498462200,
                            1498463100, 1498464000),
                          class = c("POSIXct", "POSIXt"),
                          tzone = "Europe/Zurich"))

}

test.guess_datetime <- function() {

    s <- c("  1999-08-19     10:00:00   ",
           "1999-08-19 10:00",
           "19.8.1999 10:00",
           "8/19/99  10:00:00",
           "8/19/1999 10:00:00",
           "19.8.1999 10:00:00",
           "8/19/1999    10:00:00   ",
           "19.8.1999    10:00:00   "
           )
    
    checkTrue(all(as.character(guess_datetime(s)) ==
                  "1999-08-19 10:00:00"))

}

test.end_of_year <- function() {
    checkTrue(all(
        end_of_year(as.Date(
            c("2017-01-01",
              "2017-12-31",
              "2017-08-01"))) == as.Date("2017-12-31")))

    checkTrue(all(
        end_of_year(as.Date(
            c("2017-01-01",
              "2017-12-31",
              "2017-08-01")), shift = 0) == as.Date("2017-12-31")))

    checkTrue(all(
        end_of_year(as.Date(
            c("2017-01-01",
              "2017-12-31",
              "2017-08-01")), shift = 1) == as.Date("2018-12-31")))

    checkTrue(all(
        end_of_year(as.Date(
            c("2017-01-01",
              "2017-12-31",
              "2017-08-01")), shift = -1) == as.Date("2016-12-31")))

    checkTrue(all(
        end_of_previous_year(as.Date(
            c("2017-01-01",
              "2017-12-31",
              "2017-08-01"))) == as.Date("2016-12-31")))
}

test.end_of_month <- function() {
    checkTrue(all(
        end_of_month(as.Date(
            c("2017-01-01",
              "2017-12-31",
              "2017-08-01"))) ==
        as.Date(
            c("2017-01-31",
              "2017-12-31",
              "2017-08-31"))))

    checkTrue(all(
        end_of_month(as.Date(
            c("2017-01-01",
              "2017-12-31",
              "2017-08-01")), shift = 0) ==
        as.Date(
            c("2017-01-31",
              "2017-12-31",
              "2017-08-31"))))
    
    checkTrue(all(
        end_of_month(as.Date(
            c("2017-01-01",
              "2017-12-31",
              "2017-08-01")), shift = 1) ==
        as.Date(
            c("2017-02-28",
              "2018-01-31",
              "2017-09-30"))))

    checkTrue(all(
        end_of_month(as.Date(
            c("2017-01-01",
              "2017-12-31",
              "2017-08-01")), shift = -1) ==
        as.Date(
            c("2016-12-31",
              "2017-11-30",
              "2017-07-31"))))

    checkTrue(all(
        end_of_previous_month(as.Date(
            c("2017-01-01",
              "2017-12-31",
              "2017-08-01"))) ==
        as.Date(
            c("2016-12-31",
              "2017-11-30",
              "2017-07-31"))))    
}


test.nth_day <- function() {

    sq <- seq(as.Date("2000-1-1"), as.Date("2001-11-30"), by = "day")

    checkEquals(nth_day(sq, "quarter", n = "first"),
                structure(c(10957, 11048, 11139, 11231,
                            11323, 11413, 11504, 11596),
                          class = "Date"))
    
    checkEquals(nth_day(sq, "quarter", n = "last"),
                structure(c(11047, 11138, 11230, 11322,
                            11412, 11503, 11595, 11656),
                          class = "Date"))

    checkEquals(nth_day(sq, "halfyear", n = "first"),
                structure(c(10957, 11139, 11323, 11504),
                          class = "Date"))

    checkEquals(nth_day(sq, "halfyear", n = "last"),
                structure(c(11138, 11322, 11503, 11656),
                          class = "Date"))

    checkEquals(nth_day(sq, period = c(2, 12), n = 5),
                structure(c(10992, 11296, 11358), class = "Date"))

    checkEquals(nth_day(sq, period = 2, n = 5),
                structure(c(10992, 11358), class = "Date"))

    checkEquals(nth_day(sq, period = "year", n = 5),
                structure(c(10961, 11327), class = "Date"))

    checkEquals(nth_day(sq, period = "month", n = 6),
                structure(c(10962, 10993, 11022, 11053,
                            11083, 11114, 11144, 11175,
                            11206, 11236, 11267, 11297,
                            11328, 11359, 11387, 11418,
                            11448, 11479, 11509, 11540,
                            11571, 11601, 11632),
                          class = "Date"))

    checkEquals(nth_day(sq, n = 1:2),
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

}

test.year <- function() {
    checkEquals(year(as.Date("2018-1-1")+1:3),
                rep(2018, 3))
    checkEquals(year(as.Date("2018-1-1")+1:3,
                     as.character = TRUE),
                rep("2018", 3))    
}

test.month <- function() {
    checkEquals(month(as.Date("2017-12-31")+0:1),
                c(12,1))
    checkEquals(month(as.Date("2017-12-31")+0:1,
                     as.character = TRUE),
                c("12","1"))
}
