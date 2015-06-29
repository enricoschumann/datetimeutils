test.Datetime <- function() {
    x <- as.Date(c("1996-05-30","1996-05-20"))

    checkEquals(lastWeekday(6, x, 0),
                structure(c(9641, 9641), class = "Date"))

    checkEquals(lastWeekday(6, x, -1),
                structure(c(9641, 9641), class = "Date") - 7)

    ## from <- ISOdatetime(2012,1,1,12,00,00)
    ## to <- from + 36000
    ## timegrid(from, to,
    ##          interval = "15 sec",
    ##          excludeWeekends = TRUE, holidays = NULL)
}

test.convertDate <- function() {
    checkEquals(convertDate(41824, "excel"), as.Date("2014-07-04"))
    checkEquals(convertDate(61, "excel"), as.Date("1900-03-1"))
    checkEquals(convertDate(61, "excel"), as.Date("1900-03-1"))

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
