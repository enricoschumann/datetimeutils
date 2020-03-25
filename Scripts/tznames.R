win <- trimws(readLines("https://raw.githubusercontent.com/unicode-org/cldr/master/common/supplemental/windowsZones.xml"))
win <- win[grep("<mapZone", win)]

to <- gsub('<mapZone other.*territory.*type="([^"]+?)["].*', '\\1', win)
from <- gsub('<mapZone other="([^"]+)" .*', '\\1', win)

to <- strsplit(to, " ")
from <- rep(from, times = lengths(to))

tznames <- data.frame(Windows = trimws(unlist(from)),
                      Olson = trimws(unlist(to)),
                      stringsAsFactors = FALSE)

save(tznames,
     file = "~/Packages/datetimeutils/data/tznames.RData",
     version = 2)

library("orgutils")
filename <- "~/Packages/datetimeutils/data/tznames.txt"

cat("## -*- mode: org; -*-

## The data in this file are auto-generated from file 'windowsZones.xml'
## in the Unicode Common Locale Data Repository (http://cldr.unicode.org/).
## See https://www.unicode.org/copyright.html and https://www.unicode.org/license.html.

", file = filename, append = FALSE, sep = "")

## licence <- readLines("https://www.unicode.org/license.html")
## i <- grep("</?pre>", licence)
## licence <- licence[seq(i[1] + 1, i[2] - 1)]

## cat(licence, file = filename, append = TRUE, sep = "\n")

cat(toOrg(tznames), file = filename, append = TRUE, sep = "\n")
