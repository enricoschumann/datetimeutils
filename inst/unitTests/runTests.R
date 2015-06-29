localTesting <- TRUE
if (require("RUnit", quietly = TRUE)) {
    require("datetimeutils")
    if (localTesting)
        path <- "~/Packages/datetimeutils/inst/unitTests" else
    path <- system.file("unitTests", package = "datetimeutils")
    myTestSuite <- defineTestSuite("datetimeutils",
                                   dirs = path,
                                   testFileRegexp = "ut_.*")
    stopifnot(isValidTestSuite(myTestSuite))
    testResult <- runTestSuite(myTestSuite, verbose = 0L)
    printTextProtocol(testResult, showDetails = TRUE,
                      fileName = paste(file.path(path, "report"),
                      ".txt", sep = ""))
}
