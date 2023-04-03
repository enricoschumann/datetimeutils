if (requireNamespace("tinytest", quietly = TRUE)) {
    tinytest.results <- tinytest::test_package("datetimeutils",
                                               color = interactive(),
                                               verbose = 1)
}
