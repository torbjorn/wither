library(withr)
suppressMessages(library(here))
library(fs)

test_that("with_here works", {

    # setup a working directory
    d <- local_tempdir()
    dir_create(d)
    stopifnot(dir_exists(d))

    expect_equal(with_here(d, here()), d)
    expect_equal(with_here(d,here("foo")), file.path(d,"foo"))

    # check that getwd is unaffected
    expect_equal(with_here(d, getwd()), getwd())
    expect_equal(with_here(d, getwd(), chdir=TRUE), d)

})
