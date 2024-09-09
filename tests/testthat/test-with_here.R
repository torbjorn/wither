library(withr)
suppressMessages(library(here))
library(fs)

source(test_path("helpers.R"))

test_that("with_here works", {

    # setup a working directory
    d <- local_tempdir()

    expect_pathequal(with_here(d, here()), d)

    # check that getwd is unaffected
    expect_pathequal(with_here(d, getwd()), getwd())
    expect_pathequal(with_here(d, getwd(), chdir = TRUE), d)

})
