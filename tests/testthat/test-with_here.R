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

test_that("working directory doesn't change", {

    # go to a tempdir
    local_dir(local_tempdir())

    here_was <- here()
    wd_was <- getwd()


    d <- local_tempdir()
    now <- with_here(d, here())

    expect_pathequal(d, now)

    expect_pathequal(here_was, here())
    expect_pathequal(getwd(), wd_was)

})
