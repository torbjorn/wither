library(withr)
suppressMessages(library(here))
library(fs)

test_that("local_here works", {

    # setup a working directory
    d <- local_tempdir()
    dir_create(d)
    stopifnot(dir_exists(d))

    wd_was <- getwd()
    here_was <- here()

    local({

        local_here(d)

        expect_equal(here(), d)
        expect_equal(here("foo"), file.path(d,"foo"))
        expect_equal(getwd(), wd_was)

    })

    # check that getwd is unaffected
    expect_equal(wd_was, getwd())
    expect_equal(here(), here_was)

    # test with changing dir

    local({

        local_here(d, chdir=TRUE)

        expect_equal(here(), d)
        expect_equal(here("foo"), file.path(d,"foo"))
        expect_equal(getwd(), d)

    })

})
