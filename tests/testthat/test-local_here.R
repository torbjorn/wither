library(withr)
suppressMessages(library(here))
library(fs)

source(test_path("helpers.R"))

test_that("local_here works", {

    # setup a working directory
    d <- local_tempdir()

    wd_was <- getwd()
    here_was <- here()

    local({

        returned_value <- local_here(d)

        expect_pathequal(returned_value, here_was)

        expect_pathequal(here(), d)
        expect_pathequal(getwd(), wd_was)

    })

    # check that getwd is unaffected
    expect_pathequal(wd_was, getwd())
    expect_pathequal(here(), here_was)

    # test with changing dir
    local({

        local_here(d, chdir=TRUE)

        expect_pathequal(here(), d)
        expect_pathequal(getwd(), d)

    })

    # check that getwd is unaffected
    expect_pathequal(wd_was, getwd())
    expect_pathequal(here(), here_was)

})

test_that("working directory doesn't change", {

    # go to a tempdir
    local_dir(local_tempdir())

    here_was <- here()
    wd_was <- getwd()

    local({

        d <- local_tempdir()
        local_here(d)

        # here is now d
        expect_pathequal(d, here())

    })

    expect_pathequal(here_was, here())
    expect_pathequal(getwd(), wd_was)

})
