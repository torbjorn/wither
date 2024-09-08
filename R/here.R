##' Evaluate an expresion under a temporary here() root
##'
##' Changes [here::here()] to temporarily point to a new directory when
##' evaluating expression. Changes back afterwards.
##' @title Temporarily Change Project Root
##' @param new_here new temporary here root directory
##' @param expr expression to evaluate
##' @param chdir change working directory also
##' @param verbose show here's messages on setting new root
##' @return the results of the provided R expression
##' @author Torbjørn Lindahl
##' @importFrom withr local_tempfile local_dir defer
##' @importFrom here here i_am
##' @importFrom fs path_rel dir_exists
##' @importFrom utils capture.output
##' @export
##' @examples
##' library(here)
##'
##' d <- tempfile()
##' dir.create(d)
##'
##' with_here(d, cat("here() is now: ", here(), "\n"))
with_here <- function(new_here, expr, chdir=FALSE, verbose=FALSE ) {

    if(!dir_exists(new_here))
        stop("new_here must be a directory")

    current_here <- here()

    # create a temporary file to get us back when we're done
    tf_current <- local_tempfile(tmpdir=current_here, pattern=".here")
    .file_touch(tf_current)

    # make sure it goes back aftrwards (this will trigger after the
    # above local_dir defer has changed the working dir back
    defer(f(local({
        setwd(current_here)
        i_am(path_rel(tf_current, current_here))
    })))

    # create another remporary file to get us where we want to go
    tf_temp <- local_tempfile(tmpdir=new_here, pattern=".here")
    .file_touch(tf_temp)

    # opt to suppress i_am's "here() starts at"
    f <- function(x) {
        m <- capture.output(x, type="message")
        if(!verbose) {
            m <- grep("^here\\(\\) starts at", m, value=TRUE, invert=TRUE)
        }
        if(length(m))
            message(m)
    }

    local({

        # change to this directory and setup here()
        local_dir(new_here)

        # redirect here temporarily
        f(i_am(path_rel(tf_temp, new_here)))

    })

    if(chdir)
        local_dir(new_here)

    local(expr)

}

##' Changes the here() root for current environment
##'
##' Changes [here::here()] to temporarily point to a new directory for the active
##' entironment. Changes back afterwards.
##' @title Temporarily Change Project Root
##' @param new_here new temporary here root directory
##' @param chdir change working directory also
##' @param verbose show here's messages on setting new root
##' @param .local_envir the environment to use for scoping, see [withr::local_dir()]
##' @return The original here() root
##' @author Torbjørn Lindahl
##' @importFrom withr local_tempfile local_dir defer
##' @importFrom here here i_am
##' @importFrom fs path_rel dir_exists
##' @importFrom utils capture.output
##' @export
##' @examples
##' library(here)
##'
##' myfun <- function() {
##'
##'   d <- tempfile()
##'   dir.create(d)
##'
##'   local_here(d)
##'
##'   cat("here() is now: ",here(),"\n")
##'   stopifnot(d == here())
##'
##'   # do something
##'
##'   unlink(d, recursive=TRUE)
##'
##' }
local_here <- function(new_here, chdir=FALSE, verbose=FALSE, .local_envir = parent.frame() ) {

    if(!dir_exists(new_here))
        stop("new_here must be a directory")

    current_here <- here()

    # create a temporary file to get us back when we're done
    tf_current <- local_tempfile(tmpdir=current_here, pattern=".here", .local_envir=.local_envir)
    .file_touch(tf_current)

    # make sure it goes back aftrwards (this will trigger after the
    # above local_dir defer has changed the working dir back
    defer(f(local({
        local_dir(current_here)
        i_am(path_rel(tf_current, current_here))
    })), envir=.local_envir)


    # create another remporary file to get us where we want to go
    tf_temp <- local_tempfile(tmpdir=new_here, pattern=".here")
    .file_touch(tf_temp)

    # opt to suppress i_am's "here() starts at"
    f <- function(x) {
        m <- capture.output(x, type="message")
        if(!verbose) {
            m <- grep("^here\\(\\) starts at", m, value=TRUE, invert=TRUE)
        }
        if(length(m))
            message(m)
    }

    local({

        # change to this directory and setup here()
        local_dir(new_here)

        # redirect here temporarily
        f(i_am(path_rel(tf_temp, new_here)))

    })

    if(chdir)
        local_dir(new_here, .local_envir=.local_envir)

    invisible(current_here)

}

# fs::file_touch() is causing problems on r-devel on windows. Rolling
# our own using base functions only.
.file_touch <- function(path) {
    file.create(path)
    stopifnot(file.exists(path))
}
