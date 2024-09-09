##' Temporarily change the here() root
##'
##' Changes [here::here()] to temporarily point to a new
##' directory. Automatically changes back to the original value when
##' finished.
##'
##' The with_* and local_* flavours of this functionality mimics that
##' which is typically used in the withr package.
##' @title Temporarily Change Project Root
##' @param new_here new temporary here root directory
##' @param expr expression to evaluate
##' @param chdir also temporarily change working directory
##' @param verbose show here's messages on setting new root
##' @return [with_here()] returns the result of the
##'     expression. [local_here()] returns the original value of
##'     here(), before the change.
##' @author Torbjørn Lindahl
##' @importFrom withr local_tempfile local_dir defer
##' @importFrom here here i_am
##' @importFrom fs file_touch path_rel dir_exists
##' @export
##' @rdname with_here
##' @examples
##' library(here)
##' library(withr)
##'
##' d <- local_tempdir()
##'
##' cat("here() is initially:", here(), "\n")
##'
##' # temporarily do things uner a different here() root:
##' with_here(d, cat("but here() is now:", here(), "\n"))
##'
##' # check that everything is shifted back
##' cat("here() is now again:", here(), "\n")
##'
##' local({
##'
##'   d <- local_tempdir()
##'
##'   cat("here was initially: ", here(), "\n")
##'
##'   local_here(d)
##'
##'   cat("after local_here(), here() is: ",here(),"\n")
##'   stopifnot(normalizePath(d) == normalizePath(here()))
##'
##'   # do something that requires here() be elsewhere
##'
##' })
##'
##' cat("outside the block, here() is again:", here(), "\n")
with_here <- function(new_here, expr, chdir=FALSE, verbose=FALSE ) {

    if(!dir_exists(new_here))
        stop("new_here must be a directory")

    current_here <- here()

    # create a temporary file to get us back when we're done
    tf_current <- file_beacon(current_here)


    # make sure it goes back aftrwards (this will trigger after the
    # above local_dir defer has changed the working dir back
    defer(suppress_here_message(local({
        local_dir(current_here)
        i_am(path_rel(tf_current, current_here))
    }), verbose=verbose))

    # create another remporary file to get us where we want to go
    tf_temp <- file_beacon(new_here)

    local({

        # change to this directory and setup here()
        local_dir(new_here)

        # redirect here temporarily
        suppress_here_message(i_am(path_rel(tf_temp, new_here)), verbose=verbose)

    })

    if(chdir)
        local_dir(new_here)

    local(expr)

}

##' @param .local_envir the environment to use for scoping, see [withr::local_dir()]
##' @importFrom withr local_tempfile local_dir defer
##' @importFrom here here i_am
##' @importFrom fs path_rel dir_exists
##' @export
##' @rdname with_here
local_here <- function(new_here, chdir=FALSE, verbose=FALSE, .local_envir = parent.frame() ) {

    if(!dir_exists(new_here))
        stop("new_here must be a directory")

    current_here <- here()

    # create a temporary file to get us back when we're done
    tf_current <- file_beacon(current_here)

    # make sure it goes back aftrwards (this will trigger after the
    # above local_dir defer has changed the working dir back
    defer(suppress_here_message(local({
        local_dir(current_here)
        i_am(path_rel(tf_current, current_here))
    }), verbose=verbose), envir=.local_envir)

    # create another remporary file to get us where we want to go
    tf_temp <- file_beacon(new_here)

    local({

        # change to this directory and setup here()
        local_dir(new_here)

        # redirect here temporarily
        suppress_here_message(i_am(path_rel(tf_temp, new_here)), verbose=verbose)

    })

    if(chdir)
        local_dir(new_here, .local_envir=.local_envir)

    invisible(current_here)

}

##' @importFrom fs dir_ls is_file
##' @importFrom withr local_tempfile
file_beacon <- function(where) {

    stopifnot(dir_exists(where))
    existing_paths <- Filter(is_file, dir_ls(where, all=TRUE))

    if(length(existing_paths))
        return(existing_paths[1])

    # create a temporary local to the parent environment
    f <- local_tempfile(tmpdir=where, .local_envir=parent.frame(), pattern=".here")
    file_touch(f)
    return(f)

}


##' @importFrom utils capture.output
suppress_here_message <- function(x, verbose=TRUE) {
    m <- capture.output(x, type="message")
    if(!verbose) {
        m <- grep("^here\\(\\) starts at", m, value=TRUE, invert=TRUE)
    }
    if(length(m))
        message(m)
}
