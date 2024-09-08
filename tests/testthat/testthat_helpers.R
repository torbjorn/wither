expect_pathequal <- function(path1, path2, mustWork = FALSE, ...) {
    expect_equal(
        normalizePath(path1, mustWork = mustWork),
        normalizePath(path2, mustWork = mustWork),
        ...
    )
}

