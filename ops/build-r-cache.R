# This script should be executed in a running container, never during
# docker build.
#
# This script populates the R package cache from the current R library
# (ie. all currently installed packages in a running container)
#
# This is sometimes necessary if you install packages for the first
# time during docker build, as this means the cache does not get
# populated with these packages.

project <- renv:::renv_project_resolve()
lockfile <- renv:::renv_lockfile_load(project=project)
records <- renv:::renv_lockfile_records(lockfile)

# find base packages
base_packages <- local({
    # From: https://stackoverflow.com/a/46569387/2165993
    subset(as.data.frame(installed.packages()), Priority %in% c("base","recommended"), select=c(Package, Priority))$Package
})

rp <- unlist(Map(\(x)x$Package, records))
records <- records[!rp %in% base_packages]

cached_these <- c()
failed_these <- c()

for( record in records ) {

    path <- renv:::renv_cache_find(record)

    if(!renv:::renv_cache_package_validate(path)) {

        cat(record$Package, "...", sep = " ")

        success <- renv:::renv_cache_synchronize(record, linkable = TRUE)
        if(success) {
            cached_these <- append(cached_these, record$Package)
            cat("OK\n")
        } else {
            failed_these <- append(failed_these, record$Package)
            cat("FAILED\n")
        }
    }

}

if(length(cached_these)) {
    cat("Built cache for these packages:\n")
    cat(paste(cached_these, collapse = ", "), "\n")
}

if(length(failed_these)) {
    cat("Failed to build cache for these packages:\n")
    cat(paste(failed_these, collapse = ", "), "\n")
}

if(!length(cached_these) && !length(failed_these))
    cat("All packages are already cached.\n")
