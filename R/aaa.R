
#managing environments in hi_recreation to minimize load times in subsequent runs
the <- new.env(parent = emptyenv())
the$first_time <- TRUE
