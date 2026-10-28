source("renv/activate.R")
# Warn if the running R version doesn't match the one recorded in renv.lock
local({
  check_r_version <- function() {
    if (!interactive()) {
      return(invisible())
    }
    lockfile <- "renv.lock"
    if (!file.exists(lockfile)) {
      return(invisible())
    }

    # Read R version from the lockfile without needing any packages
    txt <- paste(readLines(lockfile, warn = FALSE), collapse = "\n")
    m <- regmatches(
      txt,
      regexec('"R"\\s*:\\s*\\{\\s*"Version"\\s*:\\s*"([0-9.]+)"', txt)
    )[[1]]
    if (length(m) < 2) {
      return(invisible())
    }

    locked <- numeric_version(m[2])
    current <- getRversion()
    major_minor <- function(v) paste(unlist(v)[1:2], collapse = ".")

    if (major_minor(locked) != major_minor(current)) {
      warning(
        sprintf(
          paste0(
            "This project's renv.lock expects R %s, but you are running R %s. If you do not this version installed do the following\n",
            "Option 1: Install R version from cran manually and switch to it in your IDE session \n",
            "Option 2: Manage R versions with rig\n",
            "  1. Open PowerShell window at the project folder, start rig in user mode: rig system user-mode\n",
            "  2. Run to install R version matching what is listed in renv locker file: rig add renv.lock\n",
            "  3. Confirm you see the install with: rig list\n",
            "  4. Select that R version in Positron (Interpreter: Select Interpreter Session)\n",
            "  5. Restart R, then run renv::restore()\n",
            "  6. If a package requires Rtools for building, you can install it with no admin access in a user mode session of rig with: rig add rtools"
          ),
          as.character(locked),
          as.character(current)
        ),
        call. = FALSE,
        immediate. = TRUE
      )
    }
    invisible()
  }

  # Never let this check break R startup
  try(check_r_version(), silent = TRUE)
})
