# Load project utility functions for unit tests.
project_root <- normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = TRUE)
old_wd <- setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)
source(file.path(project_root, "R", "utils.R"), local = FALSE)
