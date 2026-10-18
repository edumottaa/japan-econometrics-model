f <- path.expand("~/.Renviron")
cat("R home (~):", normalizePath("~", winslash = "/"), "\n")
cat(".Renviron path:", f, "| exists:", file.exists(f), "\n")
if (file.exists(f)) {
  l <- readLines(f, warn = FALSE)
  cat("variables defined (names only):", paste(sub("=.*", "", l[grepl("=", l)]), collapse = ", "), "\n")
}
cat("project folder has .Renviron:", file.exists("C:/Users/dudup/OneDrive/NT Macro/.Renviron"), "\n")
cat("FRED_API_KEY set now:", nzchar(Sys.getenv("FRED_API_KEY")), "| ESTAT_APP_ID set now:", nzchar(Sys.getenv("ESTAT_APP_ID")), "\n")
