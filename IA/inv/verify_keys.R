orig <- readLines("C:/Users/dudup/OneDrive/NT Macro/model_clean_comments.R", warn = FALSE)
cat("fresh R session reads FRED_API_KEY from .Renviron, same as original:",
    any(grepl(Sys.getenv("FRED_API_KEY"), orig, fixed = TRUE)) && nzchar(Sys.getenv("FRED_API_KEY")), "\n")
cat("fresh R session reads ESTAT_APP_ID from .Renviron, same as original:",
    any(grepl(Sys.getenv("ESTAT_APP_ID"), orig, fixed = TRUE)) && nzchar(Sys.getenv("ESTAT_APP_ID")), "\n")
invisible(parse("model_clean_comments_corrigido.R")); cat("corrected script parses OK\n")
