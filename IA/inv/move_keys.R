# Moves the two API keys from the original script into ~/.Renviron and replaces them in the
# corrected copy by Sys.getenv(). Key values are never printed.
orig_path <- "C:/Users/dudup/OneDrive/NT Macro/model_clean_comments.R"
new_path  <- "C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/model_clean_comments_corrigido.R"

orig <- readLines(orig_path, warn = FALSE, encoding = "UTF-8")
fred_key  <- sub('.*fredr_set_key\\("([^"]+)"\\).*', "\\1", grep('fredr_set_key\\("', orig, value = TRUE))
estat_key <- sub('.*appId\\s*=\\s*"([^"]+)".*', "\\1", grep('appId\\s*=\\s*"', orig, value = TRUE))
stopifnot(length(fred_key) == 1, length(estat_key) == 1, nchar(fred_key) >= 20, nchar(estat_key) >= 20)

renv <- path.expand("~/.Renviron")
stopifnot(!file.exists(renv))
writeLines(c(paste0("FRED_API_KEY=", fred_key), paste0("ESTAT_APP_ID=", estat_key)), renv)

new <- readLines(new_path, warn = FALSE, encoding = "UTF-8")
i <- grep('fredr_set_key\\("', new)
j <- grep('appId\\s*=\\s*"', new)
stopifnot(length(i) == 1, length(j) == 1)
new[i] <- 'fredr_set_key(Sys.getenv("FRED_API_KEY"))   # key stored in ~/.Renviron'
new[j] <- '  appId       = Sys.getenv("ESTAT_APP_ID"),  # key stored in ~/.Renviron'
writeLines(new, new_path, useBytes = TRUE)

cat(".Renviron created:", file.exists(renv), "\n")
cat("keys left in the corrected script:",
    any(grepl(fred_key, new, fixed = TRUE) | grepl(estat_key, new, fixed = TRUE)), "\n")
cat("edited lines:", i, j, "\n")
