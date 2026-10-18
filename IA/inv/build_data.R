wd <- tempdir()
out <- "C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv"
setwd(wd)
src <- readLines("C:/Users/dudup/Downloads/model_clean_comments.R", encoding = "UTF-8")
keep <- c(1:129, 142:155, 170:179)
writeLines(src[keep], file.path(wd, "partial.R"), useBytes = TRUE)
source(file.path(wd, "partial.R"), encoding = "UTF-8")
saveRDS(list(ext = matrix_externa, int = matrix_interna,
             dates = as.character(data_bal$date)), file.path(out, "mats.rds"))
cat("dims:", dim(matrix_externa), "|", dim(matrix_interna), "\n")
cat("range dates:", as.character(range(data_bal$date)), "\n")
print(summary(cbind(matrix_externa, matrix_interna)))
