library(pdftools)
f <- "C:/Users/dudup/Downloads/bvar aplicação macro.pdf"
n <- pdf_info(f)$pages
for (i in seq_len(n)) pdf_convert(f, format = "png", pages = i, dpi = 80,
  filenames = sprintf("C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/pm09_%d.png", i), verbose = FALSE)
cat("pages:", n, "\n")
