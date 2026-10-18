library(pdftools)
f <- "C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/Diagnostico_hiperparametros_bsvarSIGNs.pdf"
n <- pdf_info(f)$pages; cat("pages:", n, "\n")
for (i in 1:n) pdf_convert(f, "png", pages = i, dpi = 70,
  filenames = sprintf("C:/Users/dudup/AppData/Roaming/Claude/scratch-workspaces/bd3503e6-7efa-47ef-bfc9-075a162afcc3/947fe66d-9d37-4a09-b09d-efe19023253e/scratch-2026-10-14-6e55f1/inv/pg%d.png", i), verbose = FALSE)
