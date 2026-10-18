.libPaths(c("C:/Users/dudup/AppData/Local/R/win-library/4.6",.libPaths()))
suppressPackageStartupMessages(library(bsvarSIGNs))
library(bsvars)

cat("BSVARS FORECAST\n");print(getS3method("forecast","PosteriorBSVAR"))
cat("BSVARS POST CONSTRUCTOR\n");print(specify_posterior_bsvar$public_methods$initialize)
cat("FORECAST EXPORTED FUNCTIONS\n");print(grep("forecast",getNamespaceExports("bsvars"),value=TRUE))
cat("TOOLS\n"); print(Sys.which(c("make","g++")))
cat("BVAR INSTALLED\n");print(requireNamespace("BVAR",quietly=TRUE))
