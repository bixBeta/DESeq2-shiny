# PCA-Explorer + DGE analysis using DESeq2

```### Launch R and INSTALL the following packages
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
if (!requireNamespace("DESeq2", quietly = TRUE))
  BiocManager::install("DESeq2")
if (!requireNamespace("dplyr", quietly = TRUE))
 install.packages("dplyr")
if (!requireNamespace("plotly", quietly = TRUE))
  install.packages("plotly")
if (!requireNamespace("shiny", quietly = TRUE))
  BiocManager::install("shiny")
if (!requireNamespace("tibble", quietly = TRUE))
 install.packages("tibble")
if (!requireNamespace("DT", quietly = TRUE))
  install.packages("DT")
if (!requireNamespace("shinyjs", quietly = TRUE))
  BiocManager::install("shinyjs")```

git clone this repository using `git clone https://github.com/bixBeta/PCA-Explorer.git`
cd to `PCA-Explorer/PCA-Explorer`
copy the path for the ui and server files by typing `pwd`

From terminal type `R -e shiny::runApp('~/path/to/PCA-Explorer/PCA-Explorer')`



