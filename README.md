# PCA-Explorer + DGE analysis using DESeq2

From terminal Launch R and INSTALL the following packages:

`if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")`<br>
`if (!requireNamespace("DESeq2", quietly = TRUE))
  BiocManager::install("DESeq2")`<br>
`if (!requireNamespace("dplyr", quietly = TRUE))
 install.packages("dplyr")`<br>
`if (!requireNamespace("plotly", quietly = TRUE))
  install.packages("plotly")`<br>
`if (!requireNamespace("shiny", quietly = TRUE))
  BiocManager::install("shiny")`<br>
`if (!requireNamespace("tibble", quietly = TRUE))
 install.packages("tibble")`<br>
`if (!requireNamespace("DT", quietly = TRUE))
  install.packages("DT")`<br>
`if (!requireNamespace("shinyjs", quietly = TRUE))
  BiocManager::install("shinyjs")`<br>

git clone this repository using `git clone https://github.com/bixBeta/PCA-Explorer.git`<br>
cd to `PCA-Explorer/PCA-Explorer`<br>
copy the path for the ui and server files by typing `pwd`

From terminal type `R -e shiny::runApp('~/path/to/PCA-Explorer/PCA-Explorer')`<br>
Copy the ip address and paste it in a web browser to launch the app

Example CountMatrix and metadata target files are available in the `examples` folder



