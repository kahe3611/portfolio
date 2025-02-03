require(pander)
require(dplyr)
library(knitr)
library(markdown)
library(rmarkdown)
library(toOrdinal)
library(ExcelPvtTbls)

# This is main program to generate 22 judicial district reports
districts<-seq(1:2)
doThese<-c(1) # do modeling
for(jd in districts){
   rmarkdown::render('SB185Tables.Rmd',  # file 2
   output_file =  paste(jd, ".docx", sep=''), 
   output_dir = 'Reports')
}

