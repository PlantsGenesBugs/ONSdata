library(tidyverse)
library(readxl)
#install.packages("janitor")
library(janitor) #clean_names()
#install.packages("pheatmap")
library(pheatmap) 

# Source: 
# https://www.nomisweb.co.uk/articles/299.aspx
# https://www.nomisweb.co.uk/query/select/getdatasetbytheme.asp?opt=3&theme=&subgrp=
# The earnings estimates in the output table(s) will show the earnings in pounds for employees who are on adult rates of pay and 
# those pay was not affected by absence.
# The quality of an estimate can be assessed by referring to its coefficient of variation (CV) which is shown next to the earnings
# estimate. The CV is the ratio of the standard error of an estimate to the estimate. Estimates with larger CVs will be less
# reliable than those with smaller CVs.
# In their published spreadsheets, ONS use the following CV values to give an indication of the quality of an estimate:
  
#CV Value	                Quality of estimate
#5% or lower            	Precise
#over 5%, up to 10%	      Reasonable precise
#over 10%, up to 20%  	 Acceptable, but use with caution
#over 20%	               unreliable, figures suppressed

# read data
pay_data2 <- read_excel("nomis_2023_08_05_125508.xlsx")

# remove information that won't be used in calculations
pay_data2 <- pay_data2 |>
  slice(-c(1:7)) |>
  slice(-c(45:49))

# prepare and define column names
pay_data2$...3[pay_data2$...2 == 1998] <- 1998
pay_data2$...5[pay_data2$...4 == 2000] <- 2000
pay_data2$...7[pay_data2$...6 == 2002] <- 2002
pay_data2$...9[pay_data2$...8 == 2004] <- 2004
pay_data2$...11[pay_data2$...10 == 2006] <- 2006
pay_data2$...13[pay_data2$...12 == 2008] <- 2008
pay_data2$...15[pay_data2$...14 == 2010] <- 2010
pay_data2$...17[pay_data2$...16 == 2012] <- 2012
pay_data2$...19[pay_data2$...18 == 2014] <- 2014
pay_data2$...21[pay_data2$...20 == 2016] <- 2016
pay_data2$...22[pay_data2$...22 == 2018] <- 2018
pay_data2$...24[pay_data2$...24 == 2020] <- 2020
pay_data2$...26[pay_data2$...26 == 2022] <- 2022

column_names2 <- paste(pay_data2[1,], pay_data2[2,])
colnames(pay_data2) <- column_names2

pay_data2 <- pay_data2 |>
  clean_names() |>
  slice(-c(1,2)) |>
  mutate_at(c(2:27), as.numeric)
colnames(pay_data2)[1] <- "english_county"

names(pay_data2) <- gsub(pattern = "_number", replacement = "", x=names(pay_data2))

# prepare data for heat map
matrix2 <- pay_data2 |>
  column_to_rownames(var="english_county") |>
  select(seq(1, 26, 2)) |>
  as.matrix()

# draw and save heatmap
pheatmap(matrix2, cluster_cols=F,
         main = "Median weekly earnings in English counties from 1998 to 2022",
         cutree_rows=3,
         cellwidth = 15, cellheight = 12, fontsize = 8, filename = "earnings.tiff")




