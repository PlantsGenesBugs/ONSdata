## Annual Survey of Hours and Earnings

Data was obtained from the [Annual Survey of Hours and Earnings](https://www.nomisweb.co.uk/query/select/getdatasetbytheme.asp?opt=3&theme=&subgrp=) (workplace analysis) and is downloadable from this repository ("nomis_2023_08_05_125508.xlsx").

The heatmap shows that there are largely 3 clusters where earning is concerned in England: Inner London, some counties in the South up to the West Midlands, and Everything Else. It would be interesting to see which counties have increased their earnings faster than others (or which haver performed poorly - I'm looking at [Northamptonshire](https://www.northantslive.news/news/northamptonshire-news/two-years-after-northamptonshire-county-4209584) in particular). But I will leave that for later.

For this analysis I used a few packages I don't usually use, including janitor (to easily clean column names) and pheatmap (to create the nice heatmap below). In order to save the heatmap, you have to pass the file details as arguments to the function that produces the image, which is a bit unusual (and means you don't get a preview of the image in RStudio).

The code can be improved. In particular, tidying up the data and creating the column names should be possible using a function. I wanted to merge the values in rows 1 and 2 into a single row and then promote that row to the column names, but couldn't quite get the rows to merge (I couldn't get [merge_rows()](https://search.r-project.org/CRAN/refmans/unpivotr/html/merge_cells.html) to work!). The repeating code should also be replaced with a loop or a function, but I will leave it as is for now and come back to it later.
