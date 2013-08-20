### Presentation to Chicago R-User's Group (CRUG) on May 1, 2013
### Peter Carl
### Favorite Packages: xlsx
 
### Overview
# Package: xlsx
# Type: Package
# Title: Read, write, format Excel 2007 and Excel 97/2000/XP/2003 files
# Version: 0.5.0
# Date: 2012-09-23
# Depends: xlsxjars, rJava
# Author and Maintainer: Adrian A. Dragulescu &lt;adrian.dragulescu@gmail.com<script type="text/javascript">
/* <![CDATA[ */
(function(){try{var s,a,i,j,r,c,l,b=document.getElementsByTagName("script");l=b[b.length-1].previousSibling;a=l.getAttribute('data-cfemail');if(a){s='';r=parseInt(a.substr(0,2),16);for(j=2;a.length-j;j+=2){c=parseInt(a.substr(j,2),16)^r;s+=String.fromCharCode(c);}s=document.createTextNode(s);l.parentNode.replaceChild(s,l);}}catch(e){}})();
/* ]]> */
</script>&gt;
# License: GPL-3
# URL: http://code.google.com/p/rexcel/
 
### Installation
# Get it from CRAN
# install.packages(xlsx)
 
### Preparing the workspace
require(PerformanceAnalytics)
require(xlsx)
 
### Reading from an Excel worksheet
# Download the file using wget
url &lt;- &quot;http://www.djindexes.com/mdsidx/downloads/xlspages/ubsci_public/DJUBS_full_hist.xls&quot;
system(paste('wget ', url))
 
# Read in the workbook data on the second sheet
# x = read.xlsx(&quot;DJUBS_full_hist.xls&quot;, sheet=&quot;sheet&quot;Total Return&quot;, stringsAsFactors=FALSE) # Too slow for big spreadsheets
x &lt;- read.xlsx2(&quot;DJUBS_full_hist.xls&quot;, sheetName=&quot;Total Return&quot;, header=TRUE, startRow=3, as.data.frame=TRUE, stringsAsFactors=FALSE, colClasses=c(&quot;Date&quot;, rep(&quot;numeric&quot;, 100)))
# The read.xlsx2 function does more work in Java so it achieves better performance (an order of magnitude faster on sheets with 100,000 cells or more).  Much faster, but dates come in as numeric unless specified in colClasses.
 
# Or the result can be fixed with this...
# excelDate2Date &lt;- function(excelDate) { # originally from HFWutils pkg, now abandoned
#   Date &lt;- excelDate + as.Date(&quot;1900-01-01&quot;) - 2
#   ## FIXME: add &quot;if &gt;1900-Feb-28&quot; switch?
#   return(Date)
# }
 
# Read the more descriptive headings from a specific sheet
x.tags &lt;- read.xlsx2(&quot;DJUBS_full_hist.xls&quot;, sheetName=&quot;Total Return&quot;, header=FALSE, startRow=1, endRow=3, as.data.frame=TRUE, stringsAsFactors=FALSE)
 
# head(x, n=10) # get a sense of what we've read in
# tail(x, n=10) # the author has some notes at the end of the data
#
# Comes in as a mix of classes in a data.frame
# &gt; class(x)
# [1] &quot;data.frame&quot;
# &gt; class(x[2,2])
# [1] &quot;numeric&quot;
# &gt; class(x[1,1])
# [1] &quot;Date&quot;
 
### Parsing the data
# Everything was read in as a string, except for a few NA's at the end
# x = na.omit(x)
 
# Get rid of the last two lines, which contains the disclaimer
x = x[-which(is.na(x[,1])),]
 
# Remove blank columns between sections for both the data and the tags
x = x[,-which(lapply(x,function(x)all(is.nan(x)))==TRUE)]
x.tags = x.tags[,-which(apply(x.tags,2,function(x)all(x==&quot;&quot;)))]
 
# Parse the dates, remembering that Excel does not keep track of time zones and DST
x.ISOdates = x[,1]
 
# Convert data into a time series of prices
x.P=as.xts(x[-1], order.by=x.ISOdates)
 
# Rename the columns using something more descriptive
colnames(x.P) = x.tags[2,-1]
 
# Use the descriptive data to identify subsets
# &gt; unique(as.character(x.tags[1,]))
# [1] &quot;&quot;                       &quot;Currency&quot;               &quot;Subindex&quot;               &quot;Individual Commodities&quot;
# [5] &quot;Additional Commodities&quot;
 
# Use subsetting to get a vector of column names
# &gt; as.character(x.tags[2, which(x.tags[1,]==&quot;Subindex&quot;)])
# [1] &quot;Agriculture&quot;       &quot;Energy&quot;            &quot;ExEnergy&quot;          &quot;Grains&quot;            &quot;Industrial Metals&quot;
# [6] &quot;Livestock&quot;         &quot;Petroleum&quot;         &quot;Precious Metals&quot;   &quot;Softs&quot;             &quot;Composite Crude&quot;
# [11] &quot;Composite Wheat&quot;
x.subindexes = as.character(x.tags[2, which(x.tags[1,]==&quot;Subindex&quot;)])
 
# &gt; as.character(x.tags[2, grep(&quot;Commodities&quot;, x.tags[1,])])
# [1] &quot;Aluminum&quot;          &quot;Brent Crude&quot;       &quot;Coffee&quot;            &quot;Copper (COMEX)&quot;    &quot;Corn&quot;
# [6] &quot;Cotton&quot;            &quot;Gold&quot;              &quot;Heating Oil&quot;       &quot;Kansas Wheat&quot;      &quot;Lean Hogs&quot;
# [11] &quot;Live Cattle&quot;       &quot;Natural Gas&quot;       &quot;Nickel&quot;            &quot;Silver&quot;            &quot;Soybeans&quot;
# [16] &quot;Soybean Meal&quot;      &quot;Soybean Oil&quot;       &quot;Sugar&quot;             &quot;Unleaded Gasoline&quot; &quot;Wheat&quot;
# [21] &quot;WTI Crude Oil&quot;     &quot;Zinc&quot;              &quot;Cocoa&quot;             &quot;Lead&quot;              &quot;Platinum&quot;
# [26] &quot;Tin&quot;
x.commodities = as.character(x.tags[2, grep(&quot;Commodities&quot;, x.tags[1,])])
 
# Calculate returns from prices
x.R = Return.calculate(x.P[,x.commodities])
 
# --- Slide 0 ---
# &gt; head(x.R)
#                 Aluminum  Brent Crude        Coffee Copper (COMEX)         Corn       Cotton         Gold
# 1991-01-02            NA           NA            NA             NA           NA           NA           NA
# 1991-01-03  0.0110040000 -0.045238000  0.0138090000   -0.024966000  0.002338000  0.013373000 -0.005445000
# 1991-01-04  0.0004599388 -0.058984333 -0.0037413359   -0.003259374  0.006639477 -0.002423589 -0.004190819
# 1991-01-07  0.0060614809  0.150057989  0.0174145756    0.008306786  0.008027806 -0.007552549  0.023785651
# 1991-01-08 -0.0166027909 -0.026213992  0.0007347181   -0.019509577 -0.011495507 -0.003766638 -0.009661283
# 1991-01-09 -0.0055101154  0.008863234 -0.0031341165   -0.008988240 -0.004114776 -0.002593289  0.001912069
# ...
 
### Analyzing the data
# --- Slide 1 ---
# Create a table of summary statistics
x.AnnRet = t(table.AnnualizedReturns(x.R), Rf=0.3/12)
x.RiskStats = as.data.frame(t(table.RiskStats(x.R)))
# &gt; x.RiskStats
#                   Annualized Return Annualized Std Dev Annualized Sharpe Ratio Annualized Downside Deviation
# Aluminum                    -0.0110             0.2022                 -0.0542                        0.1433
# Brent Crude                  0.1233             0.3080                  0.4002                        0.2156
# Coffee                      -0.0403             0.3745                 -0.1075                        0.2551
# Copper (COMEX)               0.0909             0.2690                  0.3379                        0.1873
# Corn                        -0.0387             0.2538                 -0.1525                        0.1769
# ...
 
### Writing the resulting table to an Excel worksheet
# --- Slide 2 ---
# Create a new workbook for outputs
outwb &lt;- createWorkbook()
 
# Define some cell styles within that workbook
csSheetTitle &lt;- CellStyle(outwb) + Font(outwb, heightInPoints=14, isBold=TRUE)
csSheetSubTitle &lt;- CellStyle(outwb) + Font(outwb, heightInPoints=12, isItalic=TRUE, isBold=FALSE)
csTableRowNames &lt;- CellStyle(outwb) + Font(outwb, isBold=TRUE)
csTableColNames &lt;- CellStyle(outwb) + Font(outwb, isBold=TRUE) + Alignment(wrapText=TRUE, h=&quot;ALIGN_CENTER&quot;) + Border(color=&quot;black&quot;, position=c(&quot;TOP&quot;, &quot;BOTTOM&quot;), pen=c(&quot;BORDER_THIN&quot;, &quot;BORDER_THICK&quot;))
csRatioColumn &lt;- CellStyle(outwb, dataFormat=DataFormat(&quot;0.0&quot;)) # ... for ratio results
csPercColumn &lt;- CellStyle(outwb, dataFormat=DataFormat(&quot;0.0%&quot;)) # ... for percentage results
 
# --- Slide 3 ---
# Which columns in the table should be formatted which way?
RiskStats.colRatio = list(
'3'=csRatioColumn,
'5'=csRatioColumn,
'8'=csRatioColumn,
'15'=csRatioColumn)
RiskStats.colPerc =list(
'1'=csPercColumn,
'2'=csPercColumn,
'4'=csPercColumn,
'6'=csPercColumn,
'7'=csPercColumn,
'9'=csPercColumn,
'10'=csPercColumn,
'13'=csPercColumn,
'14'=csPercColumn)
 
# --- Slide 4 ---
# Create a sheet in that workbook to contain the table
sheet &lt;- createSheet(outwb, sheetName = &quot;Performance Table&quot;)
 
# Add the table calculated above to the sheet
addDataFrame(x.RiskStats, sheet, startRow=3, startColumn=1, colStyle=c(RiskStats.colPerc,RiskStats.colRatio), colnamesStyle = csTableColNames, rownamesStyle=csTableRowNames)
setColumnWidth(sheet,colIndex=c(2:15),colWidth=11)
setColumnWidth(sheet,colIndex=16,colWidth=13)
setColumnWidth(sheet,colIndex=17,colWidth=6)
setColumnWidth(sheet,colIndex=1,colWidth=0.8*max(length(rownames(x.RiskStats))))
 
# --- Slide 5 ---
# Create the Sheet title ...
rows &lt;- createRow(sheet,rowIndex=1)
sheetTitle &lt;- createCell(rows, colIndex=1)
setCellValue(sheetTitle[[1,1]], &quot;Ex-Post Returns and Risk&quot;)
setCellStyle(sheetTitle[[1,1]], csSheetTitle)
# ... and subtitle
rows &lt;- createRow(sheet,rowIndex=2)
sheetSubTitle &lt;- createCell(rows,colIndex=1)
setCellValue(sheetSubTitle[[1,1]], &quot;Since Inception&quot;)
setCellStyle(sheetSubTitle[[1,1]], csSheetSubTitle)
 
### Add a chart to a different sheet
# --- Slide 6 ---
# Construct the chart as a  dib, emf, jpeg, pict, png, or wmf file.
require(gplots)
skewedG2R20 = c(colorpanel(16, &quot;darkgreen&quot;,&quot;yellow&quot;), colorpanel(5, &quot;yellow&quot;, &quot;darkred&quot;)[-1])
png(filename = &quot;corr.jpeg&quot;, width = 6, height = 8, units = &quot;in&quot;, pointsize=12, res=120)
require(PApages)
page.CorHeatmap(x.R[,x.commodities], Colv=TRUE, breaks = seq(-1,1,by=.1), symkey=TRUE, col=skewedG2R20, tracecol=&quot;darkblue&quot;, cexRow=0.9, cexCol=0.9)
dev.off()
 
# --- Slide 7 ---
# Create a sheet in that workbook to contain the graph
sheet &lt;- createSheet(outwb, sheetName = &quot;Correlation Chart&quot;)
 
# Create the Sheet title and subtitle
rows &lt;- createRow(sheet,rowIndex=1)
sheetTitle &lt;- createCell(rows, colIndex=1)
setCellValue(sheetTitle[[1,1]], &quot;Correlations Among Commodities&quot;)
setCellStyle(sheetTitle[[1,1]], csSheetTitle)
rows &lt;- createRow(sheet,rowIndex=2)
sheetSubTitle &lt;- createCell(rows,colIndex=1)
setCellValue(sheetSubTitle[[1,1]], &quot;Correlations of daily returns since inception&quot;)
setCellStyle(sheetSubTitle[[1,1]], csSheetSubTitle)
 
# Add the file created previously
addPicture(&quot;corr.jpeg&quot;, sheet, scale = 1, startRow = 4, startColumn = 1)
 
# --- Slide 8 ---
# Save the workbook to a file...
saveWorkbook(outwb, &quot;DJUBS Commodities Performance Summary.xlsx&quot;)
 
# --- Slides 9, 10 ---
# Show screen captures of the resulting workbook