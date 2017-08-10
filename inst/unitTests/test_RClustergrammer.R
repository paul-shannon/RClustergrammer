library(RClustergrammer)
library(RUnit)
#------------------------------------------------------------------------------------------------------------------------
PORT_RANGE <- 12000:12050
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_constructor()
   test_createSampleData()
   test_getSetMatrix()
   test_getSetMetadata()
   test_clusterAndDisplay_tiny()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
createSampleData <- function(rows=3, cols=3, columnMetadataCategories=2, rowMetadataCategories=3)
{
      # create the matrix
   data <- runif(rows*cols, min=-3, max=3)
   mtx <- matrix(data, nrow=rows, ncol=cols, dimnames=list(paste("R", 1:rows, sep=""), paste("C", 1:cols, sep="")))

     # now the row metadata table
   tbl.rmd <- data.frame()
   if(rowMetadataCategories > 0){
      category.names <- paste("cat", sample(letters, size=rowMetadataCategories), sep="_")
      tbl.rmd <- data.frame(stringsAsFactors=FALSE, row.names=rownames(mtx))
      for(cat.name in category.names){
         values <- paste(cat.name, sample(1:100, size=rows), sep=".")
         tbl.rmd <- cbind(tbl.rmd, values)
         }
      colnames(tbl.rmd) <- category.names
      } # if rmd categories > 0

     # now the column metadata table
   tbl.cmd <- data.frame()
   if(columnMetadataCategories > 0){
      category.names <- paste("cat", sample(LETTERS, size=columnMetadataCategories), sep="_")
      tbl.cmd <- data.frame(stringsAsFactors=FALSE, row.names=colnames(mtx))
      for(cat.name in category.names){
         values <- paste(cat.name, sample(1:100, size=cols), sep=".")
         tbl.cmd <- cbind(tbl.cmd, values)
         }
      colnames(tbl.cmd) <- category.names
      } # if cmd categories > 0

    list(mtx=mtx, rmd=tbl.rmd, cmd=tbl.cmd)

} # createSampleData
#------------------------------------------------------------------------------------------------------------------------
test_createSampleData <- function()
{
   printf("--- test_createSmallMatrix")

   x <- createSampleData(rows=3, cols=3, columnMetadataCategories=2, rowMetadataCategories=3)
   checkEquals(names(x), c("mtx", "rmd", "cmd"))
   checkEquals(dim(x$mtx), c(3, 3))
   checkEquals(rownames(x$rmd), rownames(x$mtx))
   checkEquals(length(colnames(x$rmd)), 3)

   x <- createSampleData(rows=10, cols=5, columnMetadataCategories=0, rowMetadataCategories=2)
   checkEquals(names(x), c("mtx", "rmd", "cmd"))
   checkEquals(dim(x$mtx), c(10, 5))
   checkEquals(rownames(x$rmd), rownames(x$mtx))
   checkEquals(x$cmd, data.frame())

} # test_createSampleData
#------------------------------------------------------------------------------------------------------------------------
test_constructor <- function()
{
   printf("--- test_constructor")

      # create with an empty matrix
   rcg <- RClustergrammer(portRange=PORT_RANGE, mtx=matrix())
   checkEquals(sort(is(rcg)), c("BrowserVizClass", "RClustergrammer"))
   checkTrue(all(is.na(getMatrix(rcg))))

} # test_constructor
#------------------------------------------------------------------------------------------------------------------------
test_getSetMatrix <- function()
{
   printf("--- test_getSetMatrix")

      # create with an empty matrix

   mtx.3x3 <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
   rcg <- RClustergrammer(portRange=PORT_RANGE, mtx=mtx.3x3)
   checkEquals(getMatrix(rcg), mtx.3x3)

   mtx.6x4 <- matrix(1:24, nrow=6, dimnames=list(paste("R", 1:6, sep=""), paste("C", 1:4, sep="")))
   setMatrix(rcg, mtx.6x4)
   checkEquals(getMatrix(rcg), mtx.6x4)

} # test_getSetMatrix
#------------------------------------------------------------------------------------------------------------------------
test_getSetMetadata <- function()
{
   printf("--- test_getSetMetadata")

      # create with an empty matrix

   mtx.6x4 <- matrix(1:24, nrow=6, dimnames=list(paste("R", 1:6, sep=""), paste("C", 1:4, sep="")))

   rcg <- RClustergrammer(portRange=PORT_RANGE, mtx=mtx.6x4)
   checkEquals(getMatrix(rcg), mtx.6x4)

      # two metadata categories for the rows: Placement and Shape
   tbl.rmd <- data.frame(row.names=c("R1","R2","R3", "R4", "R5", "R6"),
                         Placement=c("First", "Second", "Third", "Fourth", "Fifth", "Sixth"),
                         Shape=c("Triangle", "Square", "Circle", "Trapezoid", "Rhombus", "Crescent"),
                         stringsAsFactors=FALSE)

   setRowMetadata(rcg, tbl.rmd)
   checkEquals(getRowMetadata(rcg), tbl.rmd)

     # three metadata categories for the columns: Placement, Colors, group
   tbl.cmd <- data.frame(row.names=c("C1","C2","C3", "C4"),
                         Placement=c("One", "Two", "Three", "Last"),
                         Colors=c("Red", "Green", "Blue", "Magenta"),
                         group=c("w", "x", "y", "z"),
                         stringsAsFactors=FALSE)
   setColumnMetadata(rcg, tbl.cmd)
   checkEquals(getColumnMetadata(rcg), tbl.cmd)

} # test_getSetMetadata
#------------------------------------------------------------------------------------------------------------------------
test_clusterAndDisplay_tiny <- function ()
{
   x <- createSampleData(rows=8, cols=10, columnMetadataCategories=2, rowMetadataCategories=3)

   rcg <- RClustergrammer(portRange=PORT_RANGE, mtx=x$mtx,
                          rowMetadata=x$rmd, columnMetadata=x$cmd, quiet=FALSE)

   rcg <- RClustergrammer(portRange=PORT_RANGE, mtx=mtx.microglial)

   clusterAndDisplay(rcg, method="hclust")

} # test_clusterAndDisplay_tiny
#------------------------------------------------------------------------------------------------------------------------



