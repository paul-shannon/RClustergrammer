library(RClustergrammer)
library(RUnit)
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_constructor()
   test_rowFill()
   test_colFill()
   test_addRowMetaData()
   test_addColumnMetaData()
   test_smallMatrixToListWithMetaData()
   test_largeMatrixToListWithMetaData()
   test_matrixToClustergrammerList()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_constructor <- function()
{
   printf("--- test_constructor")
   mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))

   ct <- ClusteringTools(mtx)
   checkTrue(is(ct, "ClusteringTools"))

   #x <- hierarchicalCluster(ct)
   #checkTrue(is.list(x))
   #checkEquals(sort(names(x)), c("col_nodes", "mat", "row_nodes"))

} # test_constructor
#------------------------------------------------------------------------------------------------------------------------
test_rowFill <- function()
{
   printf("--- test_rowFill")

   mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
   hc.rows <- hclust(dist(mtx))
   treeMtx.rows <- cutree(hc.rows, k=1:3) #change based on size of matrix, max k-value = 11

   x <- RClustergrammer:::.rowFill(treeMtx.rows, hc.rows)

   checkTrue(is.list(x))
   checkEquals(length(x), 3)
   checkTrue(is.null(names(x)))

} # test_rowFill
#--------------------------------------------------------------------------------
test_colFill <- function()
{
   printf("--- test_colFill")

   mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
   hc.cols <- hclust(dist(t(mtx)))
   treeMtx.cols <- cutree(hc.cols, k=1:3)
   x <- RClustergrammer:::.colFill(treeMtx.cols, hc.cols)

   checkTrue(is.list(x))
   checkEquals(length(x), 3)
   checkTrue(is.null(names(x)))

} # test_colFill
#--------------------------------------------------------------------------------
test_addRowMetaData <- function()
{
   printf("--- test_addRowMetaData")

   mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
   list.cg <- RClustergrammer:::.matrixToClustergrammerList(mtx)
   tbl.rowmd <- data.frame(row.names=c("R1","R2","R3"),
                           Placement=c("First", "Second", "Third"),
                           Shape=c("Triangle", "Square", "Circle"),
                           stringsAsFactors=FALSE)

   for(i in 1:ncol(tbl.rowmd)) {
       list.cg = RClustergrammer:::.addRowMetaData(list.cg, tbl.rowmd, i)
       }

   checkTrue(is.list(list.cg))
   checkEquals(ncol(tbl.rowmd), ncol(list.cg$row_nodes) - 3) # 3 is number of original columns; name, clust, group
   checkEquals(list.cg$row_nodes$name, c("R3", "R1", "R2"))
   checkEquals(list.cg$row_nodes$clust, c(1,2,3))
   checkEquals(list.cg$row_nodes$"cat-0", c("Placement: Third", "Placement: First", "Placement: Second"))
   checkEquals(list.cg$row_nodes$"cat-1", c("Shape: Circle", "Shape: Triangle", "Shape: Square"))

} # test_addRowMetaData
#--------------------------------------------------------------------------------
test_addColumnMetaData <- function()
{
    printf("--- test_addColumnMetaData")

    mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
    list.cg <- RClustergrammer:::.matrixToClustergrammerList(mtx)

    tbl.colmd <- data.frame(row.names=c("C1","C2","C3"),
                            Placement=c("One", "Two", "Three"),
                            Colors=c("Red", "Green", "Blue"),
                            Test=c("w", "x", "y"),
                            stringsAsFactors=FALSE)

    for(i in 1:ncol(tbl.colmd)) {
        list.cg = RClustergrammer:::.addColumnMetaData(list.cg, tbl.colmd, i)
        }

    checkTrue(is.list(list.cg))
    checkEquals((ncol(tbl.colmd)), ncol(list.cg$col_nodes) - 3) # 3 is number of original groups; name, clust, group
    checkEquals(list.cg$col_nodes$name, c("C3", "C1", "C2"))
    checkEquals(list.cg$col_nodes$clust, c(1,2,3))
    checkEquals(list.cg$col_nodes$"cat-0", c("Placement: Three", "Placement: One", "Placement: Two"))
    checkEquals(list.cg$col_nodes$"cat-1", c("Colors: Blue", "Colors: Red", "Colors: Green"))
    checkEquals(list.cg$col_nodes$"cat-2", c("Test: y", "Test: w", "Test: x"))

} # test_addColumnMetaData
#--------------------------------------------------------------------------------
test_smallMatrixToListWithMetaData <- function()
{
    printf("--- test_smallMatrixToListWithMetaData")

    mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
    tbl.rowmd <- data.frame(row.names=c("R1","R2","R3"),
                            Placement=c("First", "Second", "Third"),
                            Shape=c("Triangle", "Square", "Circle"),
                            stringsAsFactors=FALSE)
    tbl.colmd <- data.frame(row.names=c("C1","C2","C3"),
                            Placement=c("One", "Two", "Three"),
                            Color=c("Red", "Green", "Blue"),
                            Test=c("w", "x", "y"),
                            stringsAsFactors=FALSE)

    list.cg <- RClustergrammer:::.matrixToListWithMetaData(mtx, tbl.rowmd, tbl.colmd)

    checkTrue(is.list(list.cg))

    checkEquals(ncol(tbl.rowmd), ncol(list.cg$row_nodes) - 3)
    checkEquals(list.cg$row_nodes$name, c("R3", "R1", "R2"))
    checkEquals(list.cg$row_nodes$clust, c(1,2,3))
    checkEquals(list.cg$row_nodes$"cat-0", c("Placement: Third", "Placement: First", "Placement: Second"))
    checkEquals(list.cg$row_nodes$"cat-1", c("Shape: Circle", "Shape: Triangle", "Shape: Square"))

    checkEquals((ncol(tbl.colmd)), ncol(list.cg$col_nodes) - 3)
    checkEquals(list.cg$col_nodes$name, c("C3", "C1", "C2"))
    checkEquals(list.cg$col_nodes$clust, c(1,2,3))
    checkEquals(list.cg$col_nodes$"cat-0", c("Placement: Three", "Placement: One", "Placement: Two"))
    checkEquals(list.cg$col_nodes$"cat-1", c("Color: Blue", "Color: Red", "Color: Green"))
    checkEquals(list.cg$col_nodes$"cat-2", c("Test: y", "Test: w", "Test: x"))

} # test_smallMatrixToListMetaData
#--------------------------------------------------------------------------------
test_mediumMatrixToListWithMetaData <- function()
{
    printf("--- test_mediumMatrixToListWithMetaData")

    set.seed(37)
    rows.of.interest <- sample(1:nrow(USArrests), size=3)
    mtx <- as.matrix(USArrests[rows.of.interest,]) #small sample from USArrests dataset

    tbl.rowmd <- data.frame(row.names=c("Nevada", "Arkansas", "New York"),
                            "Region"=c("South West", "South", "North East"),
                            stringsAsFactors=FALSE)
    tbl.colmd <- data.frame(row.names=c("Murder", "Assault", "UrbanPop", "Rape"),
                            Severity=c("Terrible", "Pretty Bad", "N/A", "Terrible"),
                            "Jail Time"=c("20 Years", "1-5 Years", "N/A", "5-10 Years"),
                            stringsAsFactors=FALSE)

    list.cg <- RClustergrammer:::.matrixToListWithMetaData(mtx, tbl.rowmd, tbl.colmd)

    checkTrue(is.list(list.cg))

    checkEquals(ncol(tbl.rowmd), ncol(list.cg$row_nodes) - 3) # 3 is number of original columns; name, clust, group
    checkEquals(list.cg$row_nodes$name, c("Arkansas", "Nevada", "New York"))
    checkEquals(list.cg$row_nodes$clust, c(1,2,3))
    checkEquals(list.cg$row_nodes$"cat-0", c("Region: South", "Region: South West", "Region: North East"))

    checkEquals(list.cg$col_nodes$name, c("Assault", "UrbanPop", "Murder", "Rape"))
    checkEquals(list.cg$col_nodes$clust, c(1,2,3,4))
    checkEquals(list.cg$col_nodes$"cat-0", c("Severity: Pretty Bad", "Severity: N/A", "Severity: Terrible", "Severity: Terrible"))
    checkEquals(list.cg$col_nodes$"cat-1", c("Jail.Time: 1-5 Years", "Jail.Time: N/A", "Jail.Time: 20 Years", "Jail.Time: 5-10 Years"))

} # test_mediumMatrixToListWithMetaData
#--------------------------------------------------------------------------------
test_largeMatrixToListWithMetaData <- function()
{
    printf("=== test_largeMatrixToListWithMetaData")

    mtx <- as.matrix(USArrests) #full USArrests dataseto
    tbl.colmd <- data.frame(row.names=c("Murder", "Assault", "UrbanPop", "Rape"),
                            Severity=c("Terrible", "Pretty Bad", "N/A", "Terrible"),
                            "Jail Time"=c("20 Years", "1-5 Years", "N/A", "5-10 Years"),
                            stringsAsFactors=FALSE)

    list.cg <- RClustergrammer:::.matrixToListWithMetaData(mtx, tbl.rowmd=data.frame(), tbl.colmd)

    checkTrue(is.list(list.cg))

    checkEquals(length(list.cg$row_nodes$name), 50)

    checkEquals(list.cg$col_nodes$name, c("Assault", "UrbanPop", "Murder", "Rape"))
    checkEquals(list.cg$col_nodes$clust, c(1,2,3,4))
    checkEquals(list.cg$col_nodes$"cat-0", c("Severity: Pretty Bad", "Severity: N/A", "Severity: Terrible", "Severity: Terrible"))
    checkEquals(list.cg$col_nodes$"cat-1", c("Jail.Time: 1-5 Years", "Jail.Time: N/A", "Jail.Time: 20 Years", "Jail.Time: 5-10 Years"))

} # test_largeMatrixToListWithMetaData
#--------------------------------------------------------------------------------
test_matrixToClustergrammerList <- function()
{
    printf("=== test_matrixToClustergrammerList")

    mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
    list.cg <- RClustergrammer:::.matrixToClustergrammerList(mtx)

    checkTrue(is.list(list.cg))
    checkEquals(sort(names(list.cg)), c("col_nodes", "mat", "row_nodes"))

      # call hclust directly to see what the order of rows and columns SHOULD be
      # and thus set us up for checking the contents of the full, returned list

    hc.rows <- hclust(dist(mtx))
    hc.cols <- hclust(dist(t(mtx)))

    checkEquals((list.cg$row_nodes$name), rownames(mtx[hc.rows$order, hc.cols$order]))
    checkEquals((list.cg$col_nodes$name), colnames(mtx[hc.rows$order, hc.cols$order]))

} # test_matrixToClustergrammerList
#--------------------------------------------------------------------------------
#test_addRowMetaData <- function() {
#
#    printf("=== test_addRowMetaData")
#
#    mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
#    list.cg <- matrixToClustergrammerList(mtx)
#    tbl.rowmd <- data.frame(row.names=c("R1","R2","R3"),
#                            Placement=c("First", "Second", "Third"),
#                            Shape=c("Triangle", "Square", "Circle"),
#                            stringsAsFactors=FALSE)
#
#    for(i in 1:ncol(tbl.rowmd)) {
#        list.cg = addRowMetaData(list.cg, tbl.rowmd, i)
#        }
#
#    checkTrue(is.list(list.cg))
#    checkEquals(ncol(tbl.rowmd), ncol(list.cg$row_nodes) - 3) # 3 is number of original columns; name, clust, group
#    checkEquals(list.cg$row_nodes$name, c("R3", "R1", "R2"))
#    checkEquals(list.cg$row_nodes$clust, c(1,2,3))
#    checkEquals(list.cg$row_nodes$"cat-0", c("Placement: Third", "Placement: First", "Placement: Second"))
#    checkEquals(list.cg$row_nodes$"cat-1", c("Shape: Circle", "Shape: Triangle", "Shape: Square"))
#
#    }#test_addRowMetaData
##--------------------------------------------------------------------------------
#
#
#
#
