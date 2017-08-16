#------------------------------------------------------------------------------------------------------------------------
.ClusteringTools <- setClass ("ClusteringTools",
                              representation = representation(
                                 matrix="matrix",
                                 rowMetadata="data.frame",
                                 columnMetadata="data.frame"
                                 )
                              )
#------------------------------------------------------------------------------------------------------------------------
setGeneric('hierarchicalCluster',  signature='obj', function(obj) standardGeneric ("hierarchicalCluster"))
#------------------------------------------------------------------------------------------------------------------------
# constructor
ClusteringTools = function(matrix, rowMetadata=data.frame(), columnMetadata=data.frame())
{
  .ClusteringTools(matrix=matrix, rowMetadata=rowMetadata, columnMetadata=columnMetadata)

} # RClustergrammer: constructor
#------------------------------------------------------------------------------------------------------------------------
setMethod('hierarchicalCluster', 'ClusteringTools',

   function(obj) {
     .matrixToListWithMetaData(obj@matrix, obj@rowMetadata, obj@columnMetadata)
     })

#------------------------------------------------------------------------------------------------------------------------
.matrixToListWithMetaData <- function(mtx, tbl.rowmd=data.frame(), tbl.colmd=data.frame(), row.branches=NA, col.branches=NA)
{
  list.cg <- .matrixToClustergrammerList(mtx, row.branches, col.branches)

  if(nrow(tbl.rowmd)) {
      for(i in 1:ncol(tbl.rowmd)) {
         list.cg <- .addRowMetaData(list.cg, tbl.rowmd, i)
         }#forloop
      }#row meta data

  if(nrow(tbl.colmd)) {
      for(i in 1:ncol(tbl.colmd)) {
          list.cg <- .addColumnMetaData(list.cg, tbl.colmd, i)
          }#forloop
      }#column meta data

  return(list.cg)

} # matrixToListWithMetaData
#--------------------------------------------------------------------------------
.matrixToClustergrammerList <- function(mtx, row.branches=NA, col.branches=NA)
{

    # the clustergrammer widget (at present - 15 aug 2017) allows only 11
    # levels of tree branching.  there should always be n-1 branches [is this true?]

   hc.rows <- hclust(dist(mtx))
   hc.cols <- hclust(dist(t(mtx)))

   clustergrammerMaxBranches <- 11

   rowBranchCount <- nrow(mtx)# - 1
   colBranchCount <- ncol(mtx)# - 1

   if(is.na(row.branches)){
      if(rowBranchCount <= 11){
         row.branches <- seq_len(rowBranchCount)
         }
      else{
         row.branches <- as.integer(seq(1, rowBranchCount, length=clustergrammerMaxBranches))
         }
      } # if row.branches not provided

   if(is.na(col.branches)){
      if(colBranchCount <= 11){
         col.branches <- seq_len(colBranchCount)
        }
     else{
         col.branches <- as.integer(seq(1, colBranchCount, length=clustergrammerMaxBranches))
        }
      } # if col.branches not provide


   #r.kIndices <- as.integer(seq(1, nrow(mtx), length=kMax))
   #c.kIndices <- as.integer(seq(1, ncol(mtx), length=kMax))
   #r.kIndices <- tail(seq_len(nrow(mtx)), n=10)
   #c.kIndices <- tail(seq_len(ncol(mtx)), n=10)

      # cutree docs say k is "an integer scalar or vector with the desired number of groups"

   treeMtx.rows <- cutree(hc.rows, k=row.branches)
   treeMtx.cols <- cutree(hc.cols, k=col.branches)

   #treeMtx.rows <- cutree(hc.rows, k=11)
   #treeMtx.cols <- cutree(hc.cols, k=11)
   #treeMtx.rows <- cutree(hc.rows, k=1:r)
   #treeMtx.cols <- cutree(hc.cols, k=1:c)

   rowname <- hc.rows$labels[hc.rows$order]
   rowclust <- 1:nrow(treeMtx.rows)

   #rowrank <- hc.rows$order

   row_nodes <- data.frame(name=rowname,
                           clust=rowclust,
 		 	    #rank=rowrank,
			    group=.rowFill(treeMtx.rows, hc.rows), #function that fills list called rowgroup with rows from treeMtx.rows
                           stringsAsFactors=FALSE,
                           check.names=FALSE)

   colname <- hc.cols$labels[hc.cols$order]
   colclust <- 1:nrow(treeMtx.cols)
   #colrank <- hc.cols$order

   col_nodes <- data.frame(name=colname,
                           clust=colclust,
           		    #rank=colrank,
                            group=.colFill(treeMtx.cols, hc.cols), #function that fills list called colgroup with rows from treeMtx.cols
                            stringsAsFactors=FALSE,
                            check.names=FALSE)

   mat <- unname(mtx, force=FALSE)
   mat <- mat[hc.rows$order, hc.cols$order]

   list.cg <- list(row_nodes=row_nodes, col_nodes=col_nodes, mat=mat)
   return(list.cg)

} # matrixToClustergrammerList
#--------------------------------------------------------------------------------
.rowFill <- function(treeMtx.rows, hc.rows) {

              rowgroup <- list()
              for(i in 1:nrow(treeMtx.rows)) {
                 rowgroup[[i]] <- rev(treeMtx.rows[hc.rows$order[i],])
                 }
              return(I(rowgroup))

              }#row names
#--------------------------------------------------------------------------------
.colFill <- function(treeMtx.cols, hc.cols) {

     colgroup <- list()
     for(i in 1:nrow(treeMtx.cols)) {
         colgroup[[i]] <- rev(treeMtx.cols[hc.cols$order[i],])
         }

    return(I(colgroup))

} # column names
#--------------------------------------------------------------------------------
.addColumnMetaData <- function(list.cg, tbl.colmd, c) {

    desired.order <- match(list.cg$col_nodes$name, rownames(tbl.colmd))
    tbl.colmd <- tbl.colmd[desired.order,,drop=FALSE]

    category.name <- sprintf("cat-%d", c-1)
    list.cg$col_nodes[category.name] <- paste(sep='',
                                              colnames(tbl.colmd)[c],
                                              ': ',
                                              tbl.colmd[,c])

    return(list.cg)

} # .addColumnMetaData
#--------------------------------------------------------------------------------
.addRowMetaData <- function(list.cg, tbl.rowmd, r) {

    desired.order <- match(list.cg$row_nodes$name, rownames(tbl.rowmd))
    tbl.rowmd <- tbl.rowmd[desired.order,,drop=FALSE]

    category.name <- sprintf("cat-%d", r-1)
    list.cg$row_nodes[category.name] <- paste(sep='',
                                              colnames(tbl.rowmd)[r],
                                              ': ',
                                              tbl.rowmd[,r])

    return(list.cg)

} # .addRowMetaData
#--------------------------------------------------------------------------------
