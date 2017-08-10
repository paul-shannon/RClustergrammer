rcgJavascriptFile <- system.file(package="RClustergrammer", "browserCode", "dist", "rclustergrammer.html")
#------------------------------------------------------------------------------------------------------------------------
.RClustergrammer <- setClass ("RClustergrammer",
                              representation = representation(
                                    state="environment"),
                              contains = "BrowserVizClass",
                              prototype = prototype (uri="http://localhost", 9200)
                              )
#------------------------------------------------------------------------------------------------------------------------
setGeneric("setMatrix",          signature="obj", function(obj, mtx) standardGeneric ("setMatrix"))
setGeneric("setRowMetadata",     signature="obj", function(obj, tbl.md) standardGeneric ("setRowMetadata"))
setGeneric("setColumnMetadata",  signature="obj", function(obj, tbl.md) standardGeneric ("setColumnMetadata"))
setGeneric("getMatrix",          signature="obj", function(obj, matrix) standardGeneric ("getMatrix"))
setGeneric("getRowMetadata",     signature="obj", function(obj) standardGeneric ("getRowMetadata"))
setGeneric("getColumnMetadata",  signature="obj", function(obj) standardGeneric ("getColumnMetadata"))
setGeneric("clusterAndDisplay",  signature="obj", function(obj, method) standardGeneric ("clusterAndDisplay"))
setGeneric("ping",               signature="obj", function(obj) standardGeneric ("ping"))
#------------------------------------------------------------------------------------------------------------------------
# constructor
RClustergrammer = function(portRange, host="localhost", title="RClustergrammer",
                           mtx=matrix(), rowMetadata=data.frame(), columnMetadata=data.frame(),
                           quiet=TRUE)
{
   state <- new.env()
   state$mtx <- mtx
   state$rowMetadata <- rowMetadata
   state$columnMetadata <- columnMetadata
   .RClustergrammer(BrowserViz(portRange, host, title, quiet, browserFile=rcgJavascriptFile,
                               httpQueryProcessingFunction=myQP),
                    state=state)

} # RClustergrammer: constructor
#------------------------------------------------------------------------------------------------------------------------
setMethod("setMatrix", "RClustergrammer",

    function(obj, mtx) {
       obj@state$mtx <- mtx
       })

#------------------------------------------------------------------------------------------------------------------------
setMethod("getMatrix", "RClustergrammer",

    function(obj) {
       obj@state$mtx
       })

#------------------------------------------------------------------------------------------------------------------------
setMethod("setRowMetadata", "RClustergrammer",

    function(obj, tbl.md) {
       if(nrow(getMatrix(obj)) == 0)
           stop("error in RClustergrammer::setRowMetadata: must set matrix before metadata")
       stopifnot(rownames(tbl.md) == rownames(getMatrix(obj)))
       obj@state$rowMetadata <- tbl.md
       })

#------------------------------------------------------------------------------------------------------------------------
setMethod("getRowMetadata", "RClustergrammer",

    function(obj) {
       obj@state$rowMetadata
       })

#------------------------------------------------------------------------------------------------------------------------
setMethod("setColumnMetadata", "RClustergrammer",

    function(obj, tbl.md) {
       if(ncol(getMatrix(obj)) == 0)
           stop("error in RClustergrammer::setRowMetadata: must set matrix before metadata")
       stopifnot(rownames(tbl.md) == colnames(getMatrix(obj)))
       obj@state$columnMetadata <- tbl.md
       })

#------------------------------------------------------------------------------------------------------------------------
setMethod("getColumnMetadata", "RClustergrammer",

    function(obj) {
       obj@state$columnMetadata
       })

#------------------------------------------------------------------------------------------------------------------------
setMethod("clusterAndDisplay", "RClustergrammer",

   function(obj, method) {
     stopifnot(method == "hclust")  # TODO stopgap.  biclust, k-means to be added
     cg.list <- .matrixToListWithMetaData(obj@state$mtx, obj@state$rowMetadata, obj@state$columnMetadata)
     cg.json <- toJSON(cg.list)
     filename <- "cg.json"
     write(cg.json, file=filename)
     send(obj, list(cmd="displayClusteredMatrix", callback="handleResponse", status="request", payload=filename))
     while (!browserResponseReady(obj)){
        Sys.sleep(.1)
        }
     printf("browserResponseReady")
     getBrowserResponse(obj);
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod("ping", "RClustergrammer",

   function(obj) {

     send(obj, list(cmd="ping", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        if(!obj@quiet) message(sprintf("plot waiting for browser response"));
        Sys.sleep(.1)
        }
     getBrowserResponse(obj)
     })

#------------------------------------------------------------------------------------------------------------------------
myQP <- function(queryString)
{
   printf("=== RClustergrammer::myQP");
   #print(queryString)
     # for reasons not quite clear, the query string comes in with extra characters
     # following the expected filename:
     #
     #  "?sampleStyle.js&_=1443650062946"
     #
     # check for that, cleanup the string, then see if the file can be found

   ampersand.loc <- as.integer(regexpr("&", queryString, fixed=TRUE))
   #printf("ampersand.loc: %d", ampersand.loc)

   if(ampersand.loc > 0){
      queryString <- substring(queryString, 1, ampersand.loc - 1);
      }

   questionMark.loc <- as.integer(regexpr("?", queryString, fixed=TRUE));
   #printf("questionMark.loc: %d", questionMark.loc)

   if(questionMark.loc == 1)
      queryString <- substring(queryString, 2, nchar(queryString))

   filename <- queryString;
   #printf("myQP filename: '%s'", filename)
   #printf("       exists?  %s", file.exists(filename));

   stopifnot(file.exists(filename))

   printf("--- about to scan %s", filename);
      # reconstitute linefeeds though collapsing file into one string, so json
      # structure is intact, and any "//" comment tokens only affect one line
   text <- paste(scan(filename, what=character(0), sep="\n", quiet=TRUE), collapse="\n")
   printf("%d chars read from %s", nchar(text), filename);

   return(text);

} # myQP
#----------------------------------------------------------------------------------------------------
