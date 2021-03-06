rcgJavascriptFile <- system.file(package="RClustergrammer", "browserCode", "dist", "rclustergrammer.html")
#------------------------------------------------------------------------------------------------------------------------
.RClustergrammer <- setClass ("RClustergrammer",
                              representation = representation(
                                    state="environment"),
                              contains = "BrowserVizClass",
                              prototype = prototype (uri="http://localhost", 9200)
                              )
#------------------------------------------------------------------------------------------------------------------------
setGeneric("getMatrix",          signature="obj", function(obj, matrix) standardGeneric("getMatrix"))
setGeneric("getRowMetadata",     signature="obj", function(obj) standardGeneric("getRowMetadata"))
setGeneric("getColumnMetadata",  signature="obj", function(obj) standardGeneric("getColumnMetadata"))
setGeneric("clusterAndDisplay",  signature="obj", function(obj, method, matrix, rowMetadata=data.frame(), columnMetadata=data.frame())
                                   standardGeneric("clusterAndDisplay"))
setGeneric("getRowNames",        signature="obj", function(obj) standardGeneric("getRowNames"))
setGeneric("getColumnNames",     signature="obj", function(obj) standardGeneric("getColumnNames"))
setGeneric("selectRows",         signature="obj", function(obj, rowNames) standardGeneric("selectRows"))
setGeneric("selectColumns",      signature="obj", function(obj, columnNames) standardGeneric("selectColumns"))
setGeneric("selectRowsAndColumns", signature="obj", function(obj, rowNames, columnNames) standardGeneric("selectRowsAndColumns"))
setGeneric("showAll",            signature="obj", function(obj, columnNames) standardGeneric("showAll"))
setGeneric("ping",               signature="obj", function(obj) standardGeneric ("ping"))
#------------------------------------------------------------------------------------------------------------------------
# constructor
RClustergrammer = function(portRange, host="localhost", title="RClustergrammer",
#                           mtx=matrix(), rowMetadata=data.frame(), columnMetadata=data.frame(),
                           quiet=TRUE)
{
   state <- new.env()
   state$mtx <- matrix()
   state$rowMetadata <- data.frame()
   state$columnMetadata <- data.frame()
   .RClustergrammer(BrowserViz(portRange, host, title, quiet, browserFile=rcgJavascriptFile,
                               httpQueryProcessingFunction=myQP),
                    state=state)

} # RClustergrammer: constructor
#------------------------------------------------------------------------------------------------------------------------
setMethod("getMatrix", "RClustergrammer",

    function(obj) {
       obj@state$mtx
       })

#------------------------------------------------------------------------------------------------------------------------
setMethod("getRowMetadata", "RClustergrammer",

    function(obj) {
       obj@state$rowMetadata
       })

#------------------------------------------------------------------------------------------------------------------------
setMethod("getColumnMetadata", "RClustergrammer",

    function(obj) {
       obj@state$columnMetadata
       })

#------------------------------------------------------------------------------------------------------------------------
setMethod("clusterAndDisplay", "RClustergrammer",

   function(obj, method, matrix, rowMetadata=data.frame(), columnMetadata=data.frame()) {
     stopifnot(method == "hclust")  # TODO stopgap.  biclust, k-means to be added
     cg.list <- .matrixToListWithMetaData(matrix, rowMetadata, columnMetadata)
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
        if(!obj@quiet) message(sprintf("ping waiting for browser response"));
        Sys.sleep(.1)
        }
     getBrowserResponse(obj)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod("getRowNames", "RClustergrammer",

   function(obj) {

     send(obj, list(cmd="getRowNames", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        if(!obj@quiet) message(sprintf("getRowNames waiting for browser response"));
        Sys.sleep(.1)
        }
     getBrowserResponse(obj)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod("getColumnNames", "RClustergrammer",

   function(obj) {

     send(obj, list(cmd="getColumnNames", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        if(!obj@quiet) message(sprintf("getColumnNames waiting for browser response"));
        Sys.sleep(.1)
        }
     getBrowserResponse(obj)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod("selectRows", "RClustergrammer",

   function(obj, rowNames) {

     send(obj, list(cmd="selectRows", callback="handleResponse", status="request", payload=rowNames))
     while (!browserResponseReady(obj)){
        if(!obj@quiet) message(sprintf("selectRows waiting for browser response"));
        Sys.sleep(.1)
        }
     getBrowserResponse(obj)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod("selectColumns", "RClustergrammer",

   function(obj, columnNames) {

     send(obj, list(cmd="selectColumns", callback="handleResponse", status="request", payload=columnNames))
     while (!browserResponseReady(obj)){
        if(!obj@quiet) message(sprintf("selectColumns waiting for browser response"));
        Sys.sleep(.1)
        }
     getBrowserResponse(obj)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod("selectRowsAndColumns", "RClustergrammer",

   function(obj, rowNames, columnNames) {

     payload = list(rows=rowNames, cols=columnNames)
     send(obj, list(cmd="selectRowsAndColumns", callback="handleResponse", status="request", payload=payload))
     while (!browserResponseReady(obj)){
        if(!obj@quiet) message(sprintf("selectRows waiting for browser response"));
        Sys.sleep(.1)
        }
     getBrowserResponse(obj)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod("showAll", "RClustergrammer",

   function(obj) {

     send(obj, list(cmd="showAll", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        if(!obj@quiet) message(sprintf("selectColumns waiting for browser response"));
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
