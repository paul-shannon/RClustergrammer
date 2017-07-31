rcgJavascriptFile <- system.file(package="RClustergrammer", "scripts", "rcg.html")
#------------------------------------------------------------------------------------------------------------------------
.RClustergrammer <- setClass ("RClustergrammer",
                              representation = representation (mtx="matrix"),
                              contains = "BrowserVizClass",
                              prototype = prototype (uri="http://localhost", 9200)
                              )
#------------------------------------------------------------------------------------------------------------------------
setGeneric('displayHeatmap',  signature='obj', function(obj) standardGeneric ("displayHeatmap"))
#------------------------------------------------------------------------------------------------------------------------
# constructor
RClustergrammer = function(matrix=matrix, portRange, host="localhost", title="RClustergrammer", quiet=TRUE)
{
  .RClustergrammer(BrowserViz(portRange, host, title, quiet, browserFile=rcgJavascriptFile), mtx=matrix)

} # RClustergrammer: constructor
#------------------------------------------------------------------------------------------------------------------------
setMethod('displayHeatmap', 'RClustergrammer',

   function(obj) {

     send(obj, list(cmd="displayHeatmap", callback="handleResponse", status="request",
                    payload=list(x=x, y=y, xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax)))
     while (!browserResponseReady(obj)){
        if(!obj@quiet) message(sprintf("plot waiting for browser response"));
        Sys.sleep(.1)
        }
     getBrowserResponse(obj)
     })

#------------------------------------------------------------------------------------------------------------------------


