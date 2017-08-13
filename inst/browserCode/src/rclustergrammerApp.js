import css from './css/bootstrap.css';
import css from './css/custom.css';
import css from './css/custom_scrolling.css';
import css from './css/font-awesome.min.css';

//var demoMatrix = require("./demoData/threebyThree.json");

var ClustergrammerModule = (function(){
//------------------------------------------------------------------------------------------------------------------------
function setHub(newHub)
{
   hub = newHub;

} // setHub
//------------------------------------------------------------------------------------------------------------------------
function addMessageHandlers()
{
   hub.addMessageHandler("ping",                    ping);
   hub.addMessageHandler("displayClusteredMatrix",  displayClusteredMatrix);
   hub.addMessageHandler("getRowNames",             getRowNames);
   hub.addMessageHandler("getColumnNames",          getColumnNames);
   hub.addMessageHandler("selectRows",              selectRows);
   hub.addMessageHandler("selectColumns",           selectColumns);
   hub.addMessageHandler("selectRowsAndColumns",    selectRowsAndColumns);
   hub.addMessageHandler("showAll",                 showAll);

} // addMessageHandlers
//------------------------------------------------------------------------------------------------------------------------
function ping(msg)
{
   console.log("--- ping")
   hub.send({cmd: msg.callback, status: "success", callback: "", payload: "pong"});

} // setMatrix
//------------------------------------------------------------------------------------------------------------------------
function displayClusteredMatrix(msg)
{
   console.log("--- displayClusteredMatrix")
   console.log(msg);
   var filename = msg.payload;

   var s = window.location.href + "?" + filename;

   fetch(s)
      .then(function(responseObj){
          console.log("fetch in action");
          console.log(responseObj);
          return responseObj.json();
          })
     .then(function(matrixObject){
         console.log("successfully fetched " + filename);
         console.log(matrixObject)
         displayMatrix(matrixObject)
         return "success";
         });

   hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});

} // displayClusteredMatrix
//------------------------------------------------------------------------------------------------------------------------
function displayMatrix(matrixObject)
{
   var about_string = 'Zoom, scroll, and click buttons to interact with the clustergram. ' +
                      '<a href="http://amp.pharm.mssm.edu/clustergrammer/help">' +
		      '<i class="fa fa-question-circle" aria-hidden="true"></i> </a>';

   var args = {
      root: '#cgDiv',
      'network_data': matrixObject,
      'about': about_string,
      'sidebar_width':150,
      };

   var screen_width = window.innerWidth;
   var screen_height = window.innerHeight - 20;
   $("#cgDiv").width(screen_width);
   $("#cgDiv").height(screen_height);

   $("#cgDiv").empty()

   window.cgm = Clustergrammer(args);

   console.log('loading clustergrammer')

} // displayMatrix
//------------------------------------------------------------------------------------------------------------------------
function getRowNames(msg)
{
   console.log("--- getRowNames")
   console.log(msg);
   var namesOfVisibleRows = cgm.params.network_data.row_nodes_names;

   hub.send({cmd: msg.callback, status: "success", callback: "", payload: namesOfVisibleRows});

} // getRowNames
//------------------------------------------------------------------------------------------------------------------------
function getColumnNames(msg)
{
   console.log("--- getColumnNames")
   var namesOfVisibleColumns = cgm.params.network_data.col_nodes_names;

   hub.send({cmd: msg.callback, status: "success", callback: "", payload: namesOfVisibleColumns});

} // getColumnNames
//------------------------------------------------------------------------------------------------------------------------
function selectRows(msg)
{
   console.log("--- selectRows")
   var rowNames = msg.payload;
   var currentColumnNames = cgm.params.network_data.col_nodes_names;
   cgm.filter_viz_using_names({row: rowNames});
   //cgm.filter_viz_using_names({row: rowNames, col: currentColumnNames});

   hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});

} // selectRows
//------------------------------------------------------------------------------------------------------------------------
function selectColumns(msg)
{
   console.log("--- selectColumns")
   var columnNames = msg.payload;
   var currentRowNames = cgm.params.network_data.row_nodes_names;
   cgm.filter_viz_using_names({col: columnNames});
   //cgm.filter_viz_using_names({row: currentRowNames, col: columnNames});

   hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});

} // selectRows
//------------------------------------------------------------------------------------------------------------------------
function selectRowsAndColumns(msg)
{
   console.log("--- selectRowsAndColumns")
   var rowNames = msg.payload.rows;
   var columnNames = msg.payload.cols;
   cgm.filter_viz_using_names({row: rowNames, col: columnNames});
   //cgm.filter_viz_using_names({row: rowNames, col: currentColumnNames});

   hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});

} // selectRows
//------------------------------------------------------------------------------------------------------------------------
function showAll(msg)
{
   console.log("--- showAll");
   cgm.filter_viz_using_names([]);

   hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});

} // selectRows
//------------------------------------------------------------------------------------------------------------------------
function initializeClusterGrammerUI()
{
   window.cwg = Clustergrammer;

} // initializeClusterGrammerUI
//------------------------------------------------------------------------------------------------------------------------
return{
   setHub: setHub,
   addMessageHandlers: addMessageHandlers,
   initializeUI: initializeClusterGrammerUI,
   };
//------------------------------------------------------------------------------------------------------------------------
}); // ClustergrammerModule

var hub = require("browservizjs")
var cg = ClustergrammerModule();
cg.setHub(hub)
hub.init();

cg.addMessageHandlers()
var bound_initializeUI = cg.initializeUI.bind(cg);
hub.addOnDocumentReadyFunction(bound_initializeUI);
hub.start();



