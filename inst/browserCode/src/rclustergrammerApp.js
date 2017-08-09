import css from './css/bootstrap.css';
import css from './css/custom.css';
import css from './css/custom_scrolling.css';
import css from './css/font-awesome.min.css';

var demoMatrix = require("./demoData/threebyThree.json");

var ClustergrammerModule = (function(){
//------------------------------------------------------------------------------------------------------------------------
function setHub(newHub)
{
   hub = newHub;

} // setHub
//------------------------------------------------------------------------------------------------------------------------
function addMessageHandlers()
{
   hub.addMessageHandler("ping",       ping)
   hub.addMessageHandler("setMatrix",  setMatrix)

} // addMessageHandlers
//------------------------------------------------------------------------------------------------------------------------
function ping(msg)
{
   console.log("--- ping")
   hub.send({cmd: msg.callback, status: "success", callback: "", payload: "pong"});

} // setMatrix
//------------------------------------------------------------------------------------------------------------------------
function setMatrix(msg)
{
   console.log("--- setMatrix")
   hub.send({cmd: msg.callback, status: "success", callback: "", payload: ""});

} // setMatrix
//------------------------------------------------------------------------------------------------------------------------
function initializeClusterGrammerUI()
{
   window.cwg = Clustergrammer;
   var about_string = 'Zoom, scroll, and click buttons to interact with the clustergram. ' +
                      '<a href="http://amp.pharm.mssm.edu/clustergrammer/help">' +
		      '<i class="fa fa-question-circle" aria-hidden="true"></i> </a>';
   function make_clust(network_data){
          var args = {
            root: '#cgDiv',
            'network_data': network_data,
            'about': about_string,
            'sidebar_width':150,
            };
         var screen_width = window.innerWidth;
         var screen_height = window.innerHeight - 20;
         $("#cgDiv").width(screen_width);
         $("#cgDiv").height(screen_height);
         window.cgm = Clustergrammer(args);
         $("#cgDiv .wait_message").remove()
         console.log('loading clustergrammer')
    }; // make_clust

  setTimeout(function(){make_clust(demoMatrix);}, 0);

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



