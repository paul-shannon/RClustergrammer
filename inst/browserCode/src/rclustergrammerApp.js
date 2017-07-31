import css from './css/bootstrap.css';
import css from './css/custom.css';
import css from './css/custom_scrolling.css';
import css from './css/font-awesome.min.css';

var demoMatrix = require("./demoData/threebyThree.json");

//------------------------------------------------------------------------------------------------------------------------
$(document).ready(function() {
   window.cwg = Clustergrammer;
    var about_string = 'Zoom, scroll, and click buttons to interact with the clustergram. <a href="http://amp.pharm.mssm.edu/clustergrammer/help"> <i class="fa fa-question-circle" aria-hidden="true"></i> </a>';
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

   console.log("document ready, starting ~/github/RClustergrammer/inst/browserCode/src/app.js")
   make_clust(demoMatrix);

   }); // document ready
//------------------------------------------------------------------------------------------------------------------------

