function doit() {
    var graph = new Springy.Graph();

    jQuery(function(){
      var springy = window.springy = jQuery('#layout').springy({
        graph: graph,
        nodeSelected: function(node){
          console.log('Node selected: ' + JSON.stringify(node.data));
        }
      });
    });
}