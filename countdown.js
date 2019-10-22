var slideSpeed = 600;

function transitionStart(idx, elm) {
  TransitionEvent.onTransitionStart(idx, elm);
}
function transitionEnd(idx, elm) {
  TransitionEvent.onTransitionEnd(idx, elm);
}
function jumpToBeginning() {
  mySwipe.slide(0, slideSpeed);
}
function jumpToConfig() {
  mySwipe.slide(1, slideSpeed);
}
function jumpToComputeFrame() {
  mySwipe.slide(2, slideSpeed);
}
function jumpToSolutionFrame() {
  mySwipe.slide(3, slideSpeed);
}
function clearGraph() {
      springy.graph.nodes = [];
      springy.graph.edges = [];
}
function init(id) {
  window.mySwipe = new Swipe(document.getElementById(id), {
      startSlide: 0,
      speed: slideSpeed,
      auto: 0,
      continuous: false,
      disableScroll: false,
      stopPropagation: false,
      callback: transitionStart,
      transitionEnd: transitionEnd
    });

    CountdownPage.render('config', 'compute', 'scriptContainer', 'layout');
}
