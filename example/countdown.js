var slideSpeed = 400;

function transitionStart(idx, elm) {
  TransitionEvent.onTransitionStart(idx, elm);
}
function transitionEnd(idx, elm) {
  TransitionEvent.onTransitionEnd(idx, elm);
}
function right() {
  mySwipe.next();
}
function left() {
  mySwipe.prev();
}
function jumpToBeginning() {
  var current = mySwipe.getPos();
  var t = mySwipe.getNumSlides();
  window.console.log("current pos is " + current + " of " + t);
  mySwipe.slide(0, slideSpeed);
}
function jumpToComputeFrame() {
  var current = mySwipe.getPos();
  var t = mySwipe.getNumSlides();
  window.console.log("jumpToComputeFrame: current pos is " + current + " of " + t);
  mySwipe.slide(1, slideSpeed);
}
function jumpToSolutionFrame() {
  var current = mySwipe.getPos();
  var t = mySwipe.getNumSlides();
  window.console.log("jumpToSolutionFrame : current pos is " + current + " of " + t);
  mySwipe.slide(2, slideSpeed);
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
