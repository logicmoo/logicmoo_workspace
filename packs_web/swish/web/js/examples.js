$(function() {
  $(".ex-header").on("click", function(ev) {
    var ex = $(ev.target).closest(".ex");
    if ( ex.hasClass("ex-current") ) {
      ex.removeClass("ex-current");
    } else {
      ex.parent().children().removeClass("ex-current");
      ex.addClass("ex-current");
    }
    ev.stopPropagation();
  });
});
