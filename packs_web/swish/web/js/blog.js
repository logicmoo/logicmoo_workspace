$(function() {
  $(".blog-tag").on("click", function(ev) {
    var el  = $(ev.target).closest(".blog-tag");
    var tag = el.data("tag");
    var set = [];

    el.toggleClass("active");
    el.parent().children(".active").each(function() {
      set.push($(this).data("tag"));
    });

    if ( set.length > 0 ) {
      el.closest(".contents.blog").find(".blog-index-entry").each(function() {
	var mytags = $(this).data("tags").split("|");

	if ( set.filter(value => mytags.includes(value)).length > 0 )
	  $(this).removeClass("filtered-out");
	else
	  $(this).addClass("filtered-out");
      });
    } else {
      el.closest(".contents.blog")
        .find(".blog-index-entry")
        .removeClass("filtered-out");
    }
  });
  $(".blog-index-entry").on("click", function(ev) {
  });
});
