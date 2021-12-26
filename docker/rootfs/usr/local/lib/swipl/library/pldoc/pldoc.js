/*  This file is  part  of  the   SWI-Prolog  PlDoc  package  for online
    documentation browsing. It defines JavaScript to issue HTTP requests
    on the Prolog server that do not create a new page and handling
    footnotes.

Author:  Jan Wielemaker & Michiel Hildebrand
Copying: Public domain
*/

function HTTPrequest(url)
{ $.get(url,
	{
	});
}

/* Improve footnote interaction.  Contributed by Anne Ogborn.
*/

$(function(){
	var footnoteactivator = $('.fn');
	footnoteactivator.mouseenter(function() {
		window.clearTimeout(this.footnoteid);
		var fn = $(this).find('span.fn-text');
		if ( fn ) {
		  fn.removeClass('fn-text');
		  fn.addClass('fnp');
		}
		$(this).find('span.fnp').show(100);
	});
	footnoteactivator.mouseleave(function() {
		var t = $(this).find('span.fnp');
		if(this.footnoteid !== null)
		{
		    window.clearTimeout(this.footnoteid);
		}

		this.footnoteid = window.setTimeout(

			function() {
				  t.hide(100);
			}, 2000);
	});
});
