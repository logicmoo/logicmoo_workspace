/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2016, VU University Amsterdam
			      CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

define([ "jquery" ],
       function() {

// import 'jquery-contextmenu';

function unfold() {
	$(this).next().toggleClass('fold')
	//$(this).remove()
	$(this).toggleClass('fold')
}

var serverRoot = "https://logicmoo.org"
var xrefDir = "/swish/lm_xref"
var ctrlPressed = false;
var shiftPressed = false;
$(window).keydown(function(evt) {
  if (evt.which == 17) {
    ctrlPressed = true;
  }
  if (evt.which == 16) {
	shiftPressed = true;
  }
}).keyup(function(evt) {
  if (evt.which == 17) { // ctrl
    ctrlPressed = false;
  }
  if (evt.which == 16) {
	shiftPressed = false;
  }
});

// Begin hooking up a domNodeInserted
  var observers = [];

  $.event.special.domNodeInserted = {

    setup: function setup(data, namespaces) {
      var observer = new MutationObserver(checkObservers);

      observers.push([this, observer, []]);
    },

    teardown: function teardown(namespaces) {
      var obs = getObserverData(this);

      obs[1].disconnect();

      observers = $.grep(observers, function(item) {
        return item !== obs;
      });
    },

    remove: function remove(handleObj) {
      var obs = getObserverData(this);

      obs[2] = obs[2].filter(function(event) {
        return event[0] !== handleObj.selector && event[1] !== handleObj.handler;
      });
    },

    add: function add(handleObj) {
      var obs = getObserverData(this);

      var opts = $.extend({}, {
        childList: true,
        subtree: true
      }, handleObj.data);

      obs[1].observe(this, opts);

      obs[2].push([handleObj.selector, handleObj.handler]);
    }
  };

  function getObserverData(element) {
    var $el = $(element);

    return $.grep(observers, function(item) {
      return $el.is(item[0]);
    })[0];
  }

  function checkObservers(records, observer) {
    var obs = $.grep(observers, function(item) {
      return item[1] === observer;
    })[0];

    var triggers = obs[2];

    var changes = [];

    records.forEach(function(record) {
      if (record.type === 'attributes') {
        if (changes.indexOf(record.target) === -1) {
          changes.push(record.target);
        }

        return;
      }

      $(record.addedNodes).toArray().forEach(function(el) {
        if (changes.indexOf(el) === -1) {
          changes.push(el);
        }
      })
    });

    triggers.forEach(function checkTrigger(item) {
      changes.forEach(function(el) {
        var $el = $(el);

        if ($el.is(item[0])) {
          $el.trigger('domNodeInserted');
        }
      });
    });
  }
// End hooking up a domNodeInserted

function documentUpdated(why, inserted) {
      console.log("updating "+why + " " + inserted );
	  // @todo try to scope it better for $(inserted)
	  $("a").each(function(){ 
		updateLink($(this));
	  });
}

function updateLink(inserted) {
	if(true) return;
	var href = ""+ $(inserted).attr("href"); // Get current url
	//console.log("seeing "+href);
	var fa = href.indexOf("?fa=");
	if(fa>=0) {
		if(fa>0) {
		  href = href.substring(fa);
		}
		var newUrl = xrefDir + "/" + href;
		$(inserted).attr("href", newUrl); // Set herf value
		$(inserted).attr('target','lm_xref');
	}
}

function openLinkInShared(href) {
  // because we are leaving this window we will miss the keyUp events
  shiftPressed = false;
  ctrlPressed = false;

  var fa = href.indexOf("?fa=");
  if(fa>=0) {
	if(fa>0) {
	  href = href.substring(fa);
	}
	href = xrefDir + "/" + href;
  } 
  if(href.startsWith("/")) {
	href = serverRoot + href;
  }
  window.open(href, 'lm_xref');
}

$(document).on('click', 'a', function(e) {
	var a = $(this);
	updateLink(a);
	var href = ""+ $(a).attr("href");
	if(href=="undefined") {
		href = this.outerText;
		href = "?fa=" + href;
		$(a).attr("href",href);
	}
	
	if((href.indexOf("?fa=")>=0) || href.startsWith(xrefDir)) {

		e.stopPropagation();
		e.preventDefault();

		if(shiftPressed) {	
			openLinkInShared(href);
			return;
		}
		if(ctrlPressed) {	
			openLinkInShared(href);
			return;
		}
	}
});

$(document).on("domNodeInserted", "select", function () { documentUpdated("domNodeInserted",$(this)); });
$(document).on('onComplete', 'select', function(){ documentUpdated("onComplete",$(this)) });
$(document).ready(function(){ 

	$( "a" ).hover(
	  function(e) {
		updateLink($(this));
	  }
	);

	$('a[href^="http://"]').each(function(){ 
		var href = $(this).attr("href"); // Get current url
		var newUrl = href.replace("http://", "https://"); // Create new url
		$(this).attr("href", newUrl); // Set herf value
	});

	documentUpdated("ready",$(this)); });



$('.pl-functor').each(function(){

	$(this).init.prototype.expand = function(want){
		var e = $(this).next('span');
		var p = $(e).next('span');
		if ($(e).is('.pl-args')) {
			//debugger;
			p = e;
			e = $(p).before('<span class="pl-ellipsis, fold">...</span>');
		} else {
			//debugger;
		}    
		if( !$(p).is('fold') != want ) {
			$(p).toggleClass('fold');
			$(e).toggleClass('fold');
		}
	};

	$(this).init.prototype.expanded = function(){
		var e = $(this).next('span');
		var p = $(e).next('span');
		if ($(e).is('.pl-args')) {
			//debugger;
			p = e;
		} else {
			//debugger;
		}    
		return !$(p).is('fold');
	};
})
/*
$(function(){
    $.contextMenu({
		selector: ".context-menu-one",
		build: function($trigger, e) {
			var options = { 
			  callback: function(key, options) {
				alert("Clicked on " + key + " on element " + options.$trigger.attr("id"));
				// TODO:
				// Display NAME of the menu item clicked(example: item1)
				//alert("Clicked on item:  " + JSON.stringify(options.items));                    
				return false;
			  },
			  // start with an empty map
			  items: {
				"fold1": { 
				  "name": "menu 1",
				  "items": {}
				},
				"fold2": {}                  
			  }
			};

			$.each(menu1_item_names, function(k, v) {
				options.items.fold1.items[k] = {
				 name: v
				};
			});

			if (typeof menu2_item_names !== "undefined" && menu2_item_names.length > 0) {
			  options.items.fold2 = {
									  "name": "menu 2",
									  "items": {} 
									}

			  $.each(menu2_item_names, function(k, v) {
				  options.items.fold2.items[k] = {
					name: v
				  };
			  });
			}

			options.items.sep1 = "---------";
			options.items.quit = {
				name: "Quit"
			};
			return options;
		}
    })
});

*/


var doChildren = 1;

$(document).on('click', '.pl-functor, .pl-infix', function() {

	var e = $(this).next('span');
	var p = $(e).next('span');
	if ($(e).is('.pl-args')) {
		//debugger;
		p = e;
		e = $(p).before('<span class="pl-ellipsis, fold">...</span>');
	} else {
		//debugger;
	}    

	var wasHidden = $(p).is('fold');
	if(wasHidden || doChildren < 1) {
	   $(p).toggleClass('fold');
	   $(e).toggleClass('fold');
	   return;
	}

	if(shiftPressed && doChildren>0) {
        $(this).parents().get().prev("span.pl-functor").click();
		return;
	}
	if(!ctrlPressed) {
		$(p).toggleClass('fold');
		$(e).toggleClass('fold');
		return;
	} else {
		var pl_functors = 0;
		var pl_grand_functors = 0;
		  $(p).children().each(function() {
					var $this = $(this);
					if ($this.is('.pl-functor')) {			
						pl_functors++;
				        $($this).children().each(function() {
								var $this = $(this);
								if ($this.is('.pl-functor')) {			
									pl_grand_functors++;
								}
						})
					}
		  });
		 if(pl_functors ==0) {
			$(p).toggleClass('fold');
			$(e).toggleClass('fold');
			return;
		 }
		 if(pl_functors == 1) {
  		     $(p).children().each(function() {
						var $this = $(this);
						if ($this.is('.pl-functor')) {			
							$this.click();
						}
			 });
			 return;
		 }

		 var shrunk = 0;
		 if( doChildren>0 || shiftPressed) {
			doChildren--;
			$(p).children().each(function() {
					var $this = $(this);
					if ($this.is('.pl-functor')) {			
						//
						var tp = $this.find('.pl-args');
						subWasHidden = $(tp).is('fold');
						$this.click();
						shrunk++;
					}
				});
			doChildren++;
			return;
		}
		if (shrunk < 1) {
			$(p).toggleClass('fold');
			$(e).toggleClass('fold');
		}
	}
})

$(document).on('click', '.pl-ellipsis', function() {
	$(this).next('span').toggleClass('fold')
	//$(this).remove()
	$(this).toggleClass('fold')
})

$.getScript( "/swish/js/butterfly_term.js" )
  .done(function( script, textStatus ) {
    console.log( textStatus + " butterfly_term "  );
  })
  .fail(function( jqxhr, settings, exception ) {
	debugger;    
});


/*
function unfold(thizfunctor,untils, folded) {
	folded.toggleClass('fold')
	$(this).remove()
}
$(document).on('click', '.pl-functor, .pl-infix', function() {
	var named = $(this).text();
	// alert(named);
    //$(this).next('span').css( "background-color", "red" );
	var p = $(this).nextAll('span:first');
	//var u = p.nextUntil('pl-functor');
	//alert($(this).nextAll('span:first').text());
	//$(this).nextAll('span:first').effect( "highlight", {color:"#669966"}, 3000 );
	$(p).toggleClass('fold')
	// var str = '<span class="pl-ellipsis">.' + named + '..</span>';
	var str = '<span class="pl-ellipsis">...</span>';
	$(p).before(str).prev().click(function() { unfold(this,u,p)})
})
*/


}); // define
	// 
	// 
