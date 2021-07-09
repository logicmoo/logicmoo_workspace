/*  Part of Logicmoo's SWISH

    Author:        Douglas Miles
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


function mouseoverHilite(class_action, class_id, anchors, debug_item) {
    for (const anchor of anchors || []) {
        const sig = document.getElementById(signature_prefix + anchor);
        if (sig) {
            sig.classList[class_action](class_id); // sig.classList.{add,remove}(class_id)
        } else {
            if (class_action === 'add') {
                console.trace('No edge for anchor', anchor, debug_item);
                // alert('No edge for ' + debug_item);
            }
        }
    }
}

// Callback for a click on a token (anchor) in the source display
async function clickAnchor(mouse_target, source_item) {
    console.assert(mouse_target.id.startsWith(signature_prefix), 'Invalid signature_prefix', mouse_target.id, 'should start with:', signature_prefix);
    const signature = mouse_target.id.substr(signature_prefix.length);
    await fetchFromServer(
        {anchor_xref: {signature: signature,
                       corpus: source_item.corpus,
                       root: source_item.root,
                       path: source_item.path,
                       language: 'python'}},  // TODO: don't hard-code language
        data => setXref(source_item, mouse_target.id, data));
}


function loadcssfile(filename){
  var fileref=document.createElement("link")
  fileref.setAttribute("rel", "stylesheet")
  fileref.setAttribute("type", "text/css")
  fileref.setAttribute("href", filename)
  if (typeof fileref!="undefined")
   document.getElementsByTagName("head")[0].appendChild(fileref)
}
function loadjsfile(filename){
/*$(jQuery).getScript( filename )
  .done(function( script, textStatus ) {
    console.log( textStatus + " " + filename  );
  })
  .fail(function( jqxhr, settings, exception ) {
	debugger;    
});*/
  var fileref=document.createElement('script')
  fileref.setAttribute("type","text/javascript")
  fileref.setAttribute("src", filename)
  if (typeof fileref!="undefined")
  document.getElementsByTagName("head")[0].appendChild(fileref);
}


var mySwish = `<div id="hidden_swish_app" style="display:none; visibility:hidden">
		<header class="navbar navbar-default">
			<div class="container pull-left">
				<div class="navbar-header">
					<a href="/" class="pengine-logo">&nbsp;</a>
					<a href="/" class="swish-logo">&nbsp;</a>
				</div>
				<nav id="navbar"></nav>
			</div>
		</header>
		<div id="content" class="container">
		  <div class="tile horizontal" data-split="60%">
			<div class="prolog-editor"></div>
			<div class="tile vertical" data-split="70%">
			  <div class="prolog-runners"></div>
			  <div class="prolog-query"></div>
			</div>
		  </div>
		</div>
   </div>`;


$('body').each(function(){
	if(! $( ".prolog-editor:first" ).hasClass("prolog-editor") ){
		$(this).append(mySwish);

	}

	// $(function() { $("body").swish(config.swish || {});
	
	loadjsfile("https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js")
	loadjsfile("https://cdnjs.cloudflare.com/ajax/libs/jquery.qrcode/1.0/jquery.qrcode.min.js")
	loadcssfile("/swish/css/menu.css")
	loadcssfile("/swish/css/cliopatria.css")	
	loadcssfile("/www/yui/2.7.0/build/autocomplete/assets/skins/sam/autocomplete.css")
	loadjsfile("/www/yui/2.7.0/build/utilities/utilities.js")
	// Use either font-awesome icons or Google icons with these links. Other icons could also be used if preferred
	loadcssfile("https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
	loadcssfile("https://fonts.googleapis.com/icon?family=Material+Icons")
	loadjsfile("/swish/lm_xref/pixmapx/popupmenu/scripts/Popup-plugin.js")
	loadcssfile("/swish/lm_xref/pixmapx/popupmenu/styles/Popup-plugin.css")
	
	//loadjsfile("/www/yui/2.7.0/build/datasource/datasource.js")
	//loadjsfile("/www/yui/2.7.0/build/autocomplete/autocomplete.js")
	// loadjsfile("https://code.jquery.com/jquery-1.12.4.min.js")
	loadjsfile("/swish/lm_xref/pixmapx/selected/js/social.selection.js")
	loadcssfile("/swish/lm_xref/pixmapx/selected/css/social.selection.css")
	// loadcssfile("/swish/lm_xref/pixmapx/selected/css/example.css")
	loadjsfile("/swish/js/cliopatria.js")
	loadcssfile("/swish/css/butterfly_term.css")
	loadcssfile("/swish/css/term.css")

});

 // necessary for the "draggable" ui  



/*

loadjsfile("/swish/lm_xref/pixmapx/popupmenu/scripts/Example.js")

// debugger;

// loadjsfile("/swish/js/jquery-2.1.3.min.js")
loadcssfile("/swish/css/term.css")
loadcssfile("/swish/css/butterfly_term.css")

// loadcssfile("/swish/lm_xref/pixmapx/popupmenu/styles/Example.css")

*/

if (!window.x) {
    window.x = {};
}

window.x.Selector = {};
window.x.Selector.getSelected = function() {
    var t = "";
    if (window.getSelection) {
        t = window.getSelection();
    } else if (document.getSelection) {
        t = document.getSelection();
    } else if (document.selection) {
        t = document.selection.createRange().text;
    }
    return t;
}

var pageX;
var pageY;
var toolElement;
var lastSelectedText;
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

$(document).ready(function() {


   function setSelected(isButtonUp) {
		var selectedText = ""+x.Selector.getSelected();
		selectedText = selectedText.trim();
		if(selectedText != ""){
			// button up we hope
			if(isButtonUp) document.execCommand('copy');

			/*
              const el = document.createElement('textarea');
			  el.value = selectedText;
			  document.body.appendChild(el);
			  el.select();
			  document.execCommand('copy');
			  document.body.removeChild(el);*/

			 lastSelectedText = selectedText;
			if(shiftPressed || ctrlPressed)showSelected();
		} else {
			hideToolElement();
		}
		
	  }

   var shouldBeVisible = false;

   function hideToolElement() {
	   if(shouldBeVisible) {
		 shouldBeVisible = false;	   
	     ensureToolElement();
		 toolElement = $("div.selectionTooltip:first");
	     toolElement.delay(2000).fadeOut(300);
	   }
   }
   function showSelected() {
	   shouldBeVisible = true;
		ensureToolElement();
		toolElement = $("div.selectionTooltip:first")
		//if(toolElement.hasClass("selectionTooltip")) {
		  var st = toolElement.find("#selectedText");
		  //debugger;
		  if(lastSelectedText.indexOf('\n')>0) {
			  $(st).replaceWith($('<textarea id="selectedText">  '+ lastSelectedText + "  </textarea>"))		  
		  } else {
			  $(st).replaceWith($('<pre id="selectedText">  '+ lastSelectedText + "  </pre>"))
		  }
		//}
		reposition();
	}

	 function reposition() {
		 if(!shouldBeVisible) {
            return;
		 }
		 ensureToolElement();
		  var toolElement = $("div.selectionTooltip:first")
		  if(toolElement.hasClass("selectionTooltip")) {
			  var x = pageX - 125; 
			  if (x<10) { x = 10; }
			  var y = pageY - 180; 
			  if (y<20) { y = 190; }
			  toolElement.css({"left": x,"top" : y}).fadeIn(200);
		  }
	 }


	 function setXY(element) {
		 pageY = $(element).position().top;
		 pageX = $(element).position().left;
		 //pageY = $(element).offset().top;
		 //pageX = $(element).offset().left;
	 }


	$("textarea").focus(function(e) {  
      var text = $(this).val(); 
	  if (text=="") {
		  document.execCommand("paste");
		  return;
	  }
	  return;
	  $(this).select(); setXY($(this)); setSelected(false)} );

	$("input:text").focus(function(e) { 
      var text = $(this).val(); 
	  if (text=="") {
		  document.execCommand("paste");
		  return;
	  }
	  return;
	  $(this).select(); setXY($(this)); setSelected(false)} );

    $(document).bind("mouseup", function(e) {
		if(contains($("div.selectionTooltip:first"),e.target)) return;        
		
        setSelected(true);      
    });

	    {// remove wierd comma that shows up
		var v = document.querySelector("body > p");
		if (v) {
			var waz = v.outerHTML;
			if(waz=="<p>,\n     </p>") v.remove();
		}}
	 

	function contains(p,target) {
		 if ($(target).parents("div.selectionTooltip:first").length) {
			 return true;
		 } 
		 return false;
	}

	function ensureToolElement() {
	   toolElement = $("div.selectionTooltip:first")
	   if(!toolElement.hasClass("selectionTooltip")) {
		  var x = pageX - 115; 
		  var y = pageY - 185; 
		  toolElementSrc = `
			<div class="selectionTooltip selectionTooltip752708 center" style="background-color: white; color: black;" top: `+y+`px; left: `+x+`px; max-width: 360px">
			  <textarea id="selectedText"> Selected </textarea>
   		      <span style="font-size: 32px;">`;
		  toolElementSrc += "<p/>";

          var obj = { search:"find", copy:"copy", paste:"replace", bath:"Assertion", 'quote-left':"English" };
          jQuery.each(obj, function(i, val) {
                 toolElementSrc += '<button title="'+val+'"><i class="fa fa-'+ i + '"/></button>';
		   });
		  toolElementSrc += "<p/>";
		  var foo = ""+$("#table5 > tbody > tr:nth-child(1) > td:nth-child(3) > label").clone().html();
		  if(foo != "undefined") {
			  toolElementSrc += foo;
		  } else {
			  toolElementSrc += `<label><select name="action_above"><option value="Find">Find $item</option><option value="Forward">Forward Direction</option><option value="Backward">Backward Direction</option><option value="query" selected="yes">Query Item</option><option value="repropagate">Repropagate $item (ReAssert)</option><option value="remove">Remove $item(Unassert)</option><option value="Code">Assume Theorem (Disable $item)</option><option value="prologSingleValued">Make $item Single Valued</option><option value="prologBuiltin">Impl $item in Prolog</option><option value="prologPTTP">Impl $item in PTTP</option><option value="prologDRA">Impl $item in DRA</option><option value="prologPfc">Impl $item in PFC</option><option value="Monotonic">Treat $item Monotonic</option><option value="NonMonotonic">Treat $item NonMonotonic</option></select>&nbsp;&nbsp;&nbsp;<input type="submit" value="Now" name="Apply"></label>`;
		  }
          
          toolElementSrc += `</span></div>`;
          toolElement = $(toolElementSrc);
	      $("body").append(toolElement);
          $(toolElement).mousedown(function(event){
			  event.stopPropagation();
  		  });
		  $(toolElement).mouseup(function(event){
			  event.stopPropagation();
  		  });
	   }
	}

	$(document).on("mousedown", function(e){
		pageX = e.pageX;
		pageY = e.pageY;
		if(contains($("div.selectionTooltip:first"),e.target)) return;
		if (e.buttons == 2) {
			if(lastSelectedText!=null)  {
				document.execCommand("paste")
			}
		}   
    });


  function getClipboard() {
    var pasteTarget = document.createElement("div");
    pasteTarget.contentEditable = true;
    var actElem = document.activeElement.appendChild(pasteTarget).parentNode;
    pasteTarget.focus();
    document.execCommand("Paste", null, null);
    var paste = pasteTarget.innerText;
    actElem.removeChild(pasteTarget);
    return paste;
  };
});
