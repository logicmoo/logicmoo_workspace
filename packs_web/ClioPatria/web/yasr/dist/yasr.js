!function(e){if("object"==typeof exports&&"undefined"!=typeof module)module.exports=e();else if("function"==typeof define&&define.amd)define([],e);else{var f;"undefined"!=typeof window?f=window:"undefined"!=typeof global?f=global:"undefined"!=typeof self&&(f=self),f.YASR=e()}}(function(){var define,module,exports;return (function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
//this is the entry-point for browserify.
//the current browserify version does not support require-ing js files which are used as entry-point
//this way, we can still require our main.js file
module.exports = require('./main.js');
},{"./main.js":30}],2:[function(require,module,exports){
/**
               _ _____           _          _     _      
              | |  __ \         (_)        | |   | |     
      ___ ___ | | |__) |___  ___ _ ______ _| |__ | | ___ 
     / __/ _ \| |  _  // _ \/ __| |_  / _` | '_ \| |/ _ \
    | (_| (_) | | | \ \  __/\__ \ |/ / (_| | |_) | |  __/
     \___\___/|_|_|  \_\___||___/_/___\__,_|_.__/|_|\___|
	 
	v 1.4 - a jQuery plugin by Alvaro Prieto Lauroba
	
	Licences: MIT & GPL
	Feel free to use or modify this plugin as far as my full name is kept	
	
	If you are going to use this plugin in production environments it is 
	strongly recomended to use its minified version: colResizable.min.js

*/

var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})();	
	var d = $(document); 		//window object
	var h = $("head");			//head object
	var drag = null;			//reference to the current grip that is being dragged
	var tables = [];			//array of the already processed tables (table.id as key)
	var	count = 0;				//internal count to create unique IDs when needed.	
	
	//common strings for minification	(in the minified version there are plenty more)
	var ID = "id";	
	var PX = "px";
	var SIGNATURE ="JColResizer";
	
	//shortcuts
	var I = parseInt;
	var M = Math;
	var ie = navigator.userAgent.indexOf('Trident/4.0')>0;
	var S;
	try{S = sessionStorage;}catch(e){}	//Firefox crashes when executed as local file system
	
	//append required CSS rules  
	h.append("<style type='text/css'>  .JColResizer{table-layout:fixed;} .JColResizer td, .JColResizer th{overflow:hidden;padding-left:0!important; padding-right:0!important;}  .JCLRgrips{ height:0px; position:relative;} .JCLRgrip{margin-left:-5px; position:absolute; z-index:5; } .JCLRgrip .JColResizer{position:absolute;background-color:red;filter:alpha(opacity=1);opacity:0;width:10px;height:100%;top:0px} .JCLRLastGrip{position:absolute; width:1px; } .JCLRgripDrag{ border-left:1px dotted black;	}</style>");

	
	/**
	 * Function to allow column resizing for table objects. It is the starting point to apply the plugin.
	 * @param {DOM node} tb - refrence to the DOM table object to be enhanced
	 * @param {Object} options	- some customization values
	 */
	var init = function( tb, options){	
		var t = $(tb);										//the table object is wrapped
		if(options.disable) return destroy(t);				//the user is asking to destroy a previously colResized table
		var	id = t.id = t.attr(ID) || SIGNATURE+count++;	//its id is obtained, if null new one is generated		
		t.p = options.postbackSafe; 						//shortcut to detect postback safe 		
		if(!t.is("table") || tables[id]) return; 			//if the object is not a table or if it was already processed then it is ignored.
		t.addClass(SIGNATURE).attr(ID, id).before('<div class="JCLRgrips"/>');	//the grips container object is added. Signature class forces table rendering in fixed-layout mode to prevent column's min-width
		t.opt = options; t.g = []; t.c = []; t.w = t.width(); t.gc = t.prev();	//t.c and t.g are arrays of columns and grips respectively				
		if(options.marginLeft) t.gc.css("marginLeft", options.marginLeft);  	//if the table contains margins, it must be specified
		if(options.marginRight) t.gc.css("marginRight", options.marginRight);  	//since there is no (direct) way to obtain margin values in its original units (%, em, ...)
		t.cs = I(ie? tb.cellSpacing || tb.currentStyle.borderSpacing :t.css('border-spacing'))||2;	//table cellspacing (not even jQuery is fully cross-browser)
		t.b  = I(ie? tb.border || tb.currentStyle.borderLeftWidth :t.css('border-left-width'))||1;	//outer border width (again cross-browser isues)
		// if(!(tb.style.width || tb.width)) t.width(t.width()); //I am not an IE fan at all, but it is a pitty that only IE has the currentStyle attribute working as expected. For this reason I can not check easily if the table has an explicit width or if it is rendered as "auto"
		tables[id] = t; 	//the table object is stored using its id as key	
		createGrips(t);		//grips are created
	
	};


	/**
	 * This function allows to remove any enhancements performed by this plugin on a previously processed table.
	 * @param {jQuery ref} t - table object
	 */
	var destroy = function(t){
		var id=t.attr(ID), t=tables[id];		//its table object is found
		if(!t||!t.is("table")) return;			//if none, then it wasnt processed	 
		t.removeClass(SIGNATURE).gc.remove();	//class and grips are removed
		delete tables[id];						//clean up data
	};


	/**
	 * Function to create all the grips associated with the table given by parameters 
	 * @param {jQuery ref} t - table object
	 */
	var createGrips = function(t){	
	
		var th = t.find(">thead>tr>th,>thead>tr>td");	//if table headers are specified in its semantically correct tag, are obtained
		if(!th.length) th = t.find(">tbody>tr:first>th,>tr:first>th,>tbody>tr:first>td, >tr:first>td");	 //but headers can also be included in different ways
		t.cg = t.find("col"); 						//a table can also contain a colgroup with col elements		
		t.ln = th.length;							//table length is stored	
		if(t.p && S && S[t.id])memento(t,th);		//if 'postbackSafe' is enabled and there is data for the current table, its coloumn layout is restored
		th.each(function(i){						//iterate through the table column headers			
			var c = $(this); 						//jquery wrap for the current column			
			var g = $(t.gc.append('<div class="JCLRgrip"></div>')[0].lastChild); //add the visual node to be used as grip
			g.t = t; g.i = i; g.c = c;	c.w =c.width();		//some values are stored in the grip's node data
			t.g.push(g); t.c.push(c);						//the current grip and column are added to its table object
			c.width(c.w).removeAttr("width");				//the width of the column is converted into pixel-based measurements
			if (i < t.ln-1) {
				g.bind('touchstart mousedown', onGripMouseDown).append(t.opt.gripInnerHtml).append('<div class="'+SIGNATURE+'" style="cursor:'+t.opt.hoverCursor+'"></div>'); //bind the mousedown event to start dragging 
			} else g.addClass("JCLRLastGrip").removeClass("JCLRgrip");	//the last grip is used only to store data			
			g.data(SIGNATURE, {i:i, t:t.attr(ID)});						//grip index and its table name are stored in the HTML 												
		}); 	
		t.cg.removeAttr("width");	//remove the width attribute from elements in the colgroup (in any)
		syncGrips(t); 				//the grips are positioned according to the current table layout			
		//there is a small problem, some cells in the table could contain dimension values interfering with the 
		//width value set by this plugin. Those values are removed
		t.find('td, th').not(th).not('table th, table td').each(function(){  
			$(this).removeAttr('width');	//the width attribute is removed from all table cells which are not nested in other tables and dont belong to the header
		});		

		
	};
	

	/**
	 * Function to allow the persistence of columns dimensions after a browser postback. It is based in
	 * the HTML5 sessionStorage object, which can be emulated for older browsers using sessionstorage.js
	 * @param {jQuery ref} t - table object
	 * @param {jQuery ref} th - reference to the first row elements (only set in deserialization)
	 */
	var memento = function(t, th){ 
		var w,m=0,i=0,aux =[];
		if(th){										//in deserialization mode (after a postback)
			t.cg.removeAttr("width");
			if(t.opt.flush){ S[t.id] =""; return;} 	//if flush is activated, stored data is removed
			w = S[t.id].split(";");					//column widths is obtained
			for(;i<t.ln;i++){						//for each column
				aux.push(100*w[i]/w[t.ln]+"%"); 	//width is stored in an array since it will be required again a couple of lines ahead
				th.eq(i).css("width", aux[i] ); 	//each column width in % is resotred
			}			
			for(i=0;i<t.ln;i++)
				t.cg.eq(i).css("width", aux[i]);	//this code is required in order to create an inline CSS rule with higher precedence than an existing CSS class in the "col" elements
		}else{							//in serialization mode (after resizing a column)
			S[t.id] ="";				//clean up previous data
			for(;i < t.c.length; i++){		//iterate through columns
			
				w = t.c[i].width();		//width is obtained
				S[t.id] += w+";";		//width is appended to the sessionStorage object using ID as key
				m+=w;					//carriage is updated to obtain the full size used by columns
			}
			S[t.id]+=m;					//the last item of the serialized string is the table's active area (width), 
										//to be able to obtain % width value of each columns while deserializing
		}	
	};
	
	
	/**
	 * Function that places each grip in the correct position according to the current table layout	 * 
	 * @param {jQuery ref} t - table object
	 */
	var syncGrips = function (t){	
		t.gc.width(t.w);			//the grip's container width is updated				
		for(var i=0; i<t.ln; i++){	//for each column
			var c = t.c[i]; 			
			t.g[i].css({			//height and position of the grip is updated according to the table layout
				left: c.offset().left - t.offset().left + c.outerWidth(false) + t.cs / 2 + PX,
				height: t.opt.headerOnly? t.c[0].outerHeight(false) : t.outerHeight(false)				
			});			
		} 	
	};
	
	
	
	/**
	* This function updates column's width according to the horizontal position increment of the grip being
	* dragged. The function can be called while dragging if liveDragging is enabled and also from the onGripDragOver
	* event handler to synchronize grip's position with their related columns.
	* @param {jQuery ref} t - table object
	* @param {nunmber} i - index of the grip being dragged
	* @param {bool} isOver - to identify when the function is being called from the onGripDragOver event	
	*/
	var syncCols = function(t,i,isOver){
		var inc = drag.x-drag.l, c = t.c[i], c2 = t.c[i+1]; 			
		var w = c.w + inc;	var w2= c2.w- inc;	//their new width is obtained					
		c.width( w + PX);	c2.width(w2 + PX);	//and set	
		t.cg.eq(i).width( w + PX); t.cg.eq(i+1).width( w2 + PX);
		if(isOver){c.w=w; c2.w=w2;}
	};

	
	/**
	 * Event handler used while dragging a grip. It checks if the next grip's position is valid and updates it. 
	 * @param {event} e - mousemove event binded to the window object
	 */
	var onGripDrag = function(e){	
		if(!drag) return; var t = drag.t;		//table object reference 
		
		if (e.originalEvent.touches) {
			var x = e.originalEvent.touches[0].pageX - drag.ox + drag.l;		//next position according to horizontal mouse position increment
		} else {
			var x = e.pageX - drag.ox + drag.l;		//next position according to horizontal mouse position increment
		}
		
		
			
		var mw = t.opt.minWidth, i = drag.i ;	//cell's min width
		var l = t.cs*1.5 + mw + t.b;

		var max = i == t.ln-1? t.w-l: t.g[i+1].position().left-t.cs-mw; //max position according to the contiguous cells
		var min = i? t.g[i-1].position().left+t.cs+mw: l;				//min position according to the contiguous cells
		
		x = M.max(min, M.min(max, x));						//apply boundings		
		drag.x = x;	 drag.css("left",  x + PX); 			//apply position increment		
			
		if(t.opt.liveDrag){ 								//if liveDrag is enabled
			syncCols(t,i); syncGrips(t);					//columns and grips are synchronized
			var cb = t.opt.onDrag;							//check if there is an onDrag callback
			if (cb) { e.currentTarget = t[0]; cb(e); }		//if any, it is fired			
		}
		
		return false; 	//prevent text selection				
	};
	

	/**
	 * Event handler fired when the dragging is over, updating table layout
	 */
	var onGripDragOver = function(e){	
		
		d.unbind('touchend.'+SIGNATURE+' mouseup.'+SIGNATURE).unbind('touchmove.'+SIGNATURE+' mousemove.'+SIGNATURE);
		$("head :last-child").remove(); 				//remove the dragging cursor style	
		if(!drag) return;
		drag.removeClass(drag.t.opt.draggingClass);		//remove the grip's dragging css-class
		var t = drag.t;
		var cb = t.opt.onResize; 			//get some values	
		if(drag.x){ 									//only if the column width has been changed
			syncCols(t,drag.i, true);	syncGrips(t);	//the columns and grips are updated
			if (cb) { e.currentTarget = t[0]; cb(e); }	//if there is a callback function, it is fired
		}
		if(t.p && S) memento(t); 						//if postbackSafe is enabled and there is sessionStorage support, the new layout is serialized and stored
		drag = null;									//since the grip's dragging is over									
	};	
	

	/**
	 * Event handler fired when the grip's dragging is about to start. Its main goal is to set up events 
	 * and store some values used while dragging.
	 * @param {event} e - grip's mousedown event
	 */
	var onGripMouseDown = function(e){
		var o = $(this).data(SIGNATURE);			//retrieve grip's data
		var t = tables[o.t],  g = t.g[o.i];			//shortcuts for the table and grip objects
		if (e.originalEvent.touches) {
			g.ox = e.originalEvent.touches[0].pageX;
		} else {
			g.ox = e.pageX;	//the initial position is kept
		}
		g.l = g.position().left;
		d.bind('touchmove.'+SIGNATURE+' mousemove.'+SIGNATURE, onGripDrag).bind('touchend.'+SIGNATURE+' mouseup.'+SIGNATURE,onGripDragOver);	//mousemove and mouseup events are bound
		h.append("<style type='text/css'>*{cursor:"+ t.opt.dragCursor +"!important}</style>"); 	//change the mouse cursor
		g.addClass(t.opt.draggingClass); 	//add the dragging class (to allow some visual feedback)				
		drag = g;							//the current grip is stored as the current dragging object
		if(t.c[o.i].l) for(var i=0,c; i<t.ln; i++){ c=t.c[i]; c.l = false; c.w= c.width(); } 	//if the colum is locked (after browser resize), then c.w must be updated		
		return false; 	//prevent text selection
	};
	
	/**
	 * Event handler fired when the browser is resized. The main purpose of this function is to update
	 * table layout according to the browser's size synchronizing related grips 
	 */
	var onResize = function(){
		for(t in tables){		
			var t = tables[t], i, mw=0;				
			t.removeClass(SIGNATURE);						//firefox doesnt like layout-fixed in some cases
			if (t.w != t.width()) {							//if the the table's width has changed
				t.w = t.width();							//its new value is kept
				for(i=0; i<t.ln; i++) mw+= t.c[i].w;		//the active cells area is obtained
				//cell rendering is not as trivial as it might seem, and it is slightly different for
				//each browser. In the begining i had a big switch for each browser, but since the code
				//was extremelly ugly now I use a different approach with several reflows. This works 
				//pretty well but it's a bit slower. For now, lets keep things simple...   
				for(i=0; i<t.ln; i++) t.c[i].css("width", M.round(1000*t.c[i].w/mw)/10 + "%").l=true; 
				//c.l locks the column, telling us that its c.w is outdated									
			}
			syncGrips(t.addClass(SIGNATURE));
		}
	};		


	//bind resize event, to update grips position 
	$(window).bind('resize.'+SIGNATURE, onResize); 


	/**
	 * The plugin is added to the jQuery library
	 * @param {Object} options -  an object containg some basic customization values 
	 */
    $.fn.extend({  
        colResizable: function(options) {           
            var defaults = {
			
				//attributes:
                draggingClass: 'JCLRgripDrag',	//css-class used when a grip is being dragged (for visual feedback purposes)
				gripInnerHtml: '',				//if it is required to use a custom grip it can be done using some custom HTML				
				liveDrag: false,				//enables table-layout updaing while dragging			
				minWidth: 15, 					//minimum width value in pixels allowed for a column 
				headerOnly: false,				//specifies that the size of the the column resizing anchors will be bounded to the size of the first row 
				hoverCursor: "e-resize",  		//cursor to be used on grip hover
				dragCursor: "e-resize",  		//cursor to be used while dragging
				postbackSafe: false, 			//when it is enabled, table layout can persist after postback. It requires browsers with sessionStorage support (it can be emulated with sessionStorage.js). Some browsers ony 
				flush: false, 					//when postbakSafe is enabled, and it is required to prevent layout restoration after postback, 'flush' will remove its associated layout data 
				marginLeft: null,				//in case the table contains any margins, colResizable needs to know the values used, e.g. "10%", "15em", "5px" ...
				marginRight: null, 				//in case the table contains any margins, colResizable needs to know the values used, e.g. "10%", "15em", "5px" ...
				disable: false,					//disables all the enhancements performed in a previously colResized table	
				
				//events:
				onDrag: null, 					//callback function to be fired during the column resizing process if liveDrag is enabled
				onResize: null					//callback function fired when the dragging process is over
            }			
			var options =  $.extend(defaults, options);			
            return this.each(function() {				
             	init( this, options);             
            });
        }
    });


},{"jquery":undefined}],3:[function(require,module,exports){
/**
 * jQuery-csv (jQuery Plugin)
 * version: 0.71 (2012-11-19)
 *
 * This document is licensed as free software under the terms of the
 * MIT License: http://www.opensource.org/licenses/mit-license.php
 *
 * Acknowledgements:
 * The original design and influence to implement this library as a jquery
 * plugin is influenced by jquery-json (http://code.google.com/p/jquery-json/).
 * If you're looking to use native JSON.Stringify but want additional backwards
 * compatibility for browsers that don't support it, I highly recommend you
 * check it out.
 *
 * A special thanks goes out to rwk@acm.org for providing a lot of valuable
 * feedback to the project including the core for the new FSM
 * (Finite State Machine) parsers. If you're looking for a stable TSV parser
 * be sure to take a look at jquery-tsv (http://code.google.com/p/jquery-tsv/).

 * For legal purposes I'll include the "NO WARRANTY EXPRESSED OR IMPLIED.
 * USE AT YOUR OWN RISK.". Which, in 'layman's terms' means, by using this
 * library you are accepting responsibility if it breaks your code.
 *
 * Legal jargon aside, I will do my best to provide a useful and stable core
 * that can effectively be built on.
 *
 * Copyrighted 2012 by Evan Plaice.
 */

RegExp.escape= function(s) {
    return s.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
};

  'use strict'
  var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})();
  /**
   * jQuery.csv.defaults
   * Encapsulates the method paramater defaults for the CSV plugin module.
   */

  $.csv = {
    defaults: {
      separator:',',
      delimiter:'"',
      headers:true
    },

    hooks: {
      castToScalar: function(value, state) {
        var hasDot = /\./;
        if (isNaN(value)) {
          return value;
        } else {
          if (hasDot.test(value)) {
            return parseFloat(value);
          } else {
            var integer = parseInt(value);
            if(isNaN(integer)) {
              return null;
            } else {
              return integer;
            }
          }
        }
      }
    },

    parsers: {
      parse: function(csv, options) {
        // cache settings
        var separator = options.separator;
        var delimiter = options.delimiter;

        // set initial state if it's missing
        if(!options.state.rowNum) {
          options.state.rowNum = 1;
        }
        if(!options.state.colNum) {
          options.state.colNum = 1;
        }

        // clear initial state
        var data = [];
        var entry = [];
        var state = 0;
        var value = ''
        var exit = false;

        function endOfEntry() {
          // reset the state
          state = 0;
          value = '';

          // if 'start' hasn't been met, don't output
          if(options.start && options.state.rowNum < options.start) {
            // update global state
            entry = [];
            options.state.rowNum++;
            options.state.colNum = 1;
            return;
          }
          
          if(options.onParseEntry === undefined) {
            // onParseEntry hook not set
            data.push(entry);
          } else {
            var hookVal = options.onParseEntry(entry, options.state); // onParseEntry Hook
            // false skips the row, configurable through a hook
            if(hookVal !== false) {
              data.push(hookVal);
            }
          }
          //console.log('entry:' + entry);
          
          // cleanup
          entry = [];

          // if 'end' is met, stop parsing
          if(options.end && options.state.rowNum >= options.end) {
            exit = true;
          }
          
          // update global state
          options.state.rowNum++;
          options.state.colNum = 1;
        }

        function endOfValue() {
          if(options.onParseValue === undefined) {
            // onParseValue hook not set
            entry.push(value);
          } else {
            var hook = options.onParseValue(value, options.state); // onParseValue Hook
            // false skips the row, configurable through a hook
            if(hook !== false) {
              entry.push(hook);
            }
          }
          //console.log('value:' + value);
          // reset the state
          value = '';
          state = 0;
          // update global state
          options.state.colNum++;
        }

        // escape regex-specific control chars
        var escSeparator = RegExp.escape(separator);
        var escDelimiter = RegExp.escape(delimiter);

        // compile the regEx str using the custom delimiter/separator
        var match = /(D|S|\n|\r|[^DS\r\n]+)/;
        var matchSrc = match.source;
        matchSrc = matchSrc.replace(/S/g, escSeparator);
        matchSrc = matchSrc.replace(/D/g, escDelimiter);
        match = RegExp(matchSrc, 'gm');

        // put on your fancy pants...
        // process control chars individually, use look-ahead on non-control chars
        csv.replace(match, function (m0) {
          if(exit) {
            return;
          }
          switch (state) {
            // the start of a value
            case 0:
              // null last value
              if (m0 === separator) {
                value += '';
                endOfValue();
                break;
              }
              // opening delimiter
              if (m0 === delimiter) {
                state = 1;
                break;
              }
              // null last value
              if (m0 === '\n') {
                endOfValue();
                endOfEntry();
                break;
              }
              // phantom carriage return
              if (/^\r$/.test(m0)) {
                break;
              }
              // un-delimited value
              value += m0;
              state = 3;
              break;

            // delimited input
            case 1:
              // second delimiter? check further
              if (m0 === delimiter) {
                state = 2;
                break;
              }
              // delimited data
              value += m0;
              state = 1;
              break;

            // delimiter found in delimited input
            case 2:
              // escaped delimiter?
              if (m0 === delimiter) {
                value += m0;
                state = 1;
                break;
              }
              // null value
              if (m0 === separator) {
                endOfValue();
                break;
              }
              // end of entry
              if (m0 === '\n') {
                endOfValue();
                endOfEntry();
                break;
              }
              // phantom carriage return
              if (/^\r$/.test(m0)) {
                break;
              }
              // broken paser?
              throw new Error('CSVDataError: Illegal State [Row:' + options.state.rowNum + '][Col:' + options.state.colNum + ']');

            // un-delimited input
            case 3:
              // null last value
              if (m0 === separator) {
                endOfValue();
                break;
              }
              // end of entry
              if (m0 === '\n') {
                endOfValue();
                endOfEntry();
                break;
              }
              // phantom carriage return
              if (/^\r$/.test(m0)) {
                break;
              }
              if (m0 === delimiter) {
              // non-compliant data
                throw new Error('CSVDataError: Illegal Quote [Row:' + options.state.rowNum + '][Col:' + options.state.colNum + ']');
              }
              // broken parser?
              throw new Error('CSVDataError: Illegal Data [Row:' + options.state.rowNum + '][Col:' + options.state.colNum + ']');
            default:
              // shenanigans
              throw new Error('CSVDataError: Unknown State [Row:' + options.state.rowNum + '][Col:' + options.state.colNum + ']');
          }
          //console.log('val:' + m0 + ' state:' + state);
        });

        // submit the last entry
        // ignore null last line
        if(entry.length !== 0) {
          endOfValue();
          endOfEntry();
        }

        return data;
      },

      // a csv-specific line splitter
      splitLines: function(csv, options) {
        // cache settings
        var separator = options.separator;
        var delimiter = options.delimiter;

        // set initial state if it's missing
        if(!options.state.rowNum) {
          options.state.rowNum = 1;
        }

        // clear initial state
        var entries = [];
        var state = 0;
        var entry = '';
        var exit = false;

        function endOfLine() {          
          // reset the state
          state = 0;
          
          // if 'start' hasn't been met, don't output
          if(options.start && options.state.rowNum < options.start) {
            // update global state
            entry = '';
            options.state.rowNum++;
            return;
          }
          
          if(options.onParseEntry === undefined) {
            // onParseEntry hook not set
            entries.push(entry);
          } else {
            var hookVal = options.onParseEntry(entry, options.state); // onParseEntry Hook
            // false skips the row, configurable through a hook
            if(hookVal !== false) {
              entries.push(hookVal);
            }
          }

          // cleanup
          entry = '';

          // if 'end' is met, stop parsing
          if(options.end && options.state.rowNum >= options.end) {
            exit = true;
          }
          
          // update global state
          options.state.rowNum++;
        }

        // escape regex-specific control chars
        var escSeparator = RegExp.escape(separator);
        var escDelimiter = RegExp.escape(delimiter);

        // compile the regEx str using the custom delimiter/separator
        var match = /(D|S|\n|\r|[^DS\r\n]+)/;
        var matchSrc = match.source;
        matchSrc = matchSrc.replace(/S/g, escSeparator);
        matchSrc = matchSrc.replace(/D/g, escDelimiter);
        match = RegExp(matchSrc, 'gm');
        
        // put on your fancy pants...
        // process control chars individually, use look-ahead on non-control chars
        csv.replace(match, function (m0) {
          if(exit) {
            return;
          }
          switch (state) {
            // the start of a value/entry
            case 0:
              // null value
              if (m0 === separator) {
                entry += m0;
                state = 0;
                break;
              }
              // opening delimiter
              if (m0 === delimiter) {
                entry += m0;
                state = 1;
                break;
              }
              // end of line
              if (m0 === '\n') {
                endOfLine();
                break;
              }
              // phantom carriage return
              if (/^\r$/.test(m0)) {
                break;
              }
              // un-delimit value
              entry += m0;
              state = 3;
              break;

            // delimited input
            case 1:
              // second delimiter? check further
              if (m0 === delimiter) {
                entry += m0;
                state = 2;
                break;
              }
              // delimited data
              entry += m0;
              state = 1;
              break;

            // delimiter found in delimited input
            case 2:
              // escaped delimiter?
              var prevChar = entry.substr(entry.length - 1);
              if (m0 === delimiter && prevChar === delimiter) {
                entry += m0;
                state = 1;
                break;
              }
              // end of value
              if (m0 === separator) {
                entry += m0;
                state = 0;
                break;
              }
              // end of line
              if (m0 === '\n') {
                endOfLine();
                break;
              }
              // phantom carriage return
              if (m0 === '\r') {
                break;
              }
              // broken paser?
              throw new Error('CSVDataError: Illegal state [Row:' + options.state.rowNum + ']');

            // un-delimited input
            case 3:
              // null value
              if (m0 === separator) {
                entry += m0;
                state = 0;
                break;
              }
              // end of line
              if (m0 === '\n') {
                endOfLine();
                break;
              }
              // phantom carriage return
              if (m0 === '\r') {
                break;
              }
              // non-compliant data
              if (m0 === delimiter) {
                throw new Error('CSVDataError: Illegal quote [Row:' + options.state.rowNum + ']');
              }
              // broken parser?
              throw new Error('CSVDataError: Illegal state [Row:' + options.state.rowNum + ']');
            default:
              // shenanigans
              throw new Error('CSVDataError: Unknown state [Row:' + options.state.rowNum + ']');
          }
          //console.log('val:' + m0 + ' state:' + state);
        });

        // submit the last entry
        // ignore null last line
        if(entry !== '') {
          endOfLine();
        }

        return entries;
      },

      // a csv entry parser
      parseEntry: function(csv, options) {
        // cache settings
        var separator = options.separator;
        var delimiter = options.delimiter;
        
        // set initial state if it's missing
        if(!options.state.rowNum) {
          options.state.rowNum = 1;
        }
        if(!options.state.colNum) {
          options.state.colNum = 1;
        }

        // clear initial state
        var entry = [];
        var state = 0;
        var value = '';

        function endOfValue() {
          if(options.onParseValue === undefined) {
            // onParseValue hook not set
            entry.push(value);
          } else {
            var hook = options.onParseValue(value, options.state); // onParseValue Hook
            // false skips the value, configurable through a hook
            if(hook !== false) {
              entry.push(hook);
            }
          }
          // reset the state
          value = '';
          state = 0;
          // update global state
          options.state.colNum++;
        }

        // checked for a cached regEx first
        if(!options.match) {
          // escape regex-specific control chars
          var escSeparator = RegExp.escape(separator);
          var escDelimiter = RegExp.escape(delimiter);
          
          // compile the regEx str using the custom delimiter/separator
          var match = /(D|S|\n|\r|[^DS\r\n]+)/;
          var matchSrc = match.source;
          matchSrc = matchSrc.replace(/S/g, escSeparator);
          matchSrc = matchSrc.replace(/D/g, escDelimiter);
          options.match = RegExp(matchSrc, 'gm');
        }

        // put on your fancy pants...
        // process control chars individually, use look-ahead on non-control chars
        csv.replace(options.match, function (m0) {
          switch (state) {
            // the start of a value
            case 0:
              // null last value
              if (m0 === separator) {
                value += '';
                endOfValue();
                break;
              }
              // opening delimiter
              if (m0 === delimiter) {
                state = 1;
                break;
              }
              // skip un-delimited new-lines
              if (m0 === '\n' || m0 === '\r') {
                break;
              }
              // un-delimited value
              value += m0;
              state = 3;
              break;

            // delimited input
            case 1:
              // second delimiter? check further
              if (m0 === delimiter) {
                state = 2;
                break;
              }
              // delimited data
              value += m0;
              state = 1;
              break;

            // delimiter found in delimited input
            case 2:
              // escaped delimiter?
              if (m0 === delimiter) {
                value += m0;
                state = 1;
                break;
              }
              // null value
              if (m0 === separator) {
                endOfValue();
                break;
              }
              // skip un-delimited new-lines
              if (m0 === '\n' || m0 === '\r') {
                break;
              }
              // broken paser?
              throw new Error('CSVDataError: Illegal State [Row:' + options.state.rowNum + '][Col:' + options.state.colNum + ']');

            // un-delimited input
            case 3:
              // null last value
              if (m0 === separator) {
                endOfValue();
                break;
              }
              // skip un-delimited new-lines
              if (m0 === '\n' || m0 === '\r') {
                break;
              }
              // non-compliant data
              if (m0 === delimiter) {
                throw new Error('CSVDataError: Illegal Quote [Row:' + options.state.rowNum + '][Col:' + options.state.colNum + ']');
              }
              // broken parser?
              throw new Error('CSVDataError: Illegal Data [Row:' + options.state.rowNum + '][Col:' + options.state.colNum + ']');
            default:
              // shenanigans
              throw new Error('CSVDataError: Unknown State [Row:' + options.state.rowNum + '][Col:' + options.state.colNum + ']');
          }
          //console.log('val:' + m0 + ' state:' + state);
        });

        // submit the last value
        endOfValue();

        return entry;
      }
    },

    /**
     * $.csv.toArray(csv)
     * Converts a CSV entry string to a javascript array.
     *
     * @param {Array} csv The string containing the CSV data.
     * @param {Object} [options] An object containing user-defined options.
     * @param {Character} [separator] An override for the separator character. Defaults to a comma(,).
     * @param {Character} [delimiter] An override for the delimiter character. Defaults to a double-quote(").
     *
     * This method deals with simple CSV strings only. It's useful if you only
     * need to parse a single entry. If you need to parse more than one line,
     * use $.csv2Array instead.
     */
    toArray: function(csv, options, callback) {
      var options = (options !== undefined ? options : {});
      var config = {};
      config.callback = ((callback !== undefined && typeof(callback) === 'function') ? callback : false);
      config.separator = 'separator' in options ? options.separator : $.csv.defaults.separator;
      config.delimiter = 'delimiter' in options ? options.delimiter : $.csv.defaults.delimiter;
      var state = (options.state !== undefined ? options.state : {});

      // setup
      var options = {
        delimiter: config.delimiter,
        separator: config.separator,
        onParseEntry: options.onParseEntry,
        onParseValue: options.onParseValue,
        state: state
      }

      var entry = $.csv.parsers.parseEntry(csv, options);

      // push the value to a callback if one is defined
      if(!config.callback) {
        return entry;
      } else {
        config.callback('', entry);
      }
    },

    /**
     * $.csv.toArrays(csv)
     * Converts a CSV string to a javascript array.
     *
     * @param {String} csv The string containing the raw CSV data.
     * @param {Object} [options] An object containing user-defined options.
     * @param {Character} [separator] An override for the separator character. Defaults to a comma(,).
     * @param {Character} [delimiter] An override for the delimiter character. Defaults to a double-quote(").
     *
     * This method deals with multi-line CSV. The breakdown is simple. The first
     * dimension of the array represents the line (or entry/row) while the second
     * dimension contains the values (or values/columns).
     */
    toArrays: function(csv, options, callback) {
      var options = (options !== undefined ? options : {});
      var config = {};
      config.callback = ((callback !== undefined && typeof(callback) === 'function') ? callback : false);
      config.separator = 'separator' in options ? options.separator : $.csv.defaults.separator;
      config.delimiter = 'delimiter' in options ? options.delimiter : $.csv.defaults.delimiter;
      
      // setup
      var data = [];
      var options = {
        delimiter: config.delimiter,
        separator: config.separator,
        onParseEntry: options.onParseEntry,
        onParseValue: options.onParseValue,
        start: options.start,
        end: options.end,
        state: {
          rowNum: 1,
          colNum: 1
        }
      };

      // break the data down to lines
      data = $.csv.parsers.parse(csv, options);

      // push the value to a callback if one is defined
      if(!config.callback) {
        return data;
      } else {
        config.callback('', data);
      }
    },

    /**
     * $.csv.toObjects(csv)
     * Converts a CSV string to a javascript object.
     * @param {String} csv The string containing the raw CSV data.
     * @param {Object} [options] An object containing user-defined options.
     * @param {Character} [separator] An override for the separator character. Defaults to a comma(,).
     * @param {Character} [delimiter] An override for the delimiter character. Defaults to a double-quote(").
     * @param {Boolean} [headers] Indicates whether the data contains a header line. Defaults to true.
     *
     * This method deals with multi-line CSV strings. Where the headers line is
     * used as the key for each value per entry.
     */
    toObjects: function(csv, options, callback) {
      var options = (options !== undefined ? options : {});
      var config = {};
      config.callback = ((callback !== undefined && typeof(callback) === 'function') ? callback : false);
      config.separator = 'separator' in options ? options.separator : $.csv.defaults.separator;
      config.delimiter = 'delimiter' in options ? options.delimiter : $.csv.defaults.delimiter;
      config.headers = 'headers' in options ? options.headers : $.csv.defaults.headers;
      options.start = 'start' in options ? options.start : 1;
      
      // account for headers
      if(config.headers) {
        options.start++;
      }
      if(options.end && config.headers) {
        options.end++;
      }
      
      // setup
      var lines = [];
      var data = [];
      
      var options = {
        delimiter: config.delimiter,
        separator: config.separator,
        onParseEntry: options.onParseEntry,
        onParseValue: options.onParseValue,
        start: options.start,
        end: options.end,
        state: {
          rowNum: 1,
          colNum: 1
        },
        match: false
      };

      // fetch the headers
      var headerOptions = {
        delimiter: config.delimiter,
        separator: config.separator,
        start: 1,
        end: 1,
        state: {
          rowNum:1,
          colNum:1
        }
      }
      var headerLine = $.csv.parsers.splitLines(csv, headerOptions);
      var headers = $.csv.toArray(headerLine[0], options);

      // fetch the data
      var lines = $.csv.parsers.splitLines(csv, options);
      
      // reset the state for re-use
      options.state.colNum = 1;
      if(headers){
        options.state.rowNum = 2;
      } else {
        options.state.rowNum = 1;
      }
      
      // convert data to objects
      for(var i=0, len=lines.length; i<len; i++) {
        var entry = $.csv.toArray(lines[i], options);
        var object = {};
        for(var j in headers) {
          object[headers[j]] = entry[j];
        }
        data.push(object);
        
        // update row state
        options.state.rowNum++;
      }

      // push the value to a callback if one is defined
      if(!config.callback) {
        return data;
      } else {
        config.callback('', data);
      }
    },

     /**
     * $.csv.fromArrays(arrays)
     * Converts a javascript array to a CSV String.
     *
     * @param {Array} array An array containing an array of CSV entries.
     * @param {Object} [options] An object containing user-defined options.
     * @param {Character} [separator] An override for the separator character. Defaults to a comma(,).
     * @param {Character} [delimiter] An override for the delimiter character. Defaults to a double-quote(").
     *
     * This method generates a CSV file from an array of arrays (representing entries).
     */
    fromArrays: function(arrays, options, callback) {
      var options = (options !== undefined ? options : {});
      var config = {};
      config.callback = ((callback !== undefined && typeof(callback) === 'function') ? callback : false);
      config.separator = 'separator' in options ? options.separator : $.csv.defaults.separator;
      config.delimiter = 'delimiter' in options ? options.delimiter : $.csv.defaults.delimiter;
      config.escaper = 'escaper' in options ? options.escaper : $.csv.defaults.escaper;
      config.experimental = 'experimental' in options ? options.experimental : false;

      if(!config.experimental) {
        throw new Error('not implemented');
      }

      var output = [];
      for(i in arrays) {
        output.push(arrays[i]);
      }

      // push the value to a callback if one is defined
      if(!config.callback) {
        return output;
      } else {
        config.callback('', output);
      }
    },

    /**
     * $.csv.fromObjects(objects)
     * Converts a javascript dictionary to a CSV string.
     * @param {Object} objects An array of objects containing the data.
     * @param {Object} [options] An object containing user-defined options.
     * @param {Character} [separator] An override for the separator character. Defaults to a comma(,).
     * @param {Character} [delimiter] An override for the delimiter character. Defaults to a double-quote(").
     *
     * This method generates a CSV file from an array of objects (name:value pairs).
     * It starts by detecting the headers and adding them as the first line of
     * the CSV file, followed by a structured dump of the data.
     */
    fromObjects2CSV: function(objects, options, callback) {
      var options = (options !== undefined ? options : {});
      var config = {};
      config.callback = ((callback !== undefined && typeof(callback) === 'function') ? callback : false);
      config.separator = 'separator' in options ? options.separator : $.csv.defaults.separator;
      config.delimiter = 'delimiter' in options ? options.delimiter : $.csv.defaults.delimiter;
      config.experimental = 'experimental' in options ? options.experimental : false;

      if(!config.experimental) {
        throw new Error('not implemented');
      }

      var output = [];
      for(i in objects) {
        output.push(arrays[i]);
      }

      // push the value to a callback if one is defined
      if(!config.callback) {
        return output;
      } else {
        config.callback('', output);
      }
    }
  };

  // Maintenance code to maintain backward-compatibility
  // Will be removed in release 1.0
  $.csvEntry2Array = $.csv.toArray;
  $.csv2Array = $.csv.toArrays;
  $.csv2Dictionary = $.csv.toObjects;



},{"jquery":undefined}],4:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

function EventEmitter() {
  this._events = this._events || {};
  this._maxListeners = this._maxListeners || undefined;
}
module.exports = EventEmitter;

// Backwards-compat with node 0.10.x
EventEmitter.EventEmitter = EventEmitter;

EventEmitter.prototype._events = undefined;
EventEmitter.prototype._maxListeners = undefined;

// By default EventEmitters will print a warning if more than 10 listeners are
// added to it. This is a useful default which helps finding memory leaks.
EventEmitter.defaultMaxListeners = 10;

// Obviously not all Emitters should be limited to 10. This function allows
// that to be increased. Set to zero for unlimited.
EventEmitter.prototype.setMaxListeners = function(n) {
  if (!isNumber(n) || n < 0 || isNaN(n))
    throw TypeError('n must be a positive number');
  this._maxListeners = n;
  return this;
};

EventEmitter.prototype.emit = function(type) {
  var er, handler, len, args, i, listeners;

  if (!this._events)
    this._events = {};

  // If there is no 'error' event listener then throw.
  if (type === 'error') {
    if (!this._events.error ||
        (isObject(this._events.error) && !this._events.error.length)) {
      er = arguments[1];
      if (er instanceof Error) {
        throw er; // Unhandled 'error' event
      }
      throw TypeError('Uncaught, unspecified "error" event.');
    }
  }

  handler = this._events[type];

  if (isUndefined(handler))
    return false;

  if (isFunction(handler)) {
    switch (arguments.length) {
      // fast cases
      case 1:
        handler.call(this);
        break;
      case 2:
        handler.call(this, arguments[1]);
        break;
      case 3:
        handler.call(this, arguments[1], arguments[2]);
        break;
      // slower
      default:
        len = arguments.length;
        args = new Array(len - 1);
        for (i = 1; i < len; i++)
          args[i - 1] = arguments[i];
        handler.apply(this, args);
    }
  } else if (isObject(handler)) {
    len = arguments.length;
    args = new Array(len - 1);
    for (i = 1; i < len; i++)
      args[i - 1] = arguments[i];

    listeners = handler.slice();
    len = listeners.length;
    for (i = 0; i < len; i++)
      listeners[i].apply(this, args);
  }

  return true;
};

EventEmitter.prototype.addListener = function(type, listener) {
  var m;

  if (!isFunction(listener))
    throw TypeError('listener must be a function');

  if (!this._events)
    this._events = {};

  // To avoid recursion in the case that type === "newListener"! Before
  // adding it to the listeners, first emit "newListener".
  if (this._events.newListener)
    this.emit('newListener', type,
              isFunction(listener.listener) ?
              listener.listener : listener);

  if (!this._events[type])
    // Optimize the case of one listener. Don't need the extra array object.
    this._events[type] = listener;
  else if (isObject(this._events[type]))
    // If we've already got an array, just append.
    this._events[type].push(listener);
  else
    // Adding the second element, need to change to array.
    this._events[type] = [this._events[type], listener];

  // Check for listener leak
  if (isObject(this._events[type]) && !this._events[type].warned) {
    var m;
    if (!isUndefined(this._maxListeners)) {
      m = this._maxListeners;
    } else {
      m = EventEmitter.defaultMaxListeners;
    }

    if (m && m > 0 && this._events[type].length > m) {
      this._events[type].warned = true;
      console.error('(node) warning: possible EventEmitter memory ' +
                    'leak detected. %d listeners added. ' +
                    'Use emitter.setMaxListeners() to increase limit.',
                    this._events[type].length);
      if (typeof console.trace === 'function') {
        // not supported in IE 10
        console.trace();
      }
    }
  }

  return this;
};

EventEmitter.prototype.on = EventEmitter.prototype.addListener;

EventEmitter.prototype.once = function(type, listener) {
  if (!isFunction(listener))
    throw TypeError('listener must be a function');

  var fired = false;

  function g() {
    this.removeListener(type, g);

    if (!fired) {
      fired = true;
      listener.apply(this, arguments);
    }
  }

  g.listener = listener;
  this.on(type, g);

  return this;
};

// emits a 'removeListener' event iff the listener was removed
EventEmitter.prototype.removeListener = function(type, listener) {
  var list, position, length, i;

  if (!isFunction(listener))
    throw TypeError('listener must be a function');

  if (!this._events || !this._events[type])
    return this;

  list = this._events[type];
  length = list.length;
  position = -1;

  if (list === listener ||
      (isFunction(list.listener) && list.listener === listener)) {
    delete this._events[type];
    if (this._events.removeListener)
      this.emit('removeListener', type, listener);

  } else if (isObject(list)) {
    for (i = length; i-- > 0;) {
      if (list[i] === listener ||
          (list[i].listener && list[i].listener === listener)) {
        position = i;
        break;
      }
    }

    if (position < 0)
      return this;

    if (list.length === 1) {
      list.length = 0;
      delete this._events[type];
    } else {
      list.splice(position, 1);
    }

    if (this._events.removeListener)
      this.emit('removeListener', type, listener);
  }

  return this;
};

EventEmitter.prototype.removeAllListeners = function(type) {
  var key, listeners;

  if (!this._events)
    return this;

  // not listening for removeListener, no need to emit
  if (!this._events.removeListener) {
    if (arguments.length === 0)
      this._events = {};
    else if (this._events[type])
      delete this._events[type];
    return this;
  }

  // emit removeListener for all listeners on all events
  if (arguments.length === 0) {
    for (key in this._events) {
      if (key === 'removeListener') continue;
      this.removeAllListeners(key);
    }
    this.removeAllListeners('removeListener');
    this._events = {};
    return this;
  }

  listeners = this._events[type];

  if (isFunction(listeners)) {
    this.removeListener(type, listeners);
  } else {
    // LIFO order
    while (listeners.length)
      this.removeListener(type, listeners[listeners.length - 1]);
  }
  delete this._events[type];

  return this;
};

EventEmitter.prototype.listeners = function(type) {
  var ret;
  if (!this._events || !this._events[type])
    ret = [];
  else if (isFunction(this._events[type]))
    ret = [this._events[type]];
  else
    ret = this._events[type].slice();
  return ret;
};

EventEmitter.listenerCount = function(emitter, type) {
  var ret;
  if (!emitter._events || !emitter._events[type])
    ret = 0;
  else if (isFunction(emitter._events[type]))
    ret = 1;
  else
    ret = emitter._events[type].length;
  return ret;
};

function isFunction(arg) {
  return typeof arg === 'function';
}

function isNumber(arg) {
  return typeof arg === 'number';
}

function isObject(arg) {
  return typeof arg === 'object' && arg !== null;
}

function isUndefined(arg) {
  return arg === void 0;
}

},{}],5:[function(require,module,exports){
// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod((function(){try{return require('codemirror')}catch(e){return window.CodeMirror}})());
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
  var ie_lt8 = /MSIE \d/.test(navigator.userAgent) &&
    (document.documentMode == null || document.documentMode < 8);

  var Pos = CodeMirror.Pos;

  var matching = {"(": ")>", ")": "(<", "[": "]>", "]": "[<", "{": "}>", "}": "{<"};

  function findMatchingBracket(cm, where, strict, config) {
    var line = cm.getLineHandle(where.line), pos = where.ch - 1;
    var match = (pos >= 0 && matching[line.text.charAt(pos)]) || matching[line.text.charAt(++pos)];
    if (!match) return null;
    var dir = match.charAt(1) == ">" ? 1 : -1;
    if (strict && (dir > 0) != (pos == where.ch)) return null;
    var style = cm.getTokenTypeAt(Pos(where.line, pos + 1));

    var found = scanForBracket(cm, Pos(where.line, pos + (dir > 0 ? 1 : 0)), dir, style || null, config);
    if (found == null) return null;
    return {from: Pos(where.line, pos), to: found && found.pos,
            match: found && found.ch == match.charAt(0), forward: dir > 0};
  }

  // bracketRegex is used to specify which type of bracket to scan
  // should be a regexp, e.g. /[[\]]/
  //
  // Note: If "where" is on an open bracket, then this bracket is ignored.
  //
  // Returns false when no bracket was found, null when it reached
  // maxScanLines and gave up
  function scanForBracket(cm, where, dir, style, config) {
    var maxScanLen = (config && config.maxScanLineLength) || 10000;
    var maxScanLines = (config && config.maxScanLines) || 1000;

    var stack = [];
    var re = config && config.bracketRegex ? config.bracketRegex : /[(){}[\]]/;
    var lineEnd = dir > 0 ? Math.min(where.line + maxScanLines, cm.lastLine() + 1)
                          : Math.max(cm.firstLine() - 1, where.line - maxScanLines);
    for (var lineNo = where.line; lineNo != lineEnd; lineNo += dir) {
      var line = cm.getLine(lineNo);
      if (!line) continue;
      var pos = dir > 0 ? 0 : line.length - 1, end = dir > 0 ? line.length : -1;
      if (line.length > maxScanLen) continue;
      if (lineNo == where.line) pos = where.ch - (dir < 0 ? 1 : 0);
      for (; pos != end; pos += dir) {
        var ch = line.charAt(pos);
        if (re.test(ch) && (style === undefined || cm.getTokenTypeAt(Pos(lineNo, pos + 1)) == style)) {
          var match = matching[ch];
          if ((match.charAt(1) == ">") == (dir > 0)) stack.push(ch);
          else if (!stack.length) return {pos: Pos(lineNo, pos), ch: ch};
          else stack.pop();
        }
      }
    }
    return lineNo - dir == (dir > 0 ? cm.lastLine() : cm.firstLine()) ? false : null;
  }

  function matchBrackets(cm, autoclear, config) {
    // Disable brace matching in long lines, since it'll cause hugely slow updates
    var maxHighlightLen = cm.state.matchBrackets.maxHighlightLineLength || 1000;
    var marks = [], ranges = cm.listSelections();
    for (var i = 0; i < ranges.length; i++) {
      var match = ranges[i].empty() && findMatchingBracket(cm, ranges[i].head, false, config);
      if (match && cm.getLine(match.from.line).length <= maxHighlightLen) {
        var style = match.match ? "CodeMirror-matchingbracket" : "CodeMirror-nonmatchingbracket";
        marks.push(cm.markText(match.from, Pos(match.from.line, match.from.ch + 1), {className: style}));
        if (match.to && cm.getLine(match.to.line).length <= maxHighlightLen)
          marks.push(cm.markText(match.to, Pos(match.to.line, match.to.ch + 1), {className: style}));
      }
    }

    if (marks.length) {
      // Kludge to work around the IE bug from issue #1193, where text
      // input stops going to the textare whever this fires.
      if (ie_lt8 && cm.state.focused) cm.display.input.focus();

      var clear = function() {
        cm.operation(function() {
          for (var i = 0; i < marks.length; i++) marks[i].clear();
        });
      };
      if (autoclear) setTimeout(clear, 800);
      else return clear;
    }
  }

  var currentlyHighlighted = null;
  function doMatchBrackets(cm) {
    cm.operation(function() {
      if (currentlyHighlighted) {currentlyHighlighted(); currentlyHighlighted = null;}
      currentlyHighlighted = matchBrackets(cm, false, cm.state.matchBrackets);
    });
  }

  CodeMirror.defineOption("matchBrackets", false, function(cm, val, old) {
    if (old && old != CodeMirror.Init)
      cm.off("cursorActivity", doMatchBrackets);
    if (val) {
      cm.state.matchBrackets = typeof val == "object" ? val : {};
      cm.on("cursorActivity", doMatchBrackets);
    }
  });

  CodeMirror.defineExtension("matchBrackets", function() {matchBrackets(this, true);});
  CodeMirror.defineExtension("findMatchingBracket", function(pos, strict, config){
    return findMatchingBracket(this, pos, strict, config);
  });
  CodeMirror.defineExtension("scanForBracket", function(pos, dir, style, config){
    return scanForBracket(this, pos, dir, style, config);
  });
});

},{"codemirror":undefined}],6:[function(require,module,exports){
// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod((function(){try{return require('codemirror')}catch(e){return window.CodeMirror}})());
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

CodeMirror.registerHelper("fold", "brace", function(cm, start) {
  var line = start.line, lineText = cm.getLine(line);
  var startCh, tokenType;

  function findOpening(openCh) {
    for (var at = start.ch, pass = 0;;) {
      var found = at <= 0 ? -1 : lineText.lastIndexOf(openCh, at - 1);
      if (found == -1) {
        if (pass == 1) break;
        pass = 1;
        at = lineText.length;
        continue;
      }
      if (pass == 1 && found < start.ch) break;
      tokenType = cm.getTokenTypeAt(CodeMirror.Pos(line, found + 1));
      if (!/^(comment|string)/.test(tokenType)) return found + 1;
      at = found - 1;
    }
  }

  var startToken = "{", endToken = "}", startCh = findOpening("{");
  if (startCh == null) {
    startToken = "[", endToken = "]";
    startCh = findOpening("[");
  }

  if (startCh == null) return;
  var count = 1, lastLine = cm.lastLine(), end, endCh;
  outer: for (var i = line; i <= lastLine; ++i) {
    var text = cm.getLine(i), pos = i == line ? startCh : 0;
    for (;;) {
      var nextOpen = text.indexOf(startToken, pos), nextClose = text.indexOf(endToken, pos);
      if (nextOpen < 0) nextOpen = text.length;
      if (nextClose < 0) nextClose = text.length;
      pos = Math.min(nextOpen, nextClose);
      if (pos == text.length) break;
      if (cm.getTokenTypeAt(CodeMirror.Pos(i, pos + 1)) == tokenType) {
        if (pos == nextOpen) ++count;
        else if (!--count) { end = i; endCh = pos; break outer; }
      }
      ++pos;
    }
  }
  if (end == null || line == end && endCh == startCh) return;
  return {from: CodeMirror.Pos(line, startCh),
          to: CodeMirror.Pos(end, endCh)};
});

CodeMirror.registerHelper("fold", "import", function(cm, start) {
  function hasImport(line) {
    if (line < cm.firstLine() || line > cm.lastLine()) return null;
    var start = cm.getTokenAt(CodeMirror.Pos(line, 1));
    if (!/\S/.test(start.string)) start = cm.getTokenAt(CodeMirror.Pos(line, start.end + 1));
    if (start.type != "keyword" || start.string != "import") return null;
    // Now find closing semicolon, return its position
    for (var i = line, e = Math.min(cm.lastLine(), line + 10); i <= e; ++i) {
      var text = cm.getLine(i), semi = text.indexOf(";");
      if (semi != -1) return {startCh: start.end, end: CodeMirror.Pos(i, semi)};
    }
  }

  var start = start.line, has = hasImport(start), prev;
  if (!has || hasImport(start - 1) || ((prev = hasImport(start - 2)) && prev.end.line == start - 1))
    return null;
  for (var end = has.end;;) {
    var next = hasImport(end.line + 1);
    if (next == null) break;
    end = next.end;
  }
  return {from: cm.clipPos(CodeMirror.Pos(start, has.startCh + 1)), to: end};
});

CodeMirror.registerHelper("fold", "include", function(cm, start) {
  function hasInclude(line) {
    if (line < cm.firstLine() || line > cm.lastLine()) return null;
    var start = cm.getTokenAt(CodeMirror.Pos(line, 1));
    if (!/\S/.test(start.string)) start = cm.getTokenAt(CodeMirror.Pos(line, start.end + 1));
    if (start.type == "meta" && start.string.slice(0, 8) == "#include") return start.start + 8;
  }

  var start = start.line, has = hasInclude(start);
  if (has == null || hasInclude(start - 1) != null) return null;
  for (var end = start;;) {
    var next = hasInclude(end + 1);
    if (next == null) break;
    ++end;
  }
  return {from: CodeMirror.Pos(start, has + 1),
          to: cm.clipPos(CodeMirror.Pos(end))};
});

});

},{"codemirror":undefined}],7:[function(require,module,exports){
// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod((function(){try{return require('codemirror')}catch(e){return window.CodeMirror}})());
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
  "use strict";

  function doFold(cm, pos, options, force) {
    if (options && options.call) {
      var finder = options;
      options = null;
    } else {
      var finder = getOption(cm, options, "rangeFinder");
    }
    if (typeof pos == "number") pos = CodeMirror.Pos(pos, 0);
    var minSize = getOption(cm, options, "minFoldSize");

    function getRange(allowFolded) {
      var range = finder(cm, pos);
      if (!range || range.to.line - range.from.line < minSize) return null;
      var marks = cm.findMarksAt(range.from);
      for (var i = 0; i < marks.length; ++i) {
        if (marks[i].__isFold && force !== "fold") {
          if (!allowFolded) return null;
          range.cleared = true;
          marks[i].clear();
        }
      }
      return range;
    }

    var range = getRange(true);
    if (getOption(cm, options, "scanUp")) while (!range && pos.line > cm.firstLine()) {
      pos = CodeMirror.Pos(pos.line - 1, 0);
      range = getRange(false);
    }
    if (!range || range.cleared || force === "unfold") return;

    var myWidget = makeWidget(cm, options);
    CodeMirror.on(myWidget, "mousedown", function(e) {
      myRange.clear();
      CodeMirror.e_preventDefault(e);
    });
    var myRange = cm.markText(range.from, range.to, {
      replacedWith: myWidget,
      clearOnEnter: true,
      __isFold: true
    });
    myRange.on("clear", function(from, to) {
      CodeMirror.signal(cm, "unfold", cm, from, to);
    });
    CodeMirror.signal(cm, "fold", cm, range.from, range.to);
  }

  function makeWidget(cm, options) {
    var widget = getOption(cm, options, "widget");
    if (typeof widget == "string") {
      var text = document.createTextNode(widget);
      widget = document.createElement("span");
      widget.appendChild(text);
      widget.className = "CodeMirror-foldmarker";
    }
    return widget;
  }

  // Clumsy backwards-compatible interface
  CodeMirror.newFoldFunction = function(rangeFinder, widget) {
    return function(cm, pos) { doFold(cm, pos, {rangeFinder: rangeFinder, widget: widget}); };
  };

  // New-style interface
  CodeMirror.defineExtension("foldCode", function(pos, options, force) {
    doFold(this, pos, options, force);
  });

  CodeMirror.defineExtension("isFolded", function(pos) {
    var marks = this.findMarksAt(pos);
    for (var i = 0; i < marks.length; ++i)
      if (marks[i].__isFold) return true;
  });

  CodeMirror.commands.toggleFold = function(cm) {
    cm.foldCode(cm.getCursor());
  };
  CodeMirror.commands.fold = function(cm) {
    cm.foldCode(cm.getCursor(), null, "fold");
  };
  CodeMirror.commands.unfold = function(cm) {
    cm.foldCode(cm.getCursor(), null, "unfold");
  };
  CodeMirror.commands.foldAll = function(cm) {
    cm.operation(function() {
      for (var i = cm.firstLine(), e = cm.lastLine(); i <= e; i++)
        cm.foldCode(CodeMirror.Pos(i, 0), null, "fold");
    });
  };
  CodeMirror.commands.unfoldAll = function(cm) {
    cm.operation(function() {
      for (var i = cm.firstLine(), e = cm.lastLine(); i <= e; i++)
        cm.foldCode(CodeMirror.Pos(i, 0), null, "unfold");
    });
  };

  CodeMirror.registerHelper("fold", "combine", function() {
    var funcs = Array.prototype.slice.call(arguments, 0);
    return function(cm, start) {
      for (var i = 0; i < funcs.length; ++i) {
        var found = funcs[i](cm, start);
        if (found) return found;
      }
    };
  });

  CodeMirror.registerHelper("fold", "auto", function(cm, start) {
    var helpers = cm.getHelpers(start, "fold");
    for (var i = 0; i < helpers.length; i++) {
      var cur = helpers[i](cm, start);
      if (cur) return cur;
    }
  });

  var defaultOptions = {
    rangeFinder: CodeMirror.fold.auto,
    widget: "\u2194",
    minFoldSize: 0,
    scanUp: false
  };

  CodeMirror.defineOption("foldOptions", null);

  function getOption(cm, options, name) {
    if (options && options[name] !== undefined)
      return options[name];
    var editorOptions = cm.options.foldOptions;
    if (editorOptions && editorOptions[name] !== undefined)
      return editorOptions[name];
    return defaultOptions[name];
  }

  CodeMirror.defineExtension("foldOption", function(options, name) {
    return getOption(this, options, name);
  });
});

},{"codemirror":undefined}],8:[function(require,module,exports){
// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod((function(){try{return require('codemirror')}catch(e){return window.CodeMirror}})(), require("./foldcode"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror", "./foldcode"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
  "use strict";

  CodeMirror.defineOption("foldGutter", false, function(cm, val, old) {
    if (old && old != CodeMirror.Init) {
      cm.clearGutter(cm.state.foldGutter.options.gutter);
      cm.state.foldGutter = null;
      cm.off("gutterClick", onGutterClick);
      cm.off("change", onChange);
      cm.off("viewportChange", onViewportChange);
      cm.off("fold", onFold);
      cm.off("unfold", onFold);
      cm.off("swapDoc", updateInViewport);
    }
    if (val) {
      cm.state.foldGutter = new State(parseOptions(val));
      updateInViewport(cm);
      cm.on("gutterClick", onGutterClick);
      cm.on("change", onChange);
      cm.on("viewportChange", onViewportChange);
      cm.on("fold", onFold);
      cm.on("unfold", onFold);
      cm.on("swapDoc", updateInViewport);
    }
  });

  var Pos = CodeMirror.Pos;

  function State(options) {
    this.options = options;
    this.from = this.to = 0;
  }

  function parseOptions(opts) {
    if (opts === true) opts = {};
    if (opts.gutter == null) opts.gutter = "CodeMirror-foldgutter";
    if (opts.indicatorOpen == null) opts.indicatorOpen = "CodeMirror-foldgutter-open";
    if (opts.indicatorFolded == null) opts.indicatorFolded = "CodeMirror-foldgutter-folded";
    return opts;
  }

  function isFolded(cm, line) {
    var marks = cm.findMarksAt(Pos(line));
    for (var i = 0; i < marks.length; ++i)
      if (marks[i].__isFold && marks[i].find().from.line == line) return true;
  }

  function marker(spec) {
    if (typeof spec == "string") {
      var elt = document.createElement("div");
      elt.className = spec + " CodeMirror-guttermarker-subtle";
      return elt;
    } else {
      return spec.cloneNode(true);
    }
  }

  function updateFoldInfo(cm, from, to) {
    var opts = cm.state.foldGutter.options, cur = from;
    var minSize = cm.foldOption(opts, "minFoldSize");
    var func = cm.foldOption(opts, "rangeFinder");
    cm.eachLine(from, to, function(line) {
      var mark = null;
      if (isFolded(cm, cur)) {
        mark = marker(opts.indicatorFolded);
      } else {
        var pos = Pos(cur, 0);
        var range = func && func(cm, pos);
        if (range && range.to.line - range.from.line >= minSize)
          mark = marker(opts.indicatorOpen);
      }
      cm.setGutterMarker(line, opts.gutter, mark);
      ++cur;
    });
  }

  function updateInViewport(cm) {
    var vp = cm.getViewport(), state = cm.state.foldGutter;
    if (!state) return;
    cm.operation(function() {
      updateFoldInfo(cm, vp.from, vp.to);
    });
    state.from = vp.from; state.to = vp.to;
  }

  function onGutterClick(cm, line, gutter) {
    var state = cm.state.foldGutter;
    if (!state) return;
    var opts = state.options;
    if (gutter != opts.gutter) return;
    cm.foldCode(Pos(line, 0), opts.rangeFinder);
  }

  function onChange(cm) {
    var state = cm.state.foldGutter;
    if (!state) return;
    var opts = state.options;
    state.from = state.to = 0;
    clearTimeout(state.changeUpdate);
    state.changeUpdate = setTimeout(function() { updateInViewport(cm); }, opts.foldOnChangeTimeSpan || 600);
  }

  function onViewportChange(cm) {
    var state = cm.state.foldGutter;
    if (!state) return;
    var opts = state.options;
    clearTimeout(state.changeUpdate);
    state.changeUpdate = setTimeout(function() {
      var vp = cm.getViewport();
      if (state.from == state.to || vp.from - state.to > 20 || state.from - vp.to > 20) {
        updateInViewport(cm);
      } else {
        cm.operation(function() {
          if (vp.from < state.from) {
            updateFoldInfo(cm, vp.from, state.from);
            state.from = vp.from;
          }
          if (vp.to > state.to) {
            updateFoldInfo(cm, state.to, vp.to);
            state.to = vp.to;
          }
        });
      }
    }, opts.updateViewportTimeSpan || 400);
  }

  function onFold(cm, from) {
    var state = cm.state.foldGutter;
    if (!state) return;
    var line = from.line;
    if (line >= state.from && line < state.to)
      updateFoldInfo(cm, line, line + 1);
  }
});

},{"./foldcode":7,"codemirror":undefined}],9:[function(require,module,exports){
// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod((function(){try{return require('codemirror')}catch(e){return window.CodeMirror}})());
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
  "use strict";

  var Pos = CodeMirror.Pos;
  function cmp(a, b) { return a.line - b.line || a.ch - b.ch; }

  var nameStartChar = "A-Z_a-z\\u00C0-\\u00D6\\u00D8-\\u00F6\\u00F8-\\u02FF\\u0370-\\u037D\\u037F-\\u1FFF\\u200C-\\u200D\\u2070-\\u218F\\u2C00-\\u2FEF\\u3001-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFFD";
  var nameChar = nameStartChar + "\-\:\.0-9\\u00B7\\u0300-\\u036F\\u203F-\\u2040";
  var xmlTagStart = new RegExp("<(/?)([" + nameStartChar + "][" + nameChar + "]*)", "g");

  function Iter(cm, line, ch, range) {
    this.line = line; this.ch = ch;
    this.cm = cm; this.text = cm.getLine(line);
    this.min = range ? range.from : cm.firstLine();
    this.max = range ? range.to - 1 : cm.lastLine();
  }

  function tagAt(iter, ch) {
    var type = iter.cm.getTokenTypeAt(Pos(iter.line, ch));
    return type && /\btag\b/.test(type);
  }

  function nextLine(iter) {
    if (iter.line >= iter.max) return;
    iter.ch = 0;
    iter.text = iter.cm.getLine(++iter.line);
    return true;
  }
  function prevLine(iter) {
    if (iter.line <= iter.min) return;
    iter.text = iter.cm.getLine(--iter.line);
    iter.ch = iter.text.length;
    return true;
  }

  function toTagEnd(iter) {
    for (;;) {
      var gt = iter.text.indexOf(">", iter.ch);
      if (gt == -1) { if (nextLine(iter)) continue; else return; }
      if (!tagAt(iter, gt + 1)) { iter.ch = gt + 1; continue; }
      var lastSlash = iter.text.lastIndexOf("/", gt);
      var selfClose = lastSlash > -1 && !/\S/.test(iter.text.slice(lastSlash + 1, gt));
      iter.ch = gt + 1;
      return selfClose ? "selfClose" : "regular";
    }
  }
  function toTagStart(iter) {
    for (;;) {
      var lt = iter.ch ? iter.text.lastIndexOf("<", iter.ch - 1) : -1;
      if (lt == -1) { if (prevLine(iter)) continue; else return; }
      if (!tagAt(iter, lt + 1)) { iter.ch = lt; continue; }
      xmlTagStart.lastIndex = lt;
      iter.ch = lt;
      var match = xmlTagStart.exec(iter.text);
      if (match && match.index == lt) return match;
    }
  }

  function toNextTag(iter) {
    for (;;) {
      xmlTagStart.lastIndex = iter.ch;
      var found = xmlTagStart.exec(iter.text);
      if (!found) { if (nextLine(iter)) continue; else return; }
      if (!tagAt(iter, found.index + 1)) { iter.ch = found.index + 1; continue; }
      iter.ch = found.index + found[0].length;
      return found;
    }
  }
  function toPrevTag(iter) {
    for (;;) {
      var gt = iter.ch ? iter.text.lastIndexOf(">", iter.ch - 1) : -1;
      if (gt == -1) { if (prevLine(iter)) continue; else return; }
      if (!tagAt(iter, gt + 1)) { iter.ch = gt; continue; }
      var lastSlash = iter.text.lastIndexOf("/", gt);
      var selfClose = lastSlash > -1 && !/\S/.test(iter.text.slice(lastSlash + 1, gt));
      iter.ch = gt + 1;
      return selfClose ? "selfClose" : "regular";
    }
  }

  function findMatchingClose(iter, tag) {
    var stack = [];
    for (;;) {
      var next = toNextTag(iter), end, startLine = iter.line, startCh = iter.ch - (next ? next[0].length : 0);
      if (!next || !(end = toTagEnd(iter))) return;
      if (end == "selfClose") continue;
      if (next[1]) { // closing tag
        for (var i = stack.length - 1; i >= 0; --i) if (stack[i] == next[2]) {
          stack.length = i;
          break;
        }
        if (i < 0 && (!tag || tag == next[2])) return {
          tag: next[2],
          from: Pos(startLine, startCh),
          to: Pos(iter.line, iter.ch)
        };
      } else { // opening tag
        stack.push(next[2]);
      }
    }
  }
  function findMatchingOpen(iter, tag) {
    var stack = [];
    for (;;) {
      var prev = toPrevTag(iter);
      if (!prev) return;
      if (prev == "selfClose") { toTagStart(iter); continue; }
      var endLine = iter.line, endCh = iter.ch;
      var start = toTagStart(iter);
      if (!start) return;
      if (start[1]) { // closing tag
        stack.push(start[2]);
      } else { // opening tag
        for (var i = stack.length - 1; i >= 0; --i) if (stack[i] == start[2]) {
          stack.length = i;
          break;
        }
        if (i < 0 && (!tag || tag == start[2])) return {
          tag: start[2],
          from: Pos(iter.line, iter.ch),
          to: Pos(endLine, endCh)
        };
      }
    }
  }

  CodeMirror.registerHelper("fold", "xml", function(cm, start) {
    var iter = new Iter(cm, start.line, 0);
    for (;;) {
      var openTag = toNextTag(iter), end;
      if (!openTag || iter.line != start.line || !(end = toTagEnd(iter))) return;
      if (!openTag[1] && end != "selfClose") {
        var start = Pos(iter.line, iter.ch);
        var close = findMatchingClose(iter, openTag[2]);
        return close && {from: start, to: close.from};
      }
    }
  });
  CodeMirror.findMatchingTag = function(cm, pos, range) {
    var iter = new Iter(cm, pos.line, pos.ch, range);
    if (iter.text.indexOf(">") == -1 && iter.text.indexOf("<") == -1) return;
    var end = toTagEnd(iter), to = end && Pos(iter.line, iter.ch);
    var start = end && toTagStart(iter);
    if (!end || !start || cmp(iter, pos) > 0) return;
    var here = {from: Pos(iter.line, iter.ch), to: to, tag: start[2]};
    if (end == "selfClose") return {open: here, close: null, at: "open"};

    if (start[1]) { // closing tag
      return {open: findMatchingOpen(iter, start[2]), close: here, at: "close"};
    } else { // opening tag
      iter = new Iter(cm, to.line, to.ch, range);
      return {open: here, close: findMatchingClose(iter, start[2]), at: "open"};
    }
  };

  CodeMirror.findEnclosingTag = function(cm, pos, range) {
    var iter = new Iter(cm, pos.line, pos.ch, range);
    for (;;) {
      var open = findMatchingOpen(iter);
      if (!open) break;
      var forward = new Iter(cm, pos.line, pos.ch, range);
      var close = findMatchingClose(forward, open.tag);
      if (close) return {open: open, close: close};
    }
  };

  // Used by addon/edit/closetag.js
  CodeMirror.scanForClosingTag = function(cm, pos, name, end) {
    var iter = new Iter(cm, pos.line, pos.ch, end ? {from: 0, to: end} : null);
    return findMatchingClose(iter, name);
  };
});

},{"codemirror":undefined}],10:[function(require,module,exports){
// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

// TODO actually recognize syntax of TypeScript constructs

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod((function(){try{return require('codemirror')}catch(e){return window.CodeMirror}})());
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

CodeMirror.defineMode("javascript", function(config, parserConfig) {
  var indentUnit = config.indentUnit;
  var statementIndent = parserConfig.statementIndent;
  var jsonldMode = parserConfig.jsonld;
  var jsonMode = parserConfig.json || jsonldMode;
  var isTS = parserConfig.typescript;
  var wordRE = parserConfig.wordCharacters || /[\w$\xa1-\uffff]/;

  // Tokenizer

  var keywords = function(){
    function kw(type) {return {type: type, style: "keyword"};}
    var A = kw("keyword a"), B = kw("keyword b"), C = kw("keyword c");
    var operator = kw("operator"), atom = {type: "atom", style: "atom"};

    var jsKeywords = {
      "if": kw("if"), "while": A, "with": A, "else": B, "do": B, "try": B, "finally": B,
      "return": C, "break": C, "continue": C, "new": C, "delete": C, "throw": C, "debugger": C,
      "var": kw("var"), "const": kw("var"), "let": kw("var"),
      "function": kw("function"), "catch": kw("catch"),
      "for": kw("for"), "switch": kw("switch"), "case": kw("case"), "default": kw("default"),
      "in": operator, "typeof": operator, "instanceof": operator,
      "true": atom, "false": atom, "null": atom, "undefined": atom, "NaN": atom, "Infinity": atom,
      "this": kw("this"), "module": kw("module"), "class": kw("class"), "super": kw("atom"),
      "yield": C, "export": kw("export"), "import": kw("import"), "extends": C
    };

    // Extend the 'normal' keywords with the TypeScript language extensions
    if (isTS) {
      var type = {type: "variable", style: "variable-3"};
      var tsKeywords = {
        // object-like things
        "interface": kw("interface"),
        "extends": kw("extends"),
        "constructor": kw("constructor"),

        // scope modifiers
        "public": kw("public"),
        "private": kw("private"),
        "protected": kw("protected"),
        "static": kw("static"),

        // types
        "string": type, "number": type, "bool": type, "any": type
      };

      for (var attr in tsKeywords) {
        jsKeywords[attr] = tsKeywords[attr];
      }
    }

    return jsKeywords;
  }();

  var isOperatorChar = /[+\-*&%=<>!?|~^]/;
  var isJsonldKeyword = /^@(context|id|value|language|type|container|list|set|reverse|index|base|vocab|graph)"/;

  function readRegexp(stream) {
    var escaped = false, next, inSet = false;
    while ((next = stream.next()) != null) {
      if (!escaped) {
        if (next == "/" && !inSet) return;
        if (next == "[") inSet = true;
        else if (inSet && next == "]") inSet = false;
      }
      escaped = !escaped && next == "\\";
    }
  }

  // Used as scratch variables to communicate multiple values without
  // consing up tons of objects.
  var type, content;
  function ret(tp, style, cont) {
    type = tp; content = cont;
    return style;
  }
  function tokenBase(stream, state) {
    var ch = stream.next();
    if (ch == '"' || ch == "'") {
      state.tokenize = tokenString(ch);
      return state.tokenize(stream, state);
    } else if (ch == "." && stream.match(/^\d+(?:[eE][+\-]?\d+)?/)) {
      return ret("number", "number");
    } else if (ch == "." && stream.match("..")) {
      return ret("spread", "meta");
    } else if (/[\[\]{}\(\),;\:\.]/.test(ch)) {
      return ret(ch);
    } else if (ch == "=" && stream.eat(">")) {
      return ret("=>", "operator");
    } else if (ch == "0" && stream.eat(/x/i)) {
      stream.eatWhile(/[\da-f]/i);
      return ret("number", "number");
    } else if (/\d/.test(ch)) {
      stream.match(/^\d*(?:\.\d*)?(?:[eE][+\-]?\d+)?/);
      return ret("number", "number");
    } else if (ch == "/") {
      if (stream.eat("*")) {
        state.tokenize = tokenComment;
        return tokenComment(stream, state);
      } else if (stream.eat("/")) {
        stream.skipToEnd();
        return ret("comment", "comment");
      } else if (state.lastType == "operator" || state.lastType == "keyword c" ||
               state.lastType == "sof" || /^[\[{}\(,;:]$/.test(state.lastType)) {
        readRegexp(stream);
        stream.match(/^\b(([gimyu])(?![gimyu]*\2))+\b/);
        return ret("regexp", "string-2");
      } else {
        stream.eatWhile(isOperatorChar);
        return ret("operator", "operator", stream.current());
      }
    } else if (ch == "`") {
      state.tokenize = tokenQuasi;
      return tokenQuasi(stream, state);
    } else if (ch == "#") {
      stream.skipToEnd();
      return ret("error", "error");
    } else if (isOperatorChar.test(ch)) {
      stream.eatWhile(isOperatorChar);
      return ret("operator", "operator", stream.current());
    } else if (wordRE.test(ch)) {
      stream.eatWhile(wordRE);
      var word = stream.current(), known = keywords.propertyIsEnumerable(word) && keywords[word];
      return (known && state.lastType != ".") ? ret(known.type, known.style, word) :
                     ret("variable", "variable", word);
    }
  }

  function tokenString(quote) {
    return function(stream, state) {
      var escaped = false, next;
      if (jsonldMode && stream.peek() == "@" && stream.match(isJsonldKeyword)){
        state.tokenize = tokenBase;
        return ret("jsonld-keyword", "meta");
      }
      while ((next = stream.next()) != null) {
        if (next == quote && !escaped) break;
        escaped = !escaped && next == "\\";
      }
      if (!escaped) state.tokenize = tokenBase;
      return ret("string", "string");
    };
  }

  function tokenComment(stream, state) {
    var maybeEnd = false, ch;
    while (ch = stream.next()) {
      if (ch == "/" && maybeEnd) {
        state.tokenize = tokenBase;
        break;
      }
      maybeEnd = (ch == "*");
    }
    return ret("comment", "comment");
  }

  function tokenQuasi(stream, state) {
    var escaped = false, next;
    while ((next = stream.next()) != null) {
      if (!escaped && (next == "`" || next == "$" && stream.eat("{"))) {
        state.tokenize = tokenBase;
        break;
      }
      escaped = !escaped && next == "\\";
    }
    return ret("quasi", "string-2", stream.current());
  }

  var brackets = "([{}])";
  // This is a crude lookahead trick to try and notice that we're
  // parsing the argument patterns for a fat-arrow function before we
  // actually hit the arrow token. It only works if the arrow is on
  // the same line as the arguments and there's no strange noise
  // (comments) in between. Fallback is to only notice when we hit the
  // arrow, and not declare the arguments as locals for the arrow
  // body.
  function findFatArrow(stream, state) {
    if (state.fatArrowAt) state.fatArrowAt = null;
    var arrow = stream.string.indexOf("=>", stream.start);
    if (arrow < 0) return;

    var depth = 0, sawSomething = false;
    for (var pos = arrow - 1; pos >= 0; --pos) {
      var ch = stream.string.charAt(pos);
      var bracket = brackets.indexOf(ch);
      if (bracket >= 0 && bracket < 3) {
        if (!depth) { ++pos; break; }
        if (--depth == 0) break;
      } else if (bracket >= 3 && bracket < 6) {
        ++depth;
      } else if (wordRE.test(ch)) {
        sawSomething = true;
      } else if (/["'\/]/.test(ch)) {
        return;
      } else if (sawSomething && !depth) {
        ++pos;
        break;
      }
    }
    if (sawSomething && !depth) state.fatArrowAt = pos;
  }

  // Parser

  var atomicTypes = {"atom": true, "number": true, "variable": true, "string": true, "regexp": true, "this": true, "jsonld-keyword": true};

  function JSLexical(indented, column, type, align, prev, info) {
    this.indented = indented;
    this.column = column;
    this.type = type;
    this.prev = prev;
    this.info = info;
    if (align != null) this.align = align;
  }

  function inScope(state, varname) {
    for (var v = state.localVars; v; v = v.next)
      if (v.name == varname) return true;
    for (var cx = state.context; cx; cx = cx.prev) {
      for (var v = cx.vars; v; v = v.next)
        if (v.name == varname) return true;
    }
  }

  function parseJS(state, style, type, content, stream) {
    var cc = state.cc;
    // Communicate our context to the combinators.
    // (Less wasteful than consing up a hundred closures on every call.)
    cx.state = state; cx.stream = stream; cx.marked = null, cx.cc = cc; cx.style = style;

    if (!state.lexical.hasOwnProperty("align"))
      state.lexical.align = true;

    while(true) {
      var combinator = cc.length ? cc.pop() : jsonMode ? expression : statement;
      if (combinator(type, content)) {
        while(cc.length && cc[cc.length - 1].lex)
          cc.pop()();
        if (cx.marked) return cx.marked;
        if (type == "variable" && inScope(state, content)) return "variable-2";
        return style;
      }
    }
  }

  // Combinator utils

  var cx = {state: null, column: null, marked: null, cc: null};
  function pass() {
    for (var i = arguments.length - 1; i >= 0; i--) cx.cc.push(arguments[i]);
  }
  function cont() {
    pass.apply(null, arguments);
    return true;
  }
  function register(varname) {
    function inList(list) {
      for (var v = list; v; v = v.next)
        if (v.name == varname) return true;
      return false;
    }
    var state = cx.state;
    if (state.context) {
      cx.marked = "def";
      if (inList(state.localVars)) return;
      state.localVars = {name: varname, next: state.localVars};
    } else {
      if (inList(state.globalVars)) return;
      if (parserConfig.globalVars)
        state.globalVars = {name: varname, next: state.globalVars};
    }
  }

  // Combinators

  var defaultVars = {name: "this", next: {name: "arguments"}};
  function pushcontext() {
    cx.state.context = {prev: cx.state.context, vars: cx.state.localVars};
    cx.state.localVars = defaultVars;
  }
  function popcontext() {
    cx.state.localVars = cx.state.context.vars;
    cx.state.context = cx.state.context.prev;
  }
  function pushlex(type, info) {
    var result = function() {
      var state = cx.state, indent = state.indented;
      if (state.lexical.type == "stat") indent = state.lexical.indented;
      else for (var outer = state.lexical; outer && outer.type == ")" && outer.align; outer = outer.prev)
        indent = outer.indented;
      state.lexical = new JSLexical(indent, cx.stream.column(), type, null, state.lexical, info);
    };
    result.lex = true;
    return result;
  }
  function poplex() {
    var state = cx.state;
    if (state.lexical.prev) {
      if (state.lexical.type == ")")
        state.indented = state.lexical.indented;
      state.lexical = state.lexical.prev;
    }
  }
  poplex.lex = true;

  function expect(wanted) {
    function exp(type) {
      if (type == wanted) return cont();
      else if (wanted == ";") return pass();
      else return cont(exp);
    };
    return exp;
  }

  function statement(type, value) {
    if (type == "var") return cont(pushlex("vardef", value.length), vardef, expect(";"), poplex);
    if (type == "keyword a") return cont(pushlex("form"), expression, statement, poplex);
    if (type == "keyword b") return cont(pushlex("form"), statement, poplex);
    if (type == "{") return cont(pushlex("}"), block, poplex);
    if (type == ";") return cont();
    if (type == "if") {
      if (cx.state.lexical.info == "else" && cx.state.cc[cx.state.cc.length - 1] == poplex)
        cx.state.cc.pop()();
      return cont(pushlex("form"), expression, statement, poplex, maybeelse);
    }
    if (type == "function") return cont(functiondef);
    if (type == "for") return cont(pushlex("form"), forspec, statement, poplex);
    if (type == "variable") return cont(pushlex("stat"), maybelabel);
    if (type == "switch") return cont(pushlex("form"), expression, pushlex("}", "switch"), expect("{"),
                                      block, poplex, poplex);
    if (type == "case") return cont(expression, expect(":"));
    if (type == "default") return cont(expect(":"));
    if (type == "catch") return cont(pushlex("form"), pushcontext, expect("("), funarg, expect(")"),
                                     statement, poplex, popcontext);
    if (type == "module") return cont(pushlex("form"), pushcontext, afterModule, popcontext, poplex);
    if (type == "class") return cont(pushlex("form"), className, poplex);
    if (type == "export") return cont(pushlex("form"), afterExport, poplex);
    if (type == "import") return cont(pushlex("form"), afterImport, poplex);
    return pass(pushlex("stat"), expression, expect(";"), poplex);
  }
  function expression(type) {
    return expressionInner(type, false);
  }
  function expressionNoComma(type) {
    return expressionInner(type, true);
  }
  function expressionInner(type, noComma) {
    if (cx.state.fatArrowAt == cx.stream.start) {
      var body = noComma ? arrowBodyNoComma : arrowBody;
      if (type == "(") return cont(pushcontext, pushlex(")"), commasep(pattern, ")"), poplex, expect("=>"), body, popcontext);
      else if (type == "variable") return pass(pushcontext, pattern, expect("=>"), body, popcontext);
    }

    var maybeop = noComma ? maybeoperatorNoComma : maybeoperatorComma;
    if (atomicTypes.hasOwnProperty(type)) return cont(maybeop);
    if (type == "function") return cont(functiondef, maybeop);
    if (type == "keyword c") return cont(noComma ? maybeexpressionNoComma : maybeexpression);
    if (type == "(") return cont(pushlex(")"), maybeexpression, comprehension, expect(")"), poplex, maybeop);
    if (type == "operator" || type == "spread") return cont(noComma ? expressionNoComma : expression);
    if (type == "[") return cont(pushlex("]"), arrayLiteral, poplex, maybeop);
    if (type == "{") return contCommasep(objprop, "}", null, maybeop);
    if (type == "quasi") { return pass(quasi, maybeop); }
    return cont();
  }
  function maybeexpression(type) {
    if (type.match(/[;\}\)\],]/)) return pass();
    return pass(expression);
  }
  function maybeexpressionNoComma(type) {
    if (type.match(/[;\}\)\],]/)) return pass();
    return pass(expressionNoComma);
  }

  function maybeoperatorComma(type, value) {
    if (type == ",") return cont(expression);
    return maybeoperatorNoComma(type, value, false);
  }
  function maybeoperatorNoComma(type, value, noComma) {
    var me = noComma == false ? maybeoperatorComma : maybeoperatorNoComma;
    var expr = noComma == false ? expression : expressionNoComma;
    if (type == "=>") return cont(pushcontext, noComma ? arrowBodyNoComma : arrowBody, popcontext);
    if (type == "operator") {
      if (/\+\+|--/.test(value)) return cont(me);
      if (value == "?") return cont(expression, expect(":"), expr);
      return cont(expr);
    }
    if (type == "quasi") { return pass(quasi, me); }
    if (type == ";") return;
    if (type == "(") return contCommasep(expressionNoComma, ")", "call", me);
    if (type == ".") return cont(property, me);
    if (type == "[") return cont(pushlex("]"), maybeexpression, expect("]"), poplex, me);
  }
  function quasi(type, value) {
    if (type != "quasi") return pass();
    if (value.slice(value.length - 2) != "${") return cont(quasi);
    return cont(expression, continueQuasi);
  }
  function continueQuasi(type) {
    if (type == "}") {
      cx.marked = "string-2";
      cx.state.tokenize = tokenQuasi;
      return cont(quasi);
    }
  }
  function arrowBody(type) {
    findFatArrow(cx.stream, cx.state);
    return pass(type == "{" ? statement : expression);
  }
  function arrowBodyNoComma(type) {
    findFatArrow(cx.stream, cx.state);
    return pass(type == "{" ? statement : expressionNoComma);
  }
  function maybelabel(type) {
    if (type == ":") return cont(poplex, statement);
    return pass(maybeoperatorComma, expect(";"), poplex);
  }
  function property(type) {
    if (type == "variable") {cx.marked = "property"; return cont();}
  }
  function objprop(type, value) {
    if (type == "variable" || cx.style == "keyword") {
      cx.marked = "property";
      if (value == "get" || value == "set") return cont(getterSetter);
      return cont(afterprop);
    } else if (type == "number" || type == "string") {
      cx.marked = jsonldMode ? "property" : (cx.style + " property");
      return cont(afterprop);
    } else if (type == "jsonld-keyword") {
      return cont(afterprop);
    } else if (type == "[") {
      return cont(expression, expect("]"), afterprop);
    }
  }
  function getterSetter(type) {
    if (type != "variable") return pass(afterprop);
    cx.marked = "property";
    return cont(functiondef);
  }
  function afterprop(type) {
    if (type == ":") return cont(expressionNoComma);
    if (type == "(") return pass(functiondef);
  }
  function commasep(what, end) {
    function proceed(type) {
      if (type == ",") {
        var lex = cx.state.lexical;
        if (lex.info == "call") lex.pos = (lex.pos || 0) + 1;
        return cont(what, proceed);
      }
      if (type == end) return cont();
      return cont(expect(end));
    }
    return function(type) {
      if (type == end) return cont();
      return pass(what, proceed);
    };
  }
  function contCommasep(what, end, info) {
    for (var i = 3; i < arguments.length; i++)
      cx.cc.push(arguments[i]);
    return cont(pushlex(end, info), commasep(what, end), poplex);
  }
  function block(type) {
    if (type == "}") return cont();
    return pass(statement, block);
  }
  function maybetype(type) {
    if (isTS && type == ":") return cont(typedef);
  }
  function typedef(type) {
    if (type == "variable"){cx.marked = "variable-3"; return cont();}
  }
  function vardef() {
    return pass(pattern, maybetype, maybeAssign, vardefCont);
  }
  function pattern(type, value) {
    if (type == "variable") { register(value); return cont(); }
    if (type == "[") return contCommasep(pattern, "]");
    if (type == "{") return contCommasep(proppattern, "}");
  }
  function proppattern(type, value) {
    if (type == "variable" && !cx.stream.match(/^\s*:/, false)) {
      register(value);
      return cont(maybeAssign);
    }
    if (type == "variable") cx.marked = "property";
    return cont(expect(":"), pattern, maybeAssign);
  }
  function maybeAssign(_type, value) {
    if (value == "=") return cont(expressionNoComma);
  }
  function vardefCont(type) {
    if (type == ",") return cont(vardef);
  }
  function maybeelse(type, value) {
    if (type == "keyword b" && value == "else") return cont(pushlex("form", "else"), statement, poplex);
  }
  function forspec(type) {
    if (type == "(") return cont(pushlex(")"), forspec1, expect(")"), poplex);
  }
  function forspec1(type) {
    if (type == "var") return cont(vardef, expect(";"), forspec2);
    if (type == ";") return cont(forspec2);
    if (type == "variable") return cont(formaybeinof);
    return pass(expression, expect(";"), forspec2);
  }
  function formaybeinof(_type, value) {
    if (value == "in" || value == "of") { cx.marked = "keyword"; return cont(expression); }
    return cont(maybeoperatorComma, forspec2);
  }
  function forspec2(type, value) {
    if (type == ";") return cont(forspec3);
    if (value == "in" || value == "of") { cx.marked = "keyword"; return cont(expression); }
    return pass(expression, expect(";"), forspec3);
  }
  function forspec3(type) {
    if (type != ")") cont(expression);
  }
  function functiondef(type, value) {
    if (value == "*") {cx.marked = "keyword"; return cont(functiondef);}
    if (type == "variable") {register(value); return cont(functiondef);}
    if (type == "(") return cont(pushcontext, pushlex(")"), commasep(funarg, ")"), poplex, statement, popcontext);
  }
  function funarg(type) {
    if (type == "spread") return cont(funarg);
    return pass(pattern, maybetype);
  }
  function className(type, value) {
    if (type == "variable") {register(value); return cont(classNameAfter);}
  }
  function classNameAfter(type, value) {
    if (value == "extends") return cont(expression, classNameAfter);
    if (type == "{") return cont(pushlex("}"), classBody, poplex);
  }
  function classBody(type, value) {
    if (type == "variable" || cx.style == "keyword") {
      cx.marked = "property";
      if (value == "get" || value == "set") return cont(classGetterSetter, functiondef, classBody);
      return cont(functiondef, classBody);
    }
    if (value == "*") {
      cx.marked = "keyword";
      return cont(classBody);
    }
    if (type == ";") return cont(classBody);
    if (type == "}") return cont();
  }
  function classGetterSetter(type) {
    if (type != "variable") return pass();
    cx.marked = "property";
    return cont();
  }
  function afterModule(type, value) {
    if (type == "string") return cont(statement);
    if (type == "variable") { register(value); return cont(maybeFrom); }
  }
  function afterExport(_type, value) {
    if (value == "*") { cx.marked = "keyword"; return cont(maybeFrom, expect(";")); }
    if (value == "default") { cx.marked = "keyword"; return cont(expression, expect(";")); }
    return pass(statement);
  }
  function afterImport(type) {
    if (type == "string") return cont();
    return pass(importSpec, maybeFrom);
  }
  function importSpec(type, value) {
    if (type == "{") return contCommasep(importSpec, "}");
    if (type == "variable") register(value);
    return cont();
  }
  function maybeFrom(_type, value) {
    if (value == "from") { cx.marked = "keyword"; return cont(expression); }
  }
  function arrayLiteral(type) {
    if (type == "]") return cont();
    return pass(expressionNoComma, maybeArrayComprehension);
  }
  function maybeArrayComprehension(type) {
    if (type == "for") return pass(comprehension, expect("]"));
    if (type == ",") return cont(commasep(maybeexpressionNoComma, "]"));
    return pass(commasep(expressionNoComma, "]"));
  }
  function comprehension(type) {
    if (type == "for") return cont(forspec, comprehension);
    if (type == "if") return cont(expression, comprehension);
  }

  function isContinuedStatement(state, textAfter) {
    return state.lastType == "operator" || state.lastType == "," ||
      isOperatorChar.test(textAfter.charAt(0)) ||
      /[,.]/.test(textAfter.charAt(0));
  }

  // Interface

  return {
    startState: function(basecolumn) {
      var state = {
        tokenize: tokenBase,
        lastType: "sof",
        cc: [],
        lexical: new JSLexical((basecolumn || 0) - indentUnit, 0, "block", false),
        localVars: parserConfig.localVars,
        context: parserConfig.localVars && {vars: parserConfig.localVars},
        indented: 0
      };
      if (parserConfig.globalVars && typeof parserConfig.globalVars == "object")
        state.globalVars = parserConfig.globalVars;
      return state;
    },

    token: function(stream, state) {
      if (stream.sol()) {
        if (!state.lexical.hasOwnProperty("align"))
          state.lexical.align = false;
        state.indented = stream.indentation();
        findFatArrow(stream, state);
      }
      if (state.tokenize != tokenComment && stream.eatSpace()) return null;
      var style = state.tokenize(stream, state);
      if (type == "comment") return style;
      state.lastType = type == "operator" && (content == "++" || content == "--") ? "incdec" : type;
      return parseJS(state, style, type, content, stream);
    },

    indent: function(state, textAfter) {
      if (state.tokenize == tokenComment) return CodeMirror.Pass;
      if (state.tokenize != tokenBase) return 0;
      var firstChar = textAfter && textAfter.charAt(0), lexical = state.lexical;
      // Kludge to prevent 'maybelse' from blocking lexical scope pops
      if (!/^\s*else\b/.test(textAfter)) for (var i = state.cc.length - 1; i >= 0; --i) {
        var c = state.cc[i];
        if (c == poplex) lexical = lexical.prev;
        else if (c != maybeelse) break;
      }
      if (lexical.type == "stat" && firstChar == "}") lexical = lexical.prev;
      if (statementIndent && lexical.type == ")" && lexical.prev.type == "stat")
        lexical = lexical.prev;
      var type = lexical.type, closing = firstChar == type;

      if (type == "vardef") return lexical.indented + (state.lastType == "operator" || state.lastType == "," ? lexical.info + 1 : 0);
      else if (type == "form" && firstChar == "{") return lexical.indented;
      else if (type == "form") return lexical.indented + indentUnit;
      else if (type == "stat")
        return lexical.indented + (isContinuedStatement(state, textAfter) ? statementIndent || indentUnit : 0);
      else if (lexical.info == "switch" && !closing && parserConfig.doubleIndentSwitch != false)
        return lexical.indented + (/^(?:case|default)\b/.test(textAfter) ? indentUnit : 2 * indentUnit);
      else if (lexical.align) return lexical.column + (closing ? 0 : 1);
      else return lexical.indented + (closing ? 0 : indentUnit);
    },

    electricInput: /^\s*(?:case .*?:|default:|\{|\})$/,
    blockCommentStart: jsonMode ? null : "/*",
    blockCommentEnd: jsonMode ? null : "*/",
    lineComment: jsonMode ? null : "//",
    fold: "brace",

    helperType: jsonMode ? "json" : "javascript",
    jsonldMode: jsonldMode,
    jsonMode: jsonMode
  };
});

CodeMirror.registerHelper("wordChars", "javascript", /[\w$]/);

CodeMirror.defineMIME("text/javascript", "javascript");
CodeMirror.defineMIME("text/ecmascript", "javascript");
CodeMirror.defineMIME("application/javascript", "javascript");
CodeMirror.defineMIME("application/x-javascript", "javascript");
CodeMirror.defineMIME("application/ecmascript", "javascript");
CodeMirror.defineMIME("application/json", {name: "javascript", json: true});
CodeMirror.defineMIME("application/x-json", {name: "javascript", json: true});
CodeMirror.defineMIME("application/ld+json", {name: "javascript", jsonld: true});
CodeMirror.defineMIME("text/typescript", { name: "javascript", typescript: true });
CodeMirror.defineMIME("application/typescript", { name: "javascript", typescript: true });

});

},{"codemirror":undefined}],11:[function(require,module,exports){
// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod((function(){try{return require('codemirror')}catch(e){return window.CodeMirror}})());
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

CodeMirror.defineMode("xml", function(config, parserConfig) {
  var indentUnit = config.indentUnit;
  var multilineTagIndentFactor = parserConfig.multilineTagIndentFactor || 1;
  var multilineTagIndentPastTag = parserConfig.multilineTagIndentPastTag;
  if (multilineTagIndentPastTag == null) multilineTagIndentPastTag = true;

  var Kludges = parserConfig.htmlMode ? {
    autoSelfClosers: {'area': true, 'base': true, 'br': true, 'col': true, 'command': true,
                      'embed': true, 'frame': true, 'hr': true, 'img': true, 'input': true,
                      'keygen': true, 'link': true, 'meta': true, 'param': true, 'source': true,
                      'track': true, 'wbr': true, 'menuitem': true},
    implicitlyClosed: {'dd': true, 'li': true, 'optgroup': true, 'option': true, 'p': true,
                       'rp': true, 'rt': true, 'tbody': true, 'td': true, 'tfoot': true,
                       'th': true, 'tr': true},
    contextGrabbers: {
      'dd': {'dd': true, 'dt': true},
      'dt': {'dd': true, 'dt': true},
      'li': {'li': true},
      'option': {'option': true, 'optgroup': true},
      'optgroup': {'optgroup': true},
      'p': {'address': true, 'article': true, 'aside': true, 'blockquote': true, 'dir': true,
            'div': true, 'dl': true, 'fieldset': true, 'footer': true, 'form': true,
            'h1': true, 'h2': true, 'h3': true, 'h4': true, 'h5': true, 'h6': true,
            'header': true, 'hgroup': true, 'hr': true, 'menu': true, 'nav': true, 'ol': true,
            'p': true, 'pre': true, 'section': true, 'table': true, 'ul': true},
      'rp': {'rp': true, 'rt': true},
      'rt': {'rp': true, 'rt': true},
      'tbody': {'tbody': true, 'tfoot': true},
      'td': {'td': true, 'th': true},
      'tfoot': {'tbody': true},
      'th': {'td': true, 'th': true},
      'thead': {'tbody': true, 'tfoot': true},
      'tr': {'tr': true}
    },
    doNotIndent: {"pre": true},
    allowUnquoted: true,
    allowMissing: true,
    caseFold: true
  } : {
    autoSelfClosers: {},
    implicitlyClosed: {},
    contextGrabbers: {},
    doNotIndent: {},
    allowUnquoted: false,
    allowMissing: false,
    caseFold: false
  };
  var alignCDATA = parserConfig.alignCDATA;

  // Return variables for tokenizers
  var type, setStyle;

  function inText(stream, state) {
    function chain(parser) {
      state.tokenize = parser;
      return parser(stream, state);
    }

    var ch = stream.next();
    if (ch == "<") {
      if (stream.eat("!")) {
        if (stream.eat("[")) {
          if (stream.match("CDATA[")) return chain(inBlock("atom", "]]>"));
          else return null;
        } else if (stream.match("--")) {
          return chain(inBlock("comment", "-->"));
        } else if (stream.match("DOCTYPE", true, true)) {
          stream.eatWhile(/[\w\._\-]/);
          return chain(doctype(1));
        } else {
          return null;
        }
      } else if (stream.eat("?")) {
        stream.eatWhile(/[\w\._\-]/);
        state.tokenize = inBlock("meta", "?>");
        return "meta";
      } else {
        type = stream.eat("/") ? "closeTag" : "openTag";
        state.tokenize = inTag;
        return "tag bracket";
      }
    } else if (ch == "&") {
      var ok;
      if (stream.eat("#")) {
        if (stream.eat("x")) {
          ok = stream.eatWhile(/[a-fA-F\d]/) && stream.eat(";");
        } else {
          ok = stream.eatWhile(/[\d]/) && stream.eat(";");
        }
      } else {
        ok = stream.eatWhile(/[\w\.\-:]/) && stream.eat(";");
      }
      return ok ? "atom" : "error";
    } else {
      stream.eatWhile(/[^&<]/);
      return null;
    }
  }

  function inTag(stream, state) {
    var ch = stream.next();
    if (ch == ">" || (ch == "/" && stream.eat(">"))) {
      state.tokenize = inText;
      type = ch == ">" ? "endTag" : "selfcloseTag";
      return "tag bracket";
    } else if (ch == "=") {
      type = "equals";
      return null;
    } else if (ch == "<") {
      state.tokenize = inText;
      state.state = baseState;
      state.tagName = state.tagStart = null;
      var next = state.tokenize(stream, state);
      return next ? next + " tag error" : "tag error";
    } else if (/[\'\"]/.test(ch)) {
      state.tokenize = inAttribute(ch);
      state.stringStartCol = stream.column();
      return state.tokenize(stream, state);
    } else {
      stream.match(/^[^\s\u00a0=<>\"\']*[^\s\u00a0=<>\"\'\/]/);
      return "word";
    }
  }

  function inAttribute(quote) {
    var closure = function(stream, state) {
      while (!stream.eol()) {
        if (stream.next() == quote) {
          state.tokenize = inTag;
          break;
        }
      }
      return "string";
    };
    closure.isInAttribute = true;
    return closure;
  }

  function inBlock(style, terminator) {
    return function(stream, state) {
      while (!stream.eol()) {
        if (stream.match(terminator)) {
          state.tokenize = inText;
          break;
        }
        stream.next();
      }
      return style;
    };
  }
  function doctype(depth) {
    return function(stream, state) {
      var ch;
      while ((ch = stream.next()) != null) {
        if (ch == "<") {
          state.tokenize = doctype(depth + 1);
          return state.tokenize(stream, state);
        } else if (ch == ">") {
          if (depth == 1) {
            state.tokenize = inText;
            break;
          } else {
            state.tokenize = doctype(depth - 1);
            return state.tokenize(stream, state);
          }
        }
      }
      return "meta";
    };
  }

  function Context(state, tagName, startOfLine) {
    this.prev = state.context;
    this.tagName = tagName;
    this.indent = state.indented;
    this.startOfLine = startOfLine;
    if (Kludges.doNotIndent.hasOwnProperty(tagName) || (state.context && state.context.noIndent))
      this.noIndent = true;
  }
  function popContext(state) {
    if (state.context) state.context = state.context.prev;
  }
  function maybePopContext(state, nextTagName) {
    var parentTagName;
    while (true) {
      if (!state.context) {
        return;
      }
      parentTagName = state.context.tagName;
      if (!Kludges.contextGrabbers.hasOwnProperty(parentTagName) ||
          !Kludges.contextGrabbers[parentTagName].hasOwnProperty(nextTagName)) {
        return;
      }
      popContext(state);
    }
  }

  function baseState(type, stream, state) {
    if (type == "openTag") {
      state.tagStart = stream.column();
      return tagNameState;
    } else if (type == "closeTag") {
      return closeTagNameState;
    } else {
      return baseState;
    }
  }
  function tagNameState(type, stream, state) {
    if (type == "word") {
      state.tagName = stream.current();
      setStyle = "tag";
      return attrState;
    } else {
      setStyle = "error";
      return tagNameState;
    }
  }
  function closeTagNameState(type, stream, state) {
    if (type == "word") {
      var tagName = stream.current();
      if (state.context && state.context.tagName != tagName &&
          Kludges.implicitlyClosed.hasOwnProperty(state.context.tagName))
        popContext(state);
      if (state.context && state.context.tagName == tagName) {
        setStyle = "tag";
        return closeState;
      } else {
        setStyle = "tag error";
        return closeStateErr;
      }
    } else {
      setStyle = "error";
      return closeStateErr;
    }
  }

  function closeState(type, _stream, state) {
    if (type != "endTag") {
      setStyle = "error";
      return closeState;
    }
    popContext(state);
    return baseState;
  }
  function closeStateErr(type, stream, state) {
    setStyle = "error";
    return closeState(type, stream, state);
  }

  function attrState(type, _stream, state) {
    if (type == "word") {
      setStyle = "attribute";
      return attrEqState;
    } else if (type == "endTag" || type == "selfcloseTag") {
      var tagName = state.tagName, tagStart = state.tagStart;
      state.tagName = state.tagStart = null;
      if (type == "selfcloseTag" ||
          Kludges.autoSelfClosers.hasOwnProperty(tagName)) {
        maybePopContext(state, tagName);
      } else {
        maybePopContext(state, tagName);
        state.context = new Context(state, tagName, tagStart == state.indented);
      }
      return baseState;
    }
    setStyle = "error";
    return attrState;
  }
  function attrEqState(type, stream, state) {
    if (type == "equals") return attrValueState;
    if (!Kludges.allowMissing) setStyle = "error";
    return attrState(type, stream, state);
  }
  function attrValueState(type, stream, state) {
    if (type == "string") return attrContinuedState;
    if (type == "word" && Kludges.allowUnquoted) {setStyle = "string"; return attrState;}
    setStyle = "error";
    return attrState(type, stream, state);
  }
  function attrContinuedState(type, stream, state) {
    if (type == "string") return attrContinuedState;
    return attrState(type, stream, state);
  }

  return {
    startState: function() {
      return {tokenize: inText,
              state: baseState,
              indented: 0,
              tagName: null, tagStart: null,
              context: null};
    },

    token: function(stream, state) {
      if (!state.tagName && stream.sol())
        state.indented = stream.indentation();

      if (stream.eatSpace()) return null;
      type = null;
      var style = state.tokenize(stream, state);
      if ((style || type) && style != "comment") {
        setStyle = null;
        state.state = state.state(type || style, stream, state);
        if (setStyle)
          style = setStyle == "error" ? style + " error" : setStyle;
      }
      return style;
    },

    indent: function(state, textAfter, fullLine) {
      var context = state.context;
      // Indent multi-line strings (e.g. css).
      if (state.tokenize.isInAttribute) {
        if (state.tagStart == state.indented)
          return state.stringStartCol + 1;
        else
          return state.indented + indentUnit;
      }
      if (context && context.noIndent) return CodeMirror.Pass;
      if (state.tokenize != inTag && state.tokenize != inText)
        return fullLine ? fullLine.match(/^(\s*)/)[0].length : 0;
      // Indent the starts of attribute names.
      if (state.tagName) {
        if (multilineTagIndentPastTag)
          return state.tagStart + state.tagName.length + 2;
        else
          return state.tagStart + indentUnit * multilineTagIndentFactor;
      }
      if (alignCDATA && /<!\[CDATA\[/.test(textAfter)) return 0;
      var tagAfter = textAfter && /^<(\/)?([\w_:\.-]*)/.exec(textAfter);
      if (tagAfter && tagAfter[1]) { // Closing tag spotted
        while (context) {
          if (context.tagName == tagAfter[2]) {
            context = context.prev;
            break;
          } else if (Kludges.implicitlyClosed.hasOwnProperty(context.tagName)) {
            context = context.prev;
          } else {
            break;
          }
        }
      } else if (tagAfter) { // Opening tag spotted
        while (context) {
          var grabbers = Kludges.contextGrabbers[context.tagName];
          if (grabbers && grabbers.hasOwnProperty(tagAfter[2]))
            context = context.prev;
          else
            break;
        }
      }
      while (context && !context.startOfLine)
        context = context.prev;
      if (context) return context.indent + indentUnit;
      else return 0;
    },

    electricInput: /<\/[\s\w:]+>$/,
    blockCommentStart: "<!--",
    blockCommentEnd: "-->",

    configuration: parserConfig.htmlMode ? "html" : "xml",
    helperType: parserConfig.htmlMode ? "html" : "xml"
  };
});

CodeMirror.defineMIME("text/xml", "xml");
CodeMirror.defineMIME("application/xml", "xml");
if (!CodeMirror.mimeModes.hasOwnProperty("text/html"))
  CodeMirror.defineMIME("text/html", {name: "xml", htmlMode: true});

});

},{"codemirror":undefined}],12:[function(require,module,exports){
(function() {
  var callWithJQuery;

  callWithJQuery = function(pivotModule) {
    if (typeof exports === "object" && typeof module === "object") {
      return pivotModule((function(){try{return require('jquery')}catch(e){return window.jQuery}})());
    } else if (typeof define === "function" && define.amd) {
      return define(["jquery"], pivotModule);
    } else {
      return pivotModule(jQuery);
    }
  };

  callWithJQuery(function($) {
    return $.pivotUtilities.d3_renderers = {
      Treemap: function(pivotData, opts) {
        var addToTree, color, defaults, height, i, len, ref, result, rowKey, tree, treemap, value, width;
        defaults = {
          localeStrings: {},
          d3: {
            width: function() {
              return $(window).width() / 1.4;
            },
            height: function() {
              return $(window).height() / 1.4;
            }
          }
        };
        opts = $.extend(defaults, opts);
        result = $("<div>").css({
          width: "100%",
          height: "100%"
        });
        tree = {
          name: "All",
          children: []
        };
        addToTree = function(tree, path, value) {
          var child, i, len, newChild, ref, x;
          if (path.length === 0) {
            tree.value = value;
            return;
          }
          if (tree.children == null) {
            tree.children = [];
          }
          x = path.shift();
          ref = tree.children;
          for (i = 0, len = ref.length; i < len; i++) {
            child = ref[i];
            if (!(child.name === x)) {
              continue;
            }
            addToTree(child, path, value);
            return;
          }
          newChild = {
            name: x
          };
          addToTree(newChild, path, value);
          return tree.children.push(newChild);
        };
        ref = pivotData.getRowKeys();
        for (i = 0, len = ref.length; i < len; i++) {
          rowKey = ref[i];
          value = pivotData.getAggregator(rowKey, []).value();
          if (value != null) {
            addToTree(tree, rowKey, value);
          }
        }
        color = d3.scale.category10();
        width = opts.d3.width();
        height = opts.d3.height();
        treemap = d3.layout.treemap().size([width, height]).sticky(true).value(function(d) {
          return d.size;
        });
        d3.select(result[0]).append("div").style("position", "relative").style("width", width + "px").style("height", height + "px").datum(tree).selectAll(".node").data(treemap.padding([15, 0, 0, 0]).value(function(d) {
          return d.value;
        }).nodes).enter().append("div").attr("class", "node").style("background", function(d) {
          if (d.children != null) {
            return "lightgrey";
          } else {
            return color(d.name);
          }
        }).text(function(d) {
          return d.name;
        }).call(function() {
          this.style("left", function(d) {
            return d.x + "px";
          }).style("top", function(d) {
            return d.y + "px";
          }).style("width", function(d) {
            return Math.max(0, d.dx - 1) + "px";
          }).style("height", function(d) {
            return Math.max(0, d.dy - 1) + "px";
          });
        });
        return result;
      }
    };
  });

}).call(this);

//# sourceMappingURL=d3_renderers.js.map
},{"jquery":undefined}],13:[function(require,module,exports){
(function() {
  var callWithJQuery;

  callWithJQuery = function(pivotModule) {
    if (typeof exports === "object" && typeof module === "object") {
      return pivotModule((function(){try{return require('jquery')}catch(e){return window.jQuery}})());
    } else if (typeof define === "function" && define.amd) {
      return define(["jquery"], pivotModule);
    } else {
      return pivotModule(jQuery);
    }
  };

  callWithJQuery(function($) {
    var makeGoogleChart;
    makeGoogleChart = function(chartType, extraOptions) {
      return function(pivotData, opts) {
        var agg, base, base1, colKey, colKeys, dataArray, dataTable, defaults, fullAggName, groupByTitle, h, hAxisTitle, headers, i, j, len, len1, numCharsInHAxis, options, ref, result, row, rowKey, rowKeys, title, tree2, vAxisTitle, val, wrapper, x, y;
        defaults = {
          localeStrings: {
            vs: "vs",
            by: "by"
          },
          gchart: {}
        };
        opts = $.extend(true, defaults, opts);
        if ((base = opts.gchart).width == null) {
          base.width = window.innerWidth / 1.4;
        }
        if ((base1 = opts.gchart).height == null) {
          base1.height = window.innerHeight / 1.4;
        }
        rowKeys = pivotData.getRowKeys();
        if (rowKeys.length === 0) {
          rowKeys.push([]);
        }
        colKeys = pivotData.getColKeys();
        if (colKeys.length === 0) {
          colKeys.push([]);
        }
        fullAggName = pivotData.aggregatorName;
        if (pivotData.valAttrs.length) {
          fullAggName += "(" + (pivotData.valAttrs.join(", ")) + ")";
        }
        headers = (function() {
          var i, len, results;
          results = [];
          for (i = 0, len = rowKeys.length; i < len; i++) {
            h = rowKeys[i];
            results.push(h.join("-"));
          }
          return results;
        })();
        headers.unshift("");
        numCharsInHAxis = 0;
        if (chartType === "ScatterChart") {
          dataArray = [];
          ref = pivotData.tree;
          for (y in ref) {
            tree2 = ref[y];
            for (x in tree2) {
              agg = tree2[x];
              dataArray.push([parseFloat(x), parseFloat(y), fullAggName + ": \n" + agg.format(agg.value())]);
            }
          }
          dataTable = new google.visualization.DataTable();
          dataTable.addColumn('number', pivotData.colAttrs.join("-"));
          dataTable.addColumn('number', pivotData.rowAttrs.join("-"));
          dataTable.addColumn({
            type: "string",
            role: "tooltip"
          });
          dataTable.addRows(dataArray);
          hAxisTitle = pivotData.colAttrs.join("-");
          vAxisTitle = pivotData.rowAttrs.join("-");
          title = "";
        } else {
          dataArray = [headers];
          for (i = 0, len = colKeys.length; i < len; i++) {
            colKey = colKeys[i];
            row = [colKey.join("-")];
            numCharsInHAxis += row[0].length;
            for (j = 0, len1 = rowKeys.length; j < len1; j++) {
              rowKey = rowKeys[j];
              agg = pivotData.getAggregator(rowKey, colKey);
              if (agg.value() != null) {
                val = agg.value();
                if ($.isNumeric(val)) {
                  if (val < 1) {
                    row.push(parseFloat(val.toPrecision(3)));
                  } else {
                    row.push(parseFloat(val.toFixed(3)));
                  }
                } else {
                  row.push(val);
                }
              } else {
                row.push(null);
              }
            }
            dataArray.push(row);
          }
          dataTable = google.visualization.arrayToDataTable(dataArray);
          title = vAxisTitle = fullAggName;
          hAxisTitle = pivotData.colAttrs.join("-");
          if (hAxisTitle !== "") {
            title += " " + opts.localeStrings.vs + " " + hAxisTitle;
          }
          groupByTitle = pivotData.rowAttrs.join("-");
          if (groupByTitle !== "") {
            title += " " + opts.localeStrings.by + " " + groupByTitle;
          }
        }
        options = {
          title: title,
          hAxis: {
            title: hAxisTitle,
            slantedText: numCharsInHAxis > 50
          },
          vAxis: {
            title: vAxisTitle
          },
          tooltip: {
            textStyle: {
              fontName: 'Arial',
              fontSize: 12
            }
          }
        };
        if (chartType === "ColumnChart") {
          options.vAxis.minValue = 0;
        }
        if (chartType === "ScatterChart") {
          options.legend = {
            position: "none"
          };
          options.chartArea = {
            'width': '80%',
            'height': '80%'
          };
        } else if (dataArray[0].length === 2 && dataArray[0][1] === "") {
          options.legend = {
            position: "none"
          };
        }
        $.extend(options, opts.gchart, extraOptions);
        result = $("<div>").css({
          width: "100%",
          height: "100%"
        });
        wrapper = new google.visualization.ChartWrapper({
          dataTable: dataTable,
          chartType: chartType,
          options: options
        });
        wrapper.draw(result[0]);
        result.bind("dblclick", function() {
          var editor;
          editor = new google.visualization.ChartEditor();
          google.visualization.events.addListener(editor, 'ok', function() {
            return editor.getChartWrapper().draw(result[0]);
          });
          return editor.openDialog(wrapper);
        });
        return result;
      };
    };
    return $.pivotUtilities.gchart_renderers = {
      "Line Chart": makeGoogleChart("LineChart"),
      "Bar Chart": makeGoogleChart("ColumnChart"),
      "Stacked Bar Chart": makeGoogleChart("ColumnChart", {
        isStacked: true
      }),
      "Area Chart": makeGoogleChart("AreaChart", {
        isStacked: true
      }),
      "Scatter Chart": makeGoogleChart("ScatterChart")
    };
  });

}).call(this);

//# sourceMappingURL=gchart_renderers.js.map
},{"jquery":undefined}],14:[function(require,module,exports){
;(function(win){
	var store = {},
		doc = win.document,
		localStorageName = 'localStorage',
		scriptTag = 'script',
		storage

	store.disabled = false
	store.version = '1.3.17'
	store.set = function(key, value) {}
	store.get = function(key, defaultVal) {}
	store.has = function(key) { return store.get(key) !== undefined }
	store.remove = function(key) {}
	store.clear = function() {}
	store.transact = function(key, defaultVal, transactionFn) {
		if (transactionFn == null) {
			transactionFn = defaultVal
			defaultVal = null
		}
		if (defaultVal == null) {
			defaultVal = {}
		}
		var val = store.get(key, defaultVal)
		transactionFn(val)
		store.set(key, val)
	}
	store.getAll = function() {}
	store.forEach = function() {}

	store.serialize = function(value) {
		return JSON.stringify(value)
	}
	store.deserialize = function(value) {
		if (typeof value != 'string') { return undefined }
		try { return JSON.parse(value) }
		catch(e) { return value || undefined }
	}

	// Functions to encapsulate questionable FireFox 3.6.13 behavior
	// when about.config::dom.storage.enabled === false
	// See https://github.com/marcuswestin/store.js/issues#issue/13
	function isLocalStorageNameSupported() {
		try { return (localStorageName in win && win[localStorageName]) }
		catch(err) { return false }
	}

	if (isLocalStorageNameSupported()) {
		storage = win[localStorageName]
		store.set = function(key, val) {
			if (val === undefined) { return store.remove(key) }
			storage.setItem(key, store.serialize(val))
			return val
		}
		store.get = function(key, defaultVal) {
			var val = store.deserialize(storage.getItem(key))
			return (val === undefined ? defaultVal : val)
		}
		store.remove = function(key) { storage.removeItem(key) }
		store.clear = function() { storage.clear() }
		store.getAll = function() {
			var ret = {}
			store.forEach(function(key, val) {
				ret[key] = val
			})
			return ret
		}
		store.forEach = function(callback) {
			for (var i=0; i<storage.length; i++) {
				var key = storage.key(i)
				callback(key, store.get(key))
			}
		}
	} else if (doc.documentElement.addBehavior) {
		var storageOwner,
			storageContainer
		// Since #userData storage applies only to specific paths, we need to
		// somehow link our data to a specific path.  We choose /favicon.ico
		// as a pretty safe option, since all browsers already make a request to
		// this URL anyway and being a 404 will not hurt us here.  We wrap an
		// iframe pointing to the favicon in an ActiveXObject(htmlfile) object
		// (see: http://msdn.microsoft.com/en-us/library/aa752574(v=VS.85).aspx)
		// since the iframe access rules appear to allow direct access and
		// manipulation of the document element, even for a 404 page.  This
		// document can be used instead of the current document (which would
		// have been limited to the current path) to perform #userData storage.
		try {
			storageContainer = new ActiveXObject('htmlfile')
			storageContainer.open()
			storageContainer.write('<'+scriptTag+'>document.w=window</'+scriptTag+'><iframe src="/favicon.ico"></iframe>')
			storageContainer.close()
			storageOwner = storageContainer.w.frames[0].document
			storage = storageOwner.createElement('div')
		} catch(e) {
			// somehow ActiveXObject instantiation failed (perhaps some special
			// security settings or otherwse), fall back to per-path storage
			storage = doc.createElement('div')
			storageOwner = doc.body
		}
		var withIEStorage = function(storeFunction) {
			return function() {
				var args = Array.prototype.slice.call(arguments, 0)
				args.unshift(storage)
				// See http://msdn.microsoft.com/en-us/library/ms531081(v=VS.85).aspx
				// and http://msdn.microsoft.com/en-us/library/ms531424(v=VS.85).aspx
				storageOwner.appendChild(storage)
				storage.addBehavior('#default#userData')
				storage.load(localStorageName)
				var result = storeFunction.apply(store, args)
				storageOwner.removeChild(storage)
				return result
			}
		}

		// In IE7, keys cannot start with a digit or contain certain chars.
		// See https://github.com/marcuswestin/store.js/issues/40
		// See https://github.com/marcuswestin/store.js/issues/83
		var forbiddenCharsRegex = new RegExp("[!\"#$%&'()*+,/\\\\:;<=>?@[\\]^`{|}~]", "g")
		function ieKeyFix(key) {
			return key.replace(/^d/, '___$&').replace(forbiddenCharsRegex, '___')
		}
		store.set = withIEStorage(function(storage, key, val) {
			key = ieKeyFix(key)
			if (val === undefined) { return store.remove(key) }
			storage.setAttribute(key, store.serialize(val))
			storage.save(localStorageName)
			return val
		})
		store.get = withIEStorage(function(storage, key, defaultVal) {
			key = ieKeyFix(key)
			var val = store.deserialize(storage.getAttribute(key))
			return (val === undefined ? defaultVal : val)
		})
		store.remove = withIEStorage(function(storage, key) {
			key = ieKeyFix(key)
			storage.removeAttribute(key)
			storage.save(localStorageName)
		})
		store.clear = withIEStorage(function(storage) {
			var attributes = storage.XMLDocument.documentElement.attributes
			storage.load(localStorageName)
			for (var i=0, attr; attr=attributes[i]; i++) {
				storage.removeAttribute(attr.name)
			}
			storage.save(localStorageName)
		})
		store.getAll = function(storage) {
			var ret = {}
			store.forEach(function(key, val) {
				ret[key] = val
			})
			return ret
		}
		store.forEach = withIEStorage(function(storage, callback) {
			var attributes = storage.XMLDocument.documentElement.attributes
			for (var i=0, attr; attr=attributes[i]; ++i) {
				callback(attr.name, store.deserialize(storage.getAttribute(attr.name)))
			}
		})
	}

	try {
		var testKey = '__storejs__'
		store.set(testKey, testKey)
		if (store.get(testKey) != testKey) { store.disabled = true }
		store.remove(testKey)
	} catch(e) {
		store.disabled = true
	}
	store.enabled = !store.disabled

	if (typeof module != 'undefined' && module.exports && this.module !== module) { module.exports = store }
	else if (typeof define === 'function' && define.amd) { define(store) }
	else { win.store = store }

})(Function('return this')());

},{}],15:[function(require,module,exports){
module.exports={
  "name": "yasgui-utils",
  "version": "1.6.0",
  "description": "Utils for YASGUI libs",
  "main": "src/main.js",
  "repository": {
    "type": "git",
    "url": "git://github.com/YASGUI/Utils.git"
  },
  "licenses": [
    {
      "type": "MIT",
      "url": "http://yasgui.github.io/license.txt"
    }
  ],
  "author": {
    "name": "Laurens Rietveld"
  },
  "maintainers": [
    {
      "name": "Laurens Rietveld",
      "email": "laurens.rietveld@gmail.com",
      "url": "http://laurensrietveld.nl"
    }
  ],
  "bugs": {
    "url": "https://github.com/YASGUI/Utils/issues"
  },
  "homepage": "https://github.com/YASGUI/Utils",
  "dependencies": {
    "store": "^1.3.14"
  },
  "readme": "A simple utils repo for the YASGUI tools\n",
  "readmeFilename": "README.md",
  "_id": "yasgui-utils@1.6.0",
  "_from": "yasgui-utils@>=1.4.1 <2.0.0"
}

},{}],16:[function(require,module,exports){
window.console = window.console || {"log":function(){}};//make sure any console statements don't break IE
module.exports = {
	storage: require("./storage.js"),
	svg: require("./svg.js"),
	version: {
		"yasgui-utils" : require("../package.json").version,
	},
	nestedExists : function(obj) {
		var args = Array.prototype.slice.call(arguments, 1);

		for (var i = 0; i < args.length; i++) {
			if (!obj || !obj.hasOwnProperty(args[i])) {
				return false;
			}
			obj = obj[args[i]];
		}
		return true;
	}
};

},{"../package.json":15,"./storage.js":17,"./svg.js":18}],17:[function(require,module,exports){
var store = require("store");
var times = {
	day: function() {
		return 1000 * 3600 * 24;//millis to day
	},
	month: function() {
		times.day() * 30;
	},
	year: function() {
		times.month() * 12;
	}
};

var root = module.exports = {
	set : function(key, val, exp) {
    if (!store.enabled) return;//this is probably in private mode. Don't run, as we might get Js errors
		if (key && val !== undefined) {
			if (typeof exp == "string") {
				exp = times[exp]();
			}
			//try to store string for dom objects (e.g. XML result). Otherwise, we might get a circular reference error when stringifying this
			if (val.documentElement) val = new XMLSerializer().serializeToString(val.documentElement);
			store.set(key, {
				val : val,
				exp : exp,
				time : new Date().getTime()
			});
		}
	},
	remove: function(key) {
		if (!store.enabled) return;//this is probably in private mode. Don't run, as we might get Js errors
		if (key) store.remove(key)
	},
	removeAll: function(filter) {
		if (!store.enabled) return;//this is probably in private mode. Don't run, as we might get Js errors
		if (typeof filter === 'function') {
			for (var key in store.getAll()) {
				if (filter(key, root.get(key))) root.remove(key);
			}
		}
	},
	get : function(key) {
    if (!store.enabled) return null;//this is probably in private mode. Don't run, as we might get Js errors
		if (key) {
			var info = store.get(key);
			if (!info) {
				return null;
			}
			if (info.exp && new Date().getTime() - info.time > info.exp) {
				return null;
			}
			return info.val;
		} else {
			return null;
		}
	}

};

},{"store":14}],18:[function(require,module,exports){
module.exports = {
	draw: function(parent, svgString) {
		if (!parent) return;
		var el = module.exports.getElement(svgString);
		if (el) {
			if (parent.append) {
				parent.append(el);
			} else {
				//regular dom doc
				parent.appendChild(el);
			}
		}
	},
	getElement: function(svgString) {
		if (svgString && svgString.indexOf("<svg") == 0) {
			//no style passed via config. guess own styles
			var parser = new DOMParser();
			var dom = parser.parseFromString(svgString, "text/xml");
			var svg = dom.documentElement;
			
			var svgContainer = document.createElement("div");
			svgContainer.className = 'svgImg';
			svgContainer.appendChild(svg);
			return svgContainer;
		}
		return false;
	}
};
},{}],19:[function(require,module,exports){
module.exports={
  "name": "yasgui-yasr",
  "description": "Yet Another SPARQL Resultset GUI",
  "version": "2.6.2",
  "main": "src/main.js",
  "license": "MIT",
  "author": "Laurens Rietveld",
  "homepage": "http://yasr.yasgui.org",
  "devDependencies": {
    "browserify": "^6.1.0",
    "gulp": "~3.6.0",
    "gulp-autoprefixer": "^3.0.2",
    "gulp-bump": "^0.1.11",
    "gulp-concat": "^2.4.1",
    "gulp-connect": "^2.0.5",
    "gulp-embedlr": "^0.5.2",
    "gulp-filter": "^1.0.2",
    "gulp-git": "^0.5.2",
    "gulp-jsvalidate": "^0.2.0",
    "gulp-livereload": "^1.3.1",
    "gulp-minify-css": "0.3.11",
    "gulp-notify": "^2.0.1",
    "gulp-rename": "^1.2.0",
    "gulp-streamify": "0.0.5",
    "gulp-tag-version": "^1.1.0",
    "gulp-uglify": "^1.0.1",
    "require-dir": "^0.1.0",
    "run-sequence": "^1.0.1",
    "vinyl-buffer": "^1.0.0",
    "vinyl-source-stream": "~0.1.1",
    "watchify": "^0.6.4",
    "gulp-sourcemaps": "^1.2.8",
    "exorcist": "^0.1.6",
    "vinyl-transform": "0.0.1",
    "gulp-sass": "^2.0.1",
    "bootstrap-sass": "^3.3.1",
    "browserify-transform-tools": "^1.2.1",
    "gulp-cssimport": "^1.3.1",
    "gulp-html-replace": "^1.4.1",
    "browserify-shim": "^3.8.1"
  },
  "bugs": "https://github.com/YASGUI/YASR/issues/",
  "keywords": [
    "JavaScript",
    "SPARQL",
    "Editor",
    "Semantic Web",
    "Linked Data"
  ],
  "maintainers": [
    {
      "name": "Laurens Rietveld",
      "email": "laurens.rietveld@gmail.com",
      "web": "http://laurensrietveld.nl"
    }
  ],
  "repository": {
    "type": "git",
    "url": "https://github.com/YASGUI/YASR.git"
  },
  "dependencies": {
    "jquery": ">=1.11.3",
    "datatables": "^1.10.7",
    "codemirror": "^4.7.0",
    "yasgui-utils": "^1.4.1",
    "pivottable": "^1.2.2",
    "jquery-ui": "^1.10.5",
    "d3": "^3.4.13"
  },
  "browserify-shim": {
    "google": "global:google"
  },
  "browserify": {
    "transform": [
      "browserify-shim"
    ]
  },
  "optionalShim": {
    "codemirror": {
      "require": "codemirror",
      "global": "CodeMirror"
    },
    "jquery": {
      "require": "jquery",
      "global": "jQuery"
    },
    "../../lib/codemirror": {
      "require": "codemirror",
      "global": "CodeMirror"
    },
    "datatables": {
      "require": "datatables",
      "global": "jQuery"
    },
    "d3": {
      "require": "d3",
      "global": "d3"
    },
    "jquery-ui/sortable": {
      "require": "jquery-ui/sortable",
      "global": "jQuery"
    },
    "pivottable": {
      "require": "pivottable",
      "global": "jQuery"
    }
  }
}

},{}],20:[function(require,module,exports){
'use strict';
module.exports = function(result) {
	var quote = "\"";
	var delimiter = ",";
	var lineBreak = "\n";

	var variables = result.head.vars;

	var querySolutions = result.results.bindings;



	var createHeader = function() {
		for (var i = 0; i < variables.length; i++) {
			addValueToString(variables[i]);
		}
		csvString += lineBreak;
	};

	var createBody = function() {
		for (var i = 0; i < querySolutions.length; i++) {
			addQuerySolutionToString(querySolutions[i]);
			csvString += lineBreak;
		}
	};

	var addQuerySolutionToString = function(querySolution) {
		for (var i = 0; i < variables.length; i++) {
			var variable = variables[i];
			if (querySolution.hasOwnProperty(variable)) {
				addValueToString(querySolution[variable]["value"]);
			} else {
				addValueToString("");
			}
		}
	};
	var addValueToString = function(value) {
		//Quotes in the string need to be escaped
		value.replace(quote, quote + quote);
		if (needToQuoteString(value)) {
			value = quote + value + quote;
		}
		csvString += " " + value + " " + delimiter;
	};

	var needToQuoteString = function(value) {
		//quote when it contains whitespace or the delimiter
		var needQuoting = false;
		if (value.match("[\\w|" + delimiter + "|" + quote + "]")) {
			needQuoting = true;
		}
		return needQuoting;
	};

	var csvString = "";
	createHeader();
	createBody();
	return csvString;
};
},{}],21:[function(require,module,exports){
'use strict';
var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})();

/**
 * Constructor of plugin which displays boolean info
 * 
 * @param yasr {object}
 * @param parent {DOM element}
 * @param options {object}
 * @class YASR.plugins.boolean
 * @return yasr-boolean (doc)
 * 
 */
var root = module.exports = function(yasr) {
	var container = $("<div class='booleanResult'></div>");
	var draw = function() {
		container.empty().appendTo(yasr.resultsContainer);
		var booleanVal = yasr.results.getBoolean();

		var imgId = null;
		var textVal = null;
		if (booleanVal === true) {
			imgId = "check";
			textVal = "True";
		} else if (booleanVal === false) {
			imgId = "cross";
			textVal = "False";
		} else {
			container.width("140");
			textVal = "Could not find boolean value in response";
		}

		//add icon
		if (imgId) require("yasgui-utils").svg.draw(container, require('./imgs.js')[imgId]);

		$("<span></span>").text(textVal).appendTo(container);
	};


	var canHandleResults = function() {
		return yasr.results.getBoolean && (yasr.results.getBoolean() === true || yasr.results.getBoolean() == false);
	};



	return {
		name: null, //don't need to set this: we don't show it in the selection widget anyway, so don't need a human-friendly name
		draw: draw,
		hideFromSelection: true,
		getPriority: 10,
		canHandleResults: canHandleResults
	}
};


root.version = {
	"YASR-boolean": require("../package.json").version,
	"jquery": $.fn.jquery,
};
},{"../package.json":19,"./imgs.js":27,"jquery":undefined,"yasgui-utils":16}],22:[function(require,module,exports){
'use strict';
var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})();
module.exports = {
	/**
	 * key of default plugin to use
	 * @property output
	 * @type string
	 * @default "table"
	 */
	output: "table",
	useGoogleCharts: true,
	outputPlugins: ["table", "error", "boolean", "rawResponse", "pivot", "gchart"],

	/**
	 * Draw the output selector widget
	 * 
	 * @property drawOutputSelector
	 * @type boolean
	 * @default true
	 */
	drawOutputSelector: true,

	/**
	 * Draw download icon. This issues html5 download functionality to 'download' files created on the client-side.
	 *  This allows the user to download results already queried for, such as a CSV when a table is shown, or the original response when the raw response output is selected
	 * 
	 * @property drawDownloadIcon
	 * @type boolean
	 * @default true
	 */
	drawDownloadIcon: true,


	getUsedPrefixes: null,
	/**
	 * Make certain settings and values of YASR persistent. Setting a key
	 * to null, will disable persistancy: nothing is stored between browser
	 * sessions Setting the values to a string (or a function which returns a
	 * string), will store the query in localstorage using the specified string.
	 * By default, the ID is dynamically generated by finding the nearest DOM element with an "id" set,
	 * to avoid collissions when using multiple YASR items on one page
	 * 
	 * @property persistency
	 * @type object
	 */
	persistency: {
		prefix: function(yasr) {
			return "yasr_" + $(yasr.container).closest('[id]').attr('id') + "_";
		},
		/**
		 * Persistency setting for the selected output
		 * 
		 * @property persistency.outputSelector
		 * @type string|function
		 * @default function (determine unique id)
		 */
		outputSelector: function(yasr) {
			return "selector";
		},
		/**
		 * Persistency setting for query results.
		 * 
		 * @property persistency.results
		 * @type object
		 */
		results: {
			/**
			 * Get the key to store results in
			 * 
			 * @property persistency.results.id
			 * @type string|function
			 * @default function (determine unique id)
			 */
			id: function(yasr) {
				return "results_" + $(yasr.container).closest('[id]').attr('id');
			},
			key: 'results',
			/**
			 * The result set might too large to fit in local storage. 
			 * It is impossible to detect how large the local storage is.
			 * Therefore, we do not store all results in local storage, depending on a max number of characters in the SPARQL result serialization.
			 * Set this function conservitavely. (especially when using multiple YASR instances on one page)
			 * 
			 * @property persistency.results.maxSize
			 * @type int
			 * @default 100000
			 */
			maxSize: 100000 //char count
		}

	},


};
},{"jquery":undefined}],23:[function(require,module,exports){
'use strict';
var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})();

/**
 * Constructor of plugin which displays SPARQL errors
 * 
 * @param yasr {object}
 * @param parent {DOM element}
 * @param options {object}
 * @class YASR.plugins.boolean
 * @return yasr-erro (doc)
 * 
 */
var root = module.exports = function(yasr) {
	var $container = $("<div class='errorResult'></div>");
	var options = $.extend(true, {}, root.defaults);

	var getTryBtn = function() {
		var $tryBtn = null;
		if (options.tryQueryLink) {
			var link = options.tryQueryLink();
			$tryBtn = $('<button>', {
					class: 'yasr_btn yasr_tryQuery'
				})
				.text('Try query in new browser window')
				.click(function() {
					window.open(link, '_blank');
					$(this).blur();
				})
		}
		return $tryBtn;
	}

	var draw = function() {
		var error = yasr.results.getException();
		$container.empty().appendTo(yasr.resultsContainer);
		var $header = $("<div>", {
			class: 'errorHeader'
		}).appendTo($container);

		if (error.status !== 0) {
			var statusText = 'Error';
			if (error.statusText && error.statusText.length < 100) {
				//use a max: otherwise the alert span will look ugly
				statusText = error.statusText;
			}
			statusText += ' (#' + error.status + ')';

			$header
				.append(
					$("<span>", {
						class: 'exception'
					})
					.text(statusText)
				)
				.append(getTryBtn());

			var responseText = null;
			if (error.responseText) {
				responseText = error.responseText;
			} else if (typeof error == "string") {
				//for backwards compatability (when creating the error string was done externally
				responseText = error;
			}
			if (responseText) $container.append($("<pre>").text(responseText));
		} else {
			$header.append(getTryBtn());
			//cors disabled, wrong url, or endpoint down
			$container
				.append(
					$('<div>', {
						class: 'corsMessage'
					})
					.append(options.corsMessage)
				);
		}

	};


	var canHandleResults = function(yasr) {
		return yasr.results.getException() || false;
	};

	return {
		name: null, //don't need to set this: we don't show it in the selection widget anyway, so don't need a human-friendly name
		draw: draw,
		getPriority: 20,
		hideFromSelection: true,
		canHandleResults: canHandleResults,
	}
};

/**
 * Defaults for error plugin
 * 
 * @type object
 * @attribute YASR.plugins.error.defaults
 */
root.defaults = {
	corsMessage: 'Unable to get response from endpoint',
	tryQueryLink: null,
};
},{"jquery":undefined}],24:[function(require,module,exports){
module.exports = {
	GoogleTypeException: function(foundTypes, varName) {
		this.foundTypes = foundTypes;
		this.varName = varName;
		this.toString = function() {
			var string = 'Conflicting data types found for variable ' + this.varName + '. Assuming all values of this variable are "string".';
			string += ' To avoid this issue, cast the values in your SPARQL query to the intended xsd datatype';

			return string;
		};
		this.toHtml = function() {
			var string = 'Conflicting data types found for variable <i>' + this.varName + '</i>. Assuming all values of this variable are "string".';
			string += ' As a result, several Google Charts will not render values of this particular variable.';
			string += ' To avoid this issue, cast the values in your SPARQL query to the intended xsd datatype';

			return string;
		};
	}
}
},{}],25:[function(require,module,exports){
(function (global){
var EventEmitter = require('events').EventEmitter,
	$ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})();
//cannot package google loader via browserify....
var loadingMain = false;
var loadingFailed = false;
var loader = function() {
	EventEmitter.call(this);
	var mod = this;
	this.init = function() {
		if (!loadingFailed && !(typeof window !== "undefined" ? window.google : typeof global !== "undefined" ? global.google : null) && !loadingMain) { //not initiated yet, not currently loading, and has not failed the previous time
			loadingMain = true;
			/**
			 * It is extremely difficult to catch script loader errors (see http://www.html5rocks.com/en/tutorials/speed/script-loading/)
			 * Existing libraries either ignore several browsers (e.g. jquery 2.x), or use ugly hacks (timeouts or something)
			 * So, we use our own custom ugly hack (yes, timeouts)
			 */
			loadScript('http://google.com/jsapi', function() {
				loadingMain = false;
				mod.emit('initDone');
			});

			var timeout = 100; //ms
			var maxTimeout = 6000; //so 6 sec max
			var startTime = +new Date();
			var checkAndWait = function() {
				if (!(typeof window !== "undefined" ? window.google : typeof global !== "undefined" ? global.google : null)) {
					if ((+new Date() - startTime) > maxTimeout) {
						//ok, we've waited long enough. Obviously we could not load the googleloader...
						loadingFailed = true;
						loadingMain = false;
						mod.emit('initError');

						//TODO: clear initDone callbacks. they won't fire anymore anyway

					} else {
						setTimeout(checkAndWait, timeout);
					}
				} else {
					//TODO: clear initFailed callbacks. they won't fire anymore anyway
				}
			}
			checkAndWait();
		} else {
			if ((typeof window !== "undefined" ? window.google : typeof global !== "undefined" ? global.google : null)) {
				//already loaded! everything is fine
				mod.emit('initDone');
			} else if (loadingFailed) {
				mod.emit('initError')
			} else {
				//hmmm, should never get here
			}

		}
	}
	this.googleLoad = function() {

		var load = function() {
			(typeof window !== "undefined" ? window.google : typeof global !== "undefined" ? global.google : null).load("visualization", "1", {
				packages: ["corechart", "charteditor"],
				callback: function() {
					mod.emit('done')
				}
			})
		}
		if (loadingMain) {
			mod.once('initDone', load);
			mod.once('initError', function() {
				mod.emit('error', 'Could not load google loader')
			});
		} else if ((typeof window !== "undefined" ? window.google : typeof global !== "undefined" ? global.google : null)) {
			//google loader is there. use it
			load();
		} else if (loadingFailed) {
			mod.emit('error', 'Could not load google loader');
		} else {
			//not loading, no loading error, and not loaded. it must not have been initialized yet. Do that
			mod.once('initDone', load);
			mod.once('initError', function() {
				mod.emit('error', 'Could not load google loader')
			});
		}
	};
}


var loadScript = function(url, callback) {
	var script = document.createElement("script")
	script.type = "text/javascript";

	if (script.readyState) { //IE
		script.onreadystatechange = function() {
			if (script.readyState == "loaded" ||
				script.readyState == "complete") {
				script.onreadystatechange = null;
				callback();
			}
		};
	} else { //Others
		script.onload = function() {
			callback();
		};
	}

	script.src = url;
	document.body.appendChild(script);
}
loader.prototype = new EventEmitter;
module.exports = new loader();
}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})

},{"events":4,"jquery":undefined}],26:[function(require,module,exports){
(function (global){
'use strict';
/**
 * todo: chart height as option
 * 
 */
var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})(),
	utils = require('./utils.js'),
	yUtils = require('yasgui-utils');

var root = module.exports = function(yasr) {

	var options = $.extend(true, {}, root.defaults);
	var id = yasr.container.closest('[id]').attr('id');

	var chartWrapper = null;
	var editor = null;

	var initEditor = function(callback) {
		var google = (typeof window !== "undefined" ? window.google : typeof global !== "undefined" ? global.google : null);
		editor = new google.visualization.ChartEditor();
		google.visualization.events.addListener(editor, 'ok', function() {
			var tmp;
			chartWrapper = editor.getChartWrapper();
			tmp = chartWrapper.getDataTable();
			chartWrapper.setDataTable(null);
			//ugly: need to parse json string to json obj again, as google chart does not provide access to object directly
			options.chartConfig = JSON.parse(chartWrapper.toJSON());
			//remove container ID though, for portability
			if (options.chartConfig.containerId) delete options.chartConfig['containerId'];
			yasr.store();
			chartWrapper.setDataTable(tmp);
			chartWrapper.setOption("width", options.width);
			chartWrapper.setOption("height", options.height);
			chartWrapper.draw();
			yasr.updateHeader();
		});
		if (callback) callback();
	};

	return {
		name: "Google Chart",
		hideFromSelection: false,
		priority: 7,
		options: options,
		getPersistentSettings: function() {
			return {
				chartConfig: options.chartConfig,
				motionChartState: options.motionChartState
			}
		},
		setPersistentSettings: function(persSettings) {
			if (persSettings['chartConfig']) options.chartConfig = persSettings['chartConfig'];
			if (persSettings['motionChartState']) options.motionChartState = persSettings['motionChartState'];
		},
		canHandleResults: function(yasr) {
			var results, variables;
			return (results = yasr.results) != null && (variables = results.getVariables()) && variables.length > 0;
		},
		getDownloadInfo: function() {
			if (!yasr.results) return null;
			var svgEl = yasr.resultsContainer.find('svg');
			if (svgEl.length > 0) {
				return {
					getContent: function() {
						if (svgEl[0].outerHTML) {
							return svgEl[0].outerHTML;
						} else {
							//outerHTML not supported. use workaround
							return $('<div>').append(svgEl.clone()).html();
						}
					},
					filename: "queryResults.svg",
					contentType: "image/svg+xml",
					buttonTitle: "Download SVG Image"
				};
			}
			//ok, not a svg. is it a table?
			var $table = yasr.resultsContainer.find('.google-visualization-table-table');
			if ($table.length > 0) {
				return {
					getContent: function() {
						return $table.tableToCsv();
					},
					filename: "queryResults.csv",
					contentType: "text/csv",
					buttonTitle: "Download as CSV"
				};
			}
		},
		getEmbedHtml: function() {
			if (!yasr.results) return null;

			var svgEl = yasr.resultsContainer.find('svg')
				.clone() //create clone, as we'd like to remove height/width attributes
				.removeAttr('height').removeAttr('width')
				.css('height', '').css('width', '');
			if (svgEl.length == 0) return null;

			var htmlString = svgEl[0].outerHTML;
			if (!htmlString) {
				//outerHTML not supported. use workaround
				htmlString = $('<div>').append(svgEl.clone()).html();
			}
			//wrap in div, so users can more easily tune width/height
			//don't use jquery, so we can easily influence indentation
			return '<div style="width: 800px; height: 600px;">\n' + htmlString + '\n</div>';
		},
		draw: function() {
			var doDraw = function() {
				//clear previous results (if any)
				yasr.resultsContainer.empty();
				var wrapperId = id + '_gchartWrapper';

				yasr.resultsContainer.append(
					$('<button>', {
						class: 'openGchartBtn yasr_btn'
					})
					.text('Chart Config')
					.click(function() {
						editor.openDialog(chartWrapper);
					})
				).append(
					$('<div>', {
						id: wrapperId,
						class: 'gchartWrapper'
					})
				);
				var dataTable = new google.visualization.DataTable();
				var jsonResults = yasr.results.getAsJson();

				jsonResults.head.vars.forEach(function(variable) {
					var type = 'string';
					try {
						type = utils.getGoogleTypeForBindings(jsonResults.results.bindings, variable);
					} catch (e) {
						if (e instanceof require('./exceptions.js').GoogleTypeException) {
							yasr.warn(e.toHtml())
						} else {
							throw e;
						}
					}
					dataTable.addColumn(type, variable);
				});
				var usedPrefixes = null;
				if (yasr.options.getUsedPrefixes) {
					usedPrefixes = (typeof yasr.options.getUsedPrefixes == "function" ? yasr.options.getUsedPrefixes(yasr) : yasr.options.getUsedPrefixes);
				}
				jsonResults.results.bindings.forEach(function(binding) {
					var row = [];
					jsonResults.head.vars.forEach(function(variable, columnId) {
						row.push(utils.castGoogleType(binding[variable], usedPrefixes, dataTable.getColumnType(columnId)));
					})
					dataTable.addRow(row);
				});

				if (options.chartConfig && options.chartConfig.chartType) {
					options.chartConfig.containerId = wrapperId;
					chartWrapper = new google.visualization.ChartWrapper(options.chartConfig);
					if (chartWrapper.getChartType() === "MotionChart" && options.motionChartState) {
						chartWrapper.setOption("state", options.motionChartState);
						google.visualization.events.addListener(chartWrapper, 'ready', function() {
							var motionChart;
							motionChart = chartWrapper.getChart();
							google.visualization.events.addListener(motionChart, 'statechange', function() {
								options.motionChartState = motionChart.getState();
								yasr.store();
							});
						});
					}
					chartWrapper.setDataTable(dataTable);
				} else {
					chartWrapper = new google.visualization.ChartWrapper({
						chartType: 'Table',
						dataTable: dataTable,
						containerId: wrapperId
					});
				}
				chartWrapper.setOption("width", options.width);
				chartWrapper.setOption("height", options.height);
				chartWrapper.draw();
				google.visualization.events.addListener(chartWrapper, 'ready', yasr.updateHeader);
			}

			if (!(typeof window !== "undefined" ? window.google : typeof global !== "undefined" ? global.google : null) || !(typeof window !== "undefined" ? window.google : typeof global !== "undefined" ? global.google : null).visualization || !editor) {
				require('./gChartLoader.js')
					.on('done', function() {
						initEditor();
						doDraw();
					})
					.on('error', function() {
						//TODO: disable or something?
					})
					.googleLoad();
			} else {
				//everything (editor as well) is already initialized
				doDraw();
			}
		}
	};
};
root.defaults = {
	height: "100%",
	width: "100%",
	persistencyId: 'gchart',
	chartConfig: null,
	motionChartState: null
};

function deepEq$(x, y, type) {
	var toString = {}.toString,
		hasOwnProperty = {}.hasOwnProperty,
		has = function(obj, key) {
			return hasOwnProperty.call(obj, key);
		};
	var first = true;
	return eq(x, y, []);

	function eq(a, b, stack) {
		var className, length, size, result, alength, blength, r, key, ref, sizeB;
		if (a == null || b == null) {
			return a === b;
		}
		if (a.__placeholder__ || b.__placeholder__) {
			return true;
		}
		if (a === b) {
			return a !== 0 || 1 / a == 1 / b;
		}
		className = toString.call(a);
		if (toString.call(b) != className) {
			return false;
		}
		switch (className) {
			case '[object String]':
				return a == String(b);
			case '[object Number]':
				return a != +a ? b != +b : (a == 0 ? 1 / a == 1 / b : a == +b);
			case '[object Date]':
			case '[object Boolean]':
				return +a == +b;
			case '[object RegExp]':
				return a.source == b.source &&
					a.global == b.global &&
					a.multiline == b.multiline &&
					a.ignoreCase == b.ignoreCase;
		}
		if (typeof a != 'object' || typeof b != 'object') {
			return false;
		}
		length = stack.length;
		while (length--) {
			if (stack[length] == a) {
				return true;
			}
		}
		stack.push(a);
		size = 0;
		result = true;
		if (className == '[object Array]') {
			alength = a.length;
			blength = b.length;
			if (first) {
				switch (type) {
					case '===':
						result = alength === blength;
						break;
					case '<==':
						result = alength <= blength;
						break;
					case '<<=':
						result = alength < blength;
						break;
				}
				size = alength;
				first = false;
			} else {
				result = alength === blength;
				size = alength;
			}
			if (result) {
				while (size--) {
					if (!(result = size in a == size in b && eq(a[size], b[size], stack))) {
						break;
					}
				}
			}
		} else {
			if ('constructor' in a != 'constructor' in b || a.constructor != b.constructor) {
				return false;
			}
			for (key in a) {
				if (has(a, key)) {
					size++;
					if (!(result = has(b, key) && eq(a[key], b[key], stack))) {
						break;
					}
				}
			}
			if (result) {
				sizeB = 0;
				for (key in b) {
					if (has(b, key)) {
						++sizeB;
					}
				}
				if (first) {
					if (type === '<<=') {
						result = size < sizeB;
					} else if (type === '<==') {
						result = size <= sizeB
					} else {
						result = size === sizeB;
					}
				} else {
					first = false;
					result = size === sizeB;
				}
			}
		}
		stack.pop();
		return result;
	}
}
}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})

},{"./exceptions.js":24,"./gChartLoader.js":25,"./utils.js":40,"jquery":undefined,"yasgui-utils":16}],27:[function(require,module,exports){
'use strict';
module.exports = {
	cross: '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" x="0px" y="0px" width="30px" height="30px" viewBox="0 0 100 100" enable-background="new 0 0 100 100" xml:space="preserve"><g>	<path d="M83.288,88.13c-2.114,2.112-5.575,2.112-7.689,0L53.659,66.188c-2.114-2.112-5.573-2.112-7.687,0L24.251,87.907   c-2.113,2.114-5.571,2.114-7.686,0l-4.693-4.691c-2.114-2.114-2.114-5.573,0-7.688l21.719-21.721c2.113-2.114,2.113-5.573,0-7.686   L11.872,24.4c-2.114-2.113-2.114-5.571,0-7.686l4.842-4.842c2.113-2.114,5.571-2.114,7.686,0L46.12,33.591   c2.114,2.114,5.572,2.114,7.688,0l21.721-21.719c2.114-2.114,5.573-2.114,7.687,0l4.695,4.695c2.111,2.113,2.111,5.571-0.003,7.686   L66.188,45.973c-2.112,2.114-2.112,5.573,0,7.686L88.13,75.602c2.112,2.111,2.112,5.572,0,7.687L83.288,88.13z"/></g></svg>',
	check: '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" x="0px" y="0px" width="30px" height="30px" viewBox="0 0 100 100" enable-background="new 0 0 100 100" xml:space="preserve"><path fill="#000000" d="M14.301,49.982l22.606,17.047L84.361,4.903c2.614-3.733,7.76-4.64,11.493-2.026l0.627,0.462  c3.732,2.614,4.64,7.758,2.025,11.492l-51.783,79.77c-1.955,2.791-3.896,3.762-7.301,3.988c-3.405,0.225-5.464-1.039-7.508-3.084  L2.447,61.814c-3.263-3.262-3.263-8.553,0-11.814l0.041-0.019C5.75,46.718,11.039,46.718,14.301,49.982z"/></svg>',
	unsorted: '<svg   xmlns:dc="http://purl.org/dc/elements/1.1/"   xmlns:cc="http://creativecommons.org/ns#"   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"   xmlns:svg="http://www.w3.org/2000/svg"   xmlns="http://www.w3.org/2000/svg"   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"   version="1.1"   id="Layer_1"   x="0px"   y="0px"   width="100%"   height="100%"   viewBox="0 0 54.552711 113.78478"   enable-background="new 0 0 100 100"   xml:space="preserve"><g     id="g5"     transform="matrix(-0.70522156,-0.70898699,-0.70898699,0.70522156,97.988199,55.081205)"><path       style="fill:#000000"       inkscape:connector-curvature="0"       id="path7"       d="M 57.911,66.915 45.808,55.063 42.904,52.238 31.661,41.25 31.435,41.083 31.131,40.775 30.794,40.523 30.486,40.3 30.069,40.05 29.815,39.911 29.285,39.659 29.089,39.576 28.474,39.326 28.363,39.297 H 28.336 L 27.665,39.128 27.526,39.1 26.94,38.99 26.714,38.961 26.212,38.934 h -0.31 -0.444 l -0.339,0.027 c -1.45,0.139 -2.876,0.671 -4.11,1.564 l -0.223,0.141 -0.279,0.25 -0.335,0.308 -0.054,0.029 -0.171,0.194 -0.334,0.364 -0.224,0.279 -0.25,0.336 -0.225,0.362 -0.192,0.308 -0.197,0.421 -0.142,0.279 -0.193,0.477 -0.084,0.222 -12.441,38.414 c -0.814,2.458 -0.313,5.029 1.115,6.988 v 0.026 l 0.418,0.532 0.17,0.165 0.251,0.281 0.084,0.079 0.283,0.281 0.25,0.194 0.474,0.367 0.083,0.053 c 2.015,1.371 4.641,1.874 7.131,1.094 L 55.228,80.776 c 4.303,-1.342 6.679,-5.814 5.308,-10.006 -0.387,-1.259 -1.086,-2.35 -1.979,-3.215 l -0.368,-0.337 -0.278,-0.303 z m -6.318,5.896 0.079,0.114 -37.369,11.57 11.854,-36.538 10.565,10.317 2.876,2.825 11.995,11.712 z" /></g><path     style="fill:#000000"     inkscape:connector-curvature="0"     id="path7-9"     d="m 8.8748339,52.571766 16.9382111,-0.222584 4.050851,-0.06665 15.719154,-0.222166 0.27778,-0.04246 0.43276,0.0017 0.41632,-0.06121 0.37532,-0.0611 0.47132,-0.119342 0.27767,-0.08206 0.55244,-0.198047 0.19707,-0.08043 0.61095,-0.259721 0.0988,-0.05825 0.019,-0.01914 0.59303,-0.356548 0.11787,-0.0788 0.49125,-0.337892 0.17994,-0.139779 0.37317,-0.336871 0.21862,-0.219786 0.31311,-0.31479 0.21993,-0.259387 c 0.92402,-1.126057 1.55249,-2.512251 1.78961,-4.016904 l 0.0573,-0.25754 0.0195,-0.374113 0.0179,-0.454719 0.0175,-0.05874 -0.0169,-0.258049 -0.0225,-0.493503 -0.0398,-0.355569 -0.0619,-0.414201 -0.098,-0.414812 -0.083,-0.353334 L 53.23955,41.1484 53.14185,40.850967 52.93977,40.377742 52.84157,40.161628 34.38021,4.2507375 C 33.211567,1.9401875 31.035446,0.48226552 28.639484,0.11316952 l -0.01843,-0.01834 -0.671963,-0.07882 -0.236871,0.0042 L 27.335984,-4.7826577e-7 27.220736,0.00379952 l -0.398804,0.0025 -0.313848,0.04043 -0.594474,0.07724 -0.09611,0.02147 C 23.424549,0.60716252 21.216017,2.1142355 20.013025,4.4296865 L 0.93967491,40.894479 c -2.08310801,3.997178 -0.588125,8.835482 3.35080799,10.819749 1.165535,0.613495 2.43199,0.88731 3.675026,0.864202 l 0.49845,-0.02325 0.410875,0.01658 z M 9.1502369,43.934401 9.0136999,43.910011 27.164145,9.2564625 44.70942,43.42818 l -14.765289,0.214677 -4.031106,0.0468 -16.7627881,0.244744 z" /></svg>',
	sortDesc: '<svg   xmlns:dc="http://purl.org/dc/elements/1.1/"   xmlns:cc="http://creativecommons.org/ns#"   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"   xmlns:svg="http://www.w3.org/2000/svg"   xmlns="http://www.w3.org/2000/svg"   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"   version="1.1"   id="Layer_1"   x="0px"   y="0px"   width="100%"   height="100%"   viewBox="0 0 54.552711 113.78478"   enable-background="new 0 0 100 100"   xml:space="preserve"><g     id="g5"     transform="matrix(-0.70522156,-0.70898699,-0.70898699,0.70522156,97.988199,55.081205)"><path       style="fill:#000000"       inkscape:connector-curvature="0"       id="path7"       d="M 57.911,66.915 45.808,55.063 42.904,52.238 31.661,41.25 31.435,41.083 31.131,40.775 30.794,40.523 30.486,40.3 30.069,40.05 29.815,39.911 29.285,39.659 29.089,39.576 28.474,39.326 28.363,39.297 H 28.336 L 27.665,39.128 27.526,39.1 26.94,38.99 26.714,38.961 26.212,38.934 h -0.31 -0.444 l -0.339,0.027 c -1.45,0.139 -2.876,0.671 -4.11,1.564 l -0.223,0.141 -0.279,0.25 -0.335,0.308 -0.054,0.029 -0.171,0.194 -0.334,0.364 -0.224,0.279 -0.25,0.336 -0.225,0.362 -0.192,0.308 -0.197,0.421 -0.142,0.279 -0.193,0.477 -0.084,0.222 -12.441,38.414 c -0.814,2.458 -0.313,5.029 1.115,6.988 v 0.026 l 0.418,0.532 0.17,0.165 0.251,0.281 0.084,0.079 0.283,0.281 0.25,0.194 0.474,0.367 0.083,0.053 c 2.015,1.371 4.641,1.874 7.131,1.094 L 55.228,80.776 c 4.303,-1.342 6.679,-5.814 5.308,-10.006 -0.387,-1.259 -1.086,-2.35 -1.979,-3.215 l -0.368,-0.337 -0.278,-0.303 z m -6.318,5.896 0.079,0.114 -37.369,11.57 11.854,-36.538 10.565,10.317 2.876,2.825 11.995,11.712 z" /></g><path     style="fill:#000000"     inkscape:connector-curvature="0"     id="path9"     d="m 27.813273,0.12823506 0.09753,0.02006 c 2.39093,0.458209 4.599455,1.96811104 5.80244,4.28639004 L 52.785897,40.894525 c 2.088044,4.002139 0.590949,8.836902 -3.348692,10.821875 -1.329078,0.688721 -2.766603,0.943695 -4.133174,0.841768 l -0.454018,0.02 L 27.910392,52.354171 23.855313,52.281851 8.14393,52.061827 7.862608,52.021477 7.429856,52.021738 7.014241,51.959818 6.638216,51.900838 6.164776,51.779369 5.889216,51.699439 5.338907,51.500691 5.139719,51.419551 4.545064,51.145023 4.430618,51.105123 4.410168,51.084563 3.817138,50.730843 3.693615,50.647783 3.207314,50.310611 3.028071,50.174369 2.652795,49.833957 2.433471,49.613462 2.140099,49.318523 1.901127,49.041407 C 0.97781,47.916059 0.347935,46.528448 0.11153,45.021676 L 0.05352,44.766255 0.05172,44.371683 0.01894,43.936017 0,43.877277 0.01836,43.62206 0.03666,43.122889 0.0765,42.765905 0.13912,42.352413 0.23568,41.940425 0.32288,41.588517 0.481021,41.151945 0.579391,40.853806 0.77369,40.381268 0.876097,40.162336 19.338869,4.2542801 c 1.172169,-2.308419 3.34759,-3.76846504 5.740829,-4.17716604 l 0.01975,0.01985 0.69605,-0.09573 0.218437,0.0225 0.490791,-0.02132 0.39809,0.0046 0.315972,0.03973 0.594462,0.08149 z" /></svg>',
	sortAsc: '<svg   xmlns:dc="http://purl.org/dc/elements/1.1/"   xmlns:cc="http://creativecommons.org/ns#"   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"   xmlns:svg="http://www.w3.org/2000/svg"   xmlns="http://www.w3.org/2000/svg"   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"   version="1.1"   id="Layer_1"   x="0px"   y="0px"   width="100%"   height="100%"   viewBox="0 0 54.552711 113.78478"   enable-background="new 0 0 100 100"   xml:space="preserve"><g     id="g5"     transform="matrix(-0.70522156,0.70898699,-0.70898699,-0.70522156,97.988199,58.704807)"><path       style="fill:#000000"       inkscape:connector-curvature="0"       id="path7"       d="M 57.911,66.915 45.808,55.063 42.904,52.238 31.661,41.25 31.435,41.083 31.131,40.775 30.794,40.523 30.486,40.3 30.069,40.05 29.815,39.911 29.285,39.659 29.089,39.576 28.474,39.326 28.363,39.297 H 28.336 L 27.665,39.128 27.526,39.1 26.94,38.99 26.714,38.961 26.212,38.934 h -0.31 -0.444 l -0.339,0.027 c -1.45,0.139 -2.876,0.671 -4.11,1.564 l -0.223,0.141 -0.279,0.25 -0.335,0.308 -0.054,0.029 -0.171,0.194 -0.334,0.364 -0.224,0.279 -0.25,0.336 -0.225,0.362 -0.192,0.308 -0.197,0.421 -0.142,0.279 -0.193,0.477 -0.084,0.222 -12.441,38.414 c -0.814,2.458 -0.313,5.029 1.115,6.988 v 0.026 l 0.418,0.532 0.17,0.165 0.251,0.281 0.084,0.079 0.283,0.281 0.25,0.194 0.474,0.367 0.083,0.053 c 2.015,1.371 4.641,1.874 7.131,1.094 L 55.228,80.776 c 4.303,-1.342 6.679,-5.814 5.308,-10.006 -0.387,-1.259 -1.086,-2.35 -1.979,-3.215 l -0.368,-0.337 -0.278,-0.303 z m -6.318,5.896 0.079,0.114 -37.369,11.57 11.854,-36.538 10.565,10.317 2.876,2.825 11.995,11.712 z" /></g><path     style="fill:#000000"     inkscape:connector-curvature="0"     id="path9"     d="m 27.813273,113.65778 0.09753,-0.0201 c 2.39093,-0.45821 4.599455,-1.96811 5.80244,-4.28639 L 52.785897,72.891487 c 2.088044,-4.002139 0.590949,-8.836902 -3.348692,-10.821875 -1.329078,-0.688721 -2.766603,-0.943695 -4.133174,-0.841768 l -0.454018,-0.02 -16.939621,0.223997 -4.055079,0.07232 -15.711383,0.220024 -0.281322,0.04035 -0.432752,-2.61e-4 -0.415615,0.06192 -0.376025,0.05898 -0.47344,0.121469 -0.27556,0.07993 -0.550309,0.198748 -0.199188,0.08114 -0.594655,0.274528 -0.114446,0.0399 -0.02045,0.02056 -0.59303,0.35372 -0.123523,0.08306 -0.486301,0.337172 -0.179243,0.136242 -0.375276,0.340412 -0.219324,0.220495 -0.293372,0.294939 -0.238972,0.277116 C 0.97781,65.869953 0.347935,67.257564 0.11153,68.764336 L 0.05352,69.019757 0.05172,69.414329 0.01894,69.849995 0,69.908735 l 0.01836,0.255217 0.0183,0.499171 0.03984,0.356984 0.06262,0.413492 0.09656,0.411988 0.0872,0.351908 0.158141,0.436572 0.09837,0.298139 0.194299,0.472538 0.102407,0.218932 18.462772,35.908054 c 1.172169,2.30842 3.34759,3.76847 5.740829,4.17717 l 0.01975,-0.0199 0.69605,0.0957 0.218437,-0.0225 0.490791,0.0213 0.39809,-0.005 0.315972,-0.0397 0.594462,-0.0815 z" /></svg>',
	download: '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" baseProfile="tiny" x="0px" y="0px" width="100%" height="100%" viewBox="0 0 100 100" xml:space="preserve"><g id="Captions"></g><g id="Your_Icon">	<path fill-rule="evenodd" fill="#000000" d="M88,84v-2c0-2.961-0.859-4-4-4H16c-2.961,0-4,0.98-4,4v2c0,3.102,1.039,4,4,4h68   C87.02,88,88,87.039,88,84z M58,12H42c-5,0-6,0.941-6,6v22H16l34,34l34-34H64V18C64,12.941,62.939,12,58,12z"/></g></svg>',
	move: '<svg   xmlns:dc="http://purl.org/dc/elements/1.1/"   xmlns:cc="http://creativecommons.org/ns#"   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"   xmlns:svg="http://www.w3.org/2000/svg"   xmlns="http://www.w3.org/2000/svg"   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"   version="1.1"   id="Layer_1"   x="0px"   y="0px"   width="100%"   height="100%"   viewBox="5 -10 74.074074 100"   enable-background="new 0 0 100 100"   xml:space="preserve"   inkscape:version="0.48.4 r9939"   sodipodi:docname="noun_11656_cc.svg"><metadata     ><rdf:RDF><cc:Work         rdf:about=""><dc:format>image/svg+xml</dc:format><dc:type           rdf:resource="http://purl.org/dc/dcmitype/StillImage" /></cc:Work></rdf:RDF></metadata><defs      /><sodipodi:namedview     pagecolor="#ffffff"     bordercolor="#666666"     borderopacity="1"     objecttolerance="10"     gridtolerance="10"     guidetolerance="10"     inkscape:pageopacity="0"     inkscape:pageshadow="2"     inkscape:window-width="753"     inkscape:window-height="480"          showgrid="false"     fit-margin-top="0"     fit-margin-left="0"     fit-margin-right="0"     fit-margin-bottom="0"     inkscape:zoom="2.36"     inkscape:cx="44.101509"     inkscape:cy="31.481481"     inkscape:window-x="287"     inkscape:window-y="249"     inkscape:window-maximized="0"     inkscape:current-layer="Layer_1" /><polygon     points="33,83 50,100 67,83 54,83 54,17 67,17 50,0 33,17 46,17 46,83 "          transform="translate(-7.962963,-10)" /><polygon     points="83,67 100,50 83,33 83,46 17,46 17,33 0,50 17,67 17,54 83,54 "          transform="translate(-7.962963,-10)" /></svg>',
	fullscreen: '<svg   xmlns:dc="http://purl.org/dc/elements/1.1/"   xmlns:cc="http://creativecommons.org/ns#"   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"   xmlns:svg="http://www.w3.org/2000/svg"   xmlns="http://www.w3.org/2000/svg"   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"   version="1.1"      x="0px"   y="0px"   width="100%"   height="100%"   viewBox="5 -10 74.074074 100"   enable-background="new 0 0 100 100"   xml:space="preserve"   inkscape:version="0.48.4 r9939"   sodipodi:docname="noun_2186_cc.svg"><metadata     ><rdf:RDF><cc:Work         rdf:about=""><dc:format>image/svg+xml</dc:format><dc:type           rdf:resource="http://purl.org/dc/dcmitype/StillImage" /></cc:Work></rdf:RDF></metadata><defs      /><sodipodi:namedview     pagecolor="#ffffff"     bordercolor="#666666"     borderopacity="1"     objecttolerance="10"     gridtolerance="10"     guidetolerance="10"     inkscape:pageopacity="0"     inkscape:pageshadow="2"     inkscape:window-width="640"     inkscape:window-height="480"          showgrid="false"     fit-margin-top="0"     fit-margin-left="0"     fit-margin-right="0"     fit-margin-bottom="0"     inkscape:zoom="2.36"     inkscape:cx="44.101509"     inkscape:cy="31.481481"     inkscape:window-x="65"     inkscape:window-y="24"     inkscape:window-maximized="0"     inkscape:current-layer="Layer_1" /><path     d="m -7.962963,-10 v 38.889 l 16.667,-16.667 16.667,16.667 5.555,-5.555 -16.667,-16.667 16.667,-16.667 h -38.889 z"          inkscape:connector-curvature="0"     style="fill:#010101" /><path     d="m 92.037037,-10 v 38.889 l -16.667,-16.667 -16.666,16.667 -5.556,-5.555 16.666,-16.667 -16.666,-16.667 h 38.889 z"          inkscape:connector-curvature="0"     style="fill:#010101" /><path     d="M -7.962963,90 V 51.111 l 16.667,16.666 16.667,-16.666 5.555,5.556 -16.667,16.666 16.667,16.667 h -38.889 z"          inkscape:connector-curvature="0"     style="fill:#010101" /><path     d="M 92.037037,90 V 51.111 l -16.667,16.666 -16.666,-16.666 -5.556,5.556 16.666,16.666 -16.666,16.667 h 38.889 z"          inkscape:connector-curvature="0"     style="fill:#010101" /></svg>',
	smallscreen: '<svg   xmlns:dc="http://purl.org/dc/elements/1.1/"   xmlns:cc="http://creativecommons.org/ns#"   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"   xmlns:svg="http://www.w3.org/2000/svg"   xmlns="http://www.w3.org/2000/svg"   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"   version="1.1"      x="0px"   y="0px"   width="100%"   height="100%"   viewBox="5 -10 74.074074 100"   enable-background="new 0 0 100 100"   xml:space="preserve"   inkscape:version="0.48.4 r9939"   sodipodi:docname="noun_2186_cc.svg"><metadata     ><rdf:RDF><cc:Work         rdf:about=""><dc:format>image/svg+xml</dc:format><dc:type           rdf:resource="http://purl.org/dc/dcmitype/StillImage" /></cc:Work></rdf:RDF></metadata><defs      /><sodipodi:namedview     pagecolor="#ffffff"     bordercolor="#666666"     borderopacity="1"     objecttolerance="10"     gridtolerance="10"     guidetolerance="10"     inkscape:pageopacity="0"     inkscape:pageshadow="2"     inkscape:window-width="1855"     inkscape:window-height="1056"          showgrid="false"     fit-margin-top="0"     fit-margin-left="0"     fit-margin-right="0"     fit-margin-bottom="0"     inkscape:zoom="2.36"     inkscape:cx="44.101509"     inkscape:cy="31.481481"     inkscape:window-x="65"     inkscape:window-y="24"     inkscape:window-maximized="1"     inkscape:current-layer="Layer_1" /><path     d="m 30.926037,28.889 0,-38.889 -16.667,16.667 -16.667,-16.667 -5.555,5.555 16.667,16.667 -16.667,16.667 38.889,0 z"          inkscape:connector-curvature="0"     style="fill:#010101" /><path     d="m 53.148037,28.889 0,-38.889 16.667,16.667 16.666,-16.667 5.556,5.555 -16.666,16.667 16.666,16.667 -38.889,0 z"          inkscape:connector-curvature="0"     style="fill:#010101" /><path     d="m 30.926037,51.111 0,38.889 -16.667,-16.666 -16.667,16.666 -5.555,-5.556 16.667,-16.666 -16.667,-16.667 38.889,0 z"          inkscape:connector-curvature="0"     style="fill:#010101" /><path     d="m 53.148037,51.111 0,38.889 16.667,-16.666 16.666,16.666 5.556,-5.556 -16.666,-16.666 16.666,-16.667 -38.889,0 z"          inkscape:connector-curvature="0"     style="fill:#010101" /></svg>',
};
},{}],28:[function(require,module,exports){
require('./tableToCsv.js');
},{"./tableToCsv.js":29}],29:[function(require,module,exports){
'use strict';
var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})();


$.fn.tableToCsv = function(config) {
	var csvString = "";
	config = $.extend({
		quote: "\"",
		delimiter: ",",
		lineBreak: "\n",
	}, config)




	var needToQuoteString = function(value) {
		//quote when it contains whitespace or the delimiter
		var needQuoting = false;
		if (value.match("[\\w|" + config.delimiter + "|" + config.quote + "]")) {
			needQuoting = true;
		}
		return needQuoting;
	};
	var addValueToString = function(value) {
		//Quotes in the string need to be escaped
		value.replace(config.quote, config.quote + config.quote);
		if (needToQuoteString(value)) {
			value = config.quote + value + config.quote;
		}
		csvString += " " + value + " " + config.delimiter;
	};

	var addRowToString = function(rowArray) {
		rowArray.forEach(function(val) {
			addValueToString(val);
		});
		csvString += config.lineBreak;
	}

	var tableArrays = [];
	var $el = $(this);
	var rowspans = {};



	var totalColCount = 0;
	$el.find('tr:first *').each(function() {
		if ($(this).attr('colspan')) {
			totalColCount += +$(this).attr('colspan');
		} else {
			totalColCount++;
		}
	});

	$el.find('tr').each(function(rowId, tr) {
		var $tr = $(tr);
		var rowArray = []

		var htmlColId = 0;
		var actualColId = 0;
		while (actualColId < totalColCount) {
			if (rowspans[actualColId]) {
				rowArray.push(rowspans[actualColId].text);
				rowspans[actualColId].rowSpan--;
				if (!rowspans[actualColId].rowSpan) rowspans[actualColId] = null;
				actualColId++;
				continue;
			}

			var $cell = $tr.find(':nth-child(' + (htmlColId + 1) + ')');
			if (!$cell) break;
			var colspan = $cell.attr('colspan') || 1;
			var rowspan = $cell.attr('rowspan') || 1;

			for (var i = 0; i < colspan; i++) {
				rowArray.push($cell.text());
				if (rowspan > 1) {
					rowspans[actualColId] = {
						rowSpan: rowspan - 1,
						text: $cell.text(),
					}
				}
				actualColId++;
			}
			htmlColId++;
		}
		addRowToString(rowArray);


	})

	return csvString;
}

},{"jquery":undefined}],30:[function(require,module,exports){
'use strict';
var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})(),
	EventEmitter = require('events').EventEmitter,
	utils = require("yasgui-utils");
console = console || {
	"log": function() {}
}; //make sure any console statements don't break in IE

require('./jquery/extendJquery.js');


/**
 * Main YASR constructor
 *
 * @constructor
 * @param {DOM-Element} parent element to append editor to.
 * @param {object} settings
 * @class YASR
 * @return {doc} YASR document
 */
var YASR = function(parent, options, queryResults) {
	EventEmitter.call(this);
	var yasr = this;
	// console.log(EventEmitter.call(this));

	// var yasr = {};
	// EventEmitter.call(yasr);
	yasr.options = $.extend(true, {}, module.exports.defaults, options);
	//the recursive copy does merge (overwrite) array values how we want it to. Do this manually
	if (options.outputPlugins) yasr.options.outputPlugins = options.outputPlugins;

	yasr.container = $("<div class='yasr'></div>").appendTo(parent);
	yasr.header = $("<div class='yasr_header'></div>").appendTo(yasr.container);
	yasr.resultsContainer = $("<div class='yasr_results'></div>").appendTo(yasr.container);
	yasr.storage = utils.storage;

	var prefix = null;
	yasr.getPersistencyId = function(postfix) {
		if (prefix === null) {
			//instantiate prefix
			if (yasr.options.persistency && yasr.options.persistency.prefix) {
				prefix = (typeof yasr.options.persistency.prefix == 'string' ? yasr.options.persistency.prefix : yasr.options.persistency.prefix(yasr));
			} else {
				prefix = false;
			}
		}
		if (prefix && postfix != null) {
			return prefix + (typeof postfix == 'string' ? postfix : postfix(yasr));
		} else {
			return null;
		}
	};

	if (yasr.options.useGoogleCharts) {
		//pre-load google-loader
		require('./gChartLoader.js')
			.once('initError', function() {
				yasr.options.useGoogleCharts = false
			})
			.init();
	}

	//first initialize plugins
	yasr.plugins = {};
	for (var pluginName in module.exports.plugins) {
		if (!yasr.options.useGoogleCharts && pluginName == "gchart") continue;
		yasr.plugins[pluginName] = new module.exports.plugins[pluginName](yasr);
	}


	yasr.updateHeader = function() {
		var downloadIcon = yasr.header.find(".yasr_downloadIcon")
			.removeAttr("title"); //and remove previous titles
		var embedButton = yasr.header.find(".yasr_embedBtn");
		var outputPlugin = yasr.plugins[yasr.options.output];
		if (outputPlugin) {

			//Manage download link
			var info = (outputPlugin.getDownloadInfo ? outputPlugin.getDownloadInfo() : null);
			if (info) {
				if (info.buttonTitle) downloadIcon.attr('title', info.buttonTitle);
				downloadIcon.prop("disabled", false);
				downloadIcon.find("path").each(function() {
					this.style.fill = "black";
				});
			} else {
				downloadIcon.prop("disabled", true).prop("title", "Download not supported for this result representation");
				downloadIcon.find("path").each(function() {
					this.style.fill = "gray";
				});
			}

			//Manage embed button
			var link = null;
			if (outputPlugin.getEmbedHtml) link = outputPlugin.getEmbedHtml();
			if (link && link.length > 0) {
				embedButton.show();
			} else {
				embedButton.hide();
			}
		}
	};
	yasr.draw = function(output) {
		if (!yasr.results) return false;
		if (!output) output = yasr.options.output;


		//ah, our default output does not take our current results. Try to autodetect
		var selectedOutput = null;
		var selectedOutputPriority = -1;
		var unsupportedOutputs = [];
		for (var tryOutput in yasr.plugins) {
			if (yasr.plugins[tryOutput].canHandleResults(yasr)) {
				var priority = yasr.plugins[tryOutput].getPriority;
				if (typeof priority == "function") priority = priority(yasr);
				if (priority != null && priority != undefined && priority > selectedOutputPriority) {
					selectedOutputPriority = priority;
					selectedOutput = tryOutput;
				}
			} else {
				unsupportedOutputs.push(tryOutput);
			}
		}
		disableOutputs(unsupportedOutputs);
		var outputToDraw = null;
		if (output in yasr.plugins && yasr.plugins[output].canHandleResults(yasr)) {
			outputToDraw = output;
		} else if (selectedOutput) {
			outputToDraw = selectedOutput;
		}

		if (outputToDraw) {
			$(yasr.resultsContainer).empty();
			yasr.emit('draw', yasr, yasr.plugins[outputToDraw]);
			yasr.plugins[outputToDraw].draw();
			yasr.emit('drawn', yasr, yasr.plugins[outputToDraw]);
			yasr.updateHeader();
			return true;
		} else {
			yasr.updateHeader();
			return false;
		}
	};

	var disableOutputs = function(outputs) {
		//first enable everything.
		yasr.header.find('.yasr_btnGroup .yasr_btn').removeClass('disabled');


		//now disable the outputs passed as param
		outputs.forEach(function(outputName) {
			yasr.header.find('.yasr_btnGroup .select_' + outputName).addClass('disabled');
		});

	};
	yasr.somethingDrawn = function() {
		return !yasr.resultsContainer.is(":empty");
	};

	yasr.setResponse = function(dataOrJqXhr, textStatus, jqXhrOrErrorString) {
		try {
			yasr.results = require("./parsers/wrapper.js")(dataOrJqXhr, textStatus, jqXhrOrErrorString);
		} catch (exception) {
			yasr.results = {
				getException: function() {
					return exception
				}
			};
		}
		yasr.draw();

		//store if needed
		var resultsId = yasr.getPersistencyId(yasr.options.persistency.results.key);
		if (resultsId) {
			if (yasr.results.getOriginalResponseAsString && yasr.results.getOriginalResponseAsString().length < yasr.options.persistency.results.maxSize) {
				utils.storage.set(resultsId, yasr.results.getAsStoreObject(), "month");
			} else {
				//remove old string
				utils.storage.remove(resultsId);
			}
		}
	};
	var $toggableWarning = null;
	var $toggableWarningClose = null;
	var $toggableWarningMsg = null;
	yasr.warn = function(warning) {
		if (!$toggableWarning) {
			//first time instantiation
			$toggableWarning = $('<div>', {
				class: 'toggableWarning'
			}).prependTo(yasr.container).hide();
			$toggableWarningClose = $('<span>', {
					class: 'toggleWarning'
				})
				.html('&times;')
				.click(function() {
					$toggableWarning.hide(400);
				})
				.appendTo($toggableWarning);
			$toggableWarningMsg = $('<span>', {
				class: 'toggableMsg'
			}).appendTo($toggableWarning);
		}
		$toggableWarningMsg.empty();
		if (warning instanceof $) {
			$toggableWarningMsg.append(warning);
		} else {
			$toggableWarningMsg.html(warning);
		}
		$toggableWarning.show(400);
	};

	var blobDownloadSupported = null;
	var checkBlobDownloadSupported = function() {
		if (blobDownloadSupported === null) {
			var windowUrl = window.URL || window.webkitURL || window.mozURL || window.msURL;
			blobDownloadSupported = windowUrl && Blob;
		}
		return blobDownloadSupported;
	};
	var embedBtn = null;
	var drawHeader = function(yasr) {
		var drawOutputSelector = function() {
			var btnGroup = $('<div class="yasr_btnGroup"></div>');
			$.each(yasr.options.outputPlugins, function(i, pluginName) {
				console.log(pluginName);
				var plugin = yasr.plugins[pluginName];
				if (!plugin) return; //plugin not loaded

				if (plugin.hideFromSelection) return;
				var name = plugin.name || pluginName;
				var button = $("<button class='yasr_btn'></button>")
					.text(name)
					.addClass("select_" + pluginName)
					.click(function() {
						//update buttons
						btnGroup.find("button.selected").removeClass("selected");
						$(this).addClass("selected");
						//set and draw output
						yasr.options.output = pluginName;

						//store if needed
						yasr.store();

						//close warning if there is any
						if ($toggableWarning) $toggableWarning.hide(400);

						yasr.draw();
					})
					.appendTo(btnGroup);
				if (yasr.options.output == pluginName) button.addClass("selected");
			});

			if (btnGroup.children().length > 1) yasr.header.append(btnGroup);
		};
		var drawDownloadIcon = function() {
			var stringToUrl = function(string, contentType) {
				var url = null;
				var windowUrl = window.URL || window.webkitURL || window.mozURL || window.msURL;
				if (windowUrl && Blob) {
					var blob = new Blob([string], {
						type: contentType
					});
					url = windowUrl.createObjectURL(blob);
				}
				return url;
			};
			var button = $("<button class='yasr_btn yasr_downloadIcon btn_icon'></button>")
				.append(require("yasgui-utils").svg.getElement(require('./imgs.js').download))
				.click(function() {
					var currentPlugin = yasr.plugins[yasr.options.output];
					if (currentPlugin && currentPlugin.getDownloadInfo) {
						var downloadInfo = currentPlugin.getDownloadInfo();
						var downloadUrl = stringToUrl(downloadInfo.getContent(), (downloadInfo.contentType ? downloadInfo.contentType : "text/plain"));
						var downloadMockLink = $("<a></a>", {
							href: downloadUrl,
							download: downloadInfo.filename
						});
						require('./utils.js').fireClick(downloadMockLink);
						//						downloadMockLink[0].click();
					}
				});
			yasr.header.append(button);
		};
		var drawFullscreenButton = function() {
			var button = $("<button class='yasr_btn btn_fullscreen btn_icon'></button>")
				.append(require("yasgui-utils").svg.getElement(require('./imgs.js').fullscreen))
				.click(function() {
					yasr.container.addClass('yasr_fullscreen');
				});
			yasr.header.append(button);
		};
		var drawSmallscreenButton = function() {
			var button = $("<button class='yasr_btn btn_smallscreen btn_icon'></button>")
				.append(require("yasgui-utils").svg.getElement(require('./imgs.js').smallscreen))
				.click(function() {
					yasr.container.removeClass('yasr_fullscreen');
				});
			yasr.header.append(button);
		};
		var drawEmbedButton = function() {
			embedBtn = $("<button>", {
					class: 'yasr_btn yasr_embedBtn',
					title: 'Get HTML snippet to embed results on a web page'
				})
				.text('</>')
				.click(function(event) {
					var currentPlugin = yasr.plugins[yasr.options.output];
					if (currentPlugin && currentPlugin.getEmbedHtml) {
						var embedLink = currentPlugin.getEmbedHtml();

						event.stopPropagation();
						var popup = $("<div class='yasr_embedPopup'></div>").appendTo(yasr.header);
						$('html').click(function() {
							if (popup) popup.remove();
						});

						popup.click(function(event) {
							event.stopPropagation();
							//dont close when clicking on popup
						});
						var prePopup = $("<textarea>").val(embedLink);
						prePopup.focus(function() {
							var $this = $(this);
							$this.select();

							// Work around Chrome's little problem
							$this.mouseup(function() {
								// Prevent further mouseup intervention
								$this.unbind("mouseup");
								return false;
							});
						});

						popup.empty().append(prePopup);
						var positions = embedBtn.position();
						var top = (positions.top + embedBtn.outerHeight()) + 'px';
						var left = Math.max(((positions.left + embedBtn.outerWidth()) - popup.outerWidth()), 0) + 'px';

						popup.css("top", top).css("left", left);

					}
				})
			yasr.header.append(embedBtn);
		};
		drawFullscreenButton();
		drawSmallscreenButton();
		if (yasr.options.drawOutputSelector) drawOutputSelector();
		if (yasr.options.drawDownloadIcon && checkBlobDownloadSupported()) drawDownloadIcon(); //only draw when it's supported
		drawEmbedButton();
	};

	var persistentId = null;
	//store persistent options (not results though. store these separately, as they are too large)
	yasr.store = function() {
		if (!persistentId) persistentId = yasr.getPersistencyId('main');
		if (persistentId) {
			utils.storage.set(persistentId, yasr.getPersistentSettings());
		}
	};


	yasr.load = function() {
		if (!persistentId) persistentId = yasr.getPersistencyId('main');
		yasr.setPersistentSettings(utils.storage.get(persistentId));
	};


	yasr.setPersistentSettings = function(settings) {
		if (settings) {
			if (settings.output) {
				yasr.options.output = settings.output;
			}
			for (var pluginName in settings.plugins) {
				if (yasr.plugins[pluginName] && yasr.plugins[pluginName].setPersistentSettings) {
					yasr.plugins[pluginName].setPersistentSettings(settings.plugins[pluginName]);
				}
			}
		}
	}

	yasr.getPersistentSettings = function() {
		var settings = {
			output: yasr.options.output,
			plugins: {}
		};
		for (var pluginName in yasr.plugins) {
			if (yasr.plugins[pluginName].getPersistentSettings) {
				settings.plugins[pluginName] = yasr.plugins[pluginName].getPersistentSettings();
			}
		}
		return settings;
	}


	/**
	 * postprocess
	 */
	yasr.load();
	drawHeader(yasr);
	if (!queryResults && yasr.options.persistency && yasr.options.persistency.results) {
		var resultsId = yasr.getPersistencyId(yasr.options.persistency.results.key)
		var fromStorage;
		if (resultsId) {
			fromStorage = utils.storage.get(resultsId);
		}


		if (!fromStorage && yasr.options.persistency.results.id) {
			//deprecated! But keep for backwards compatability
			//if results are stored under old ID. Fetch the results, and delete that key (results can be large, and clutter space)
			//setting the results, will automatically store it under the new key, so we don't have to worry about that here
			var deprId = (typeof yasr.options.persistency.results.id == "string" ? yasr.options.persistency.results.id : yasr.options.persistency.results.id(yasr));
			if (deprId) {
				fromStorage = utils.storage.get(deprId);
				if (fromStorage) utils.storage.remove(deprId);
			}
		}
		if (fromStorage) {
			if ($.isArray(fromStorage)) {
				yasr.setResponse.apply(this, fromStorage);
			} else {
				yasr.setResponse(fromStorage);
			}
		}
	}

	if (queryResults) {
		yasr.setResponse(queryResults);
	}
	yasr.updateHeader();


	return yasr;
};

YASR.prototype = new EventEmitter;
module.exports = function(parent, options, queryResults) {
	return new YASR(parent, options, queryResults);
}


module.exports.plugins = {};
module.exports.registerOutput = function(name, constructor) {
	module.exports.plugins[name] = constructor;
};




/**
 * The default options of YASR. Either change the default options by setting YASR.defaults, or by
 * passing your own options as second argument to the YASR constructor
 *
 * @attribute YASR.defaults
 */
module.exports.defaults = require('./defaults.js');
module.exports.version = {
	"YASR": require("../package.json").version,
	"jquery": $.fn.jquery,
	"yasgui-utils": require("yasgui-utils").version
};
module.exports.$ = $;



//put these in a try-catch. When using the unbundled version, and when some dependencies are missing, then YASR as a whole will still function
try {
	module.exports.registerOutput('boolean', require("./boolean.js"))
} catch (e) {};
try {
	module.exports.registerOutput('rawResponse', require("./rawResponse.js"))
} catch (e) {};
try {
	module.exports.registerOutput('table', require("./table.js"))
} catch (e) {};
try {
	module.exports.registerOutput('error', require("./error.js"))
} catch (e) {};
try {
	module.exports.registerOutput('pivot', require("./pivot.js"))
} catch (e) {};
try {
	module.exports.registerOutput('gchart', require("./gchart.js"))
} catch (e) {};

},{"../package.json":19,"./boolean.js":21,"./defaults.js":22,"./error.js":23,"./gChartLoader.js":25,"./gchart.js":26,"./imgs.js":27,"./jquery/extendJquery.js":28,"./parsers/wrapper.js":35,"./pivot.js":37,"./rawResponse.js":38,"./table.js":39,"./utils.js":40,"events":4,"jquery":undefined,"yasgui-utils":16}],31:[function(require,module,exports){
'use strict';
var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})();
var root = module.exports = function(queryResponse) {
	return require("./dlv.js")(queryResponse, ",");
};
},{"./dlv.js":32,"jquery":undefined}],32:[function(require,module,exports){
'use strict';
var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})();
require("../../lib/jquery.csv-0.71.js");
var root = module.exports = function(queryResponse, separator) {
	var json = {};
	var arrays = $.csv.toArrays(queryResponse, {
		separator: separator
	});
	var detectType = function(value) {
		if (value.indexOf("http") == 0) {
			return "uri";
		} else {
			return null;
		}
	};

	var getBoolean = function() {
		if (arrays.length == 2 && arrays[0].length == 1 && arrays[1].length == 1 && arrays[0][0] == "boolean" && (arrays[1][0] == "1" || arrays[1][0] == "0")) {
			json.boolean = (arrays[1][0] == "1" ? true : false);
			return true;
		}
		return false;
	};

	var getVariables = function() {
		if (arrays.length > 0 && arrays[0].length > 0) {
			json.head = {
				vars: arrays[0]
			};
			return true;
		}
		return false;
	};

	var getBindings = function() {
		if (arrays.length > 1) {
			json.results = {
				bindings: []
			};
			for (var rowIt = 1; rowIt < arrays.length; rowIt++) {
				var binding = {};
				for (var colIt = 0; colIt < arrays[rowIt].length; colIt++) {
					var varName = json.head.vars[colIt];
					if (varName) {
						var value = arrays[rowIt][colIt];
						var detectedType = detectType(value);
						binding[varName] = {
							value: value
						};
						if (detectedType) binding[varName].type = detectedType;
					}
				}

				json.results.bindings.push(binding);
			}
			json.head = {
				vars: arrays[0]
			};
			return true;
		}
		return false;
	};
	var isBoolean = getBoolean();
	if (!isBoolean) {
		var varsFetched = getVariables();
		if (varsFetched) getBindings();
	}

	return json;
};
},{"../../lib/jquery.csv-0.71.js":3,"jquery":undefined}],33:[function(require,module,exports){
'use strict';
var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})();
var root = module.exports = function(queryResponse) {

	if (typeof queryResponse == "string") {
		try {
			return JSON.parse(queryResponse);
		} catch (e) {
			return false;
		}
	}
	if (typeof queryResponse == "object" && queryResponse.constructor === {}.constructor) {
		return queryResponse;
	}
	return false;

};
},{"jquery":undefined}],34:[function(require,module,exports){
'use strict';
var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})();
var root = module.exports = function(queryResponse) {
	return require("./dlv.js")(queryResponse, "\t");
};
},{"./dlv.js":32,"jquery":undefined}],35:[function(require,module,exports){
'use strict';
var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})();

/**
 * arg1 can be:
 * - a string (an exception, or e.g. a csv result string)
 * - an object (e.g. xml or json object)
 * - an object with exception key (contains solely a string to display)
 * - a jqXHR object (when this function is used as 'fail' callback for jquery)
 * arg2 can be:
 * - the textstatus of the response
 * arg3 can be:
 * - a jqXHR object (when this function is used as 'done/success' callback for jquery)
 * - an 'errorThrown' string (
 */
var root = module.exports = function(dataOrJqXhr, textStatus, jqXhrOrErrorString) {
	var parsers = {
		xml: require("./xml.js"),
		json: require("./json.js"),
		tsv: require("./tsv.js"),
		csv: require("./csv.js")
	};
	var contentType = null;
	var origResponse = null;
	var json = null;
	var type = null; //json, xml, csv, or tsv
	var exception = null;

	var init = function() {
		if (typeof dataOrJqXhr == "object") {
			/**
			 * Extract exception info (if there is any)
			 */
			if (dataOrJqXhr.exception) {
				//this object just has this exception string, nothing more. (here for backwards compatability)
				exception = dataOrJqXhr.exception;
			} else if (dataOrJqXhr.status != undefined && (dataOrJqXhr.status >= 300 || dataOrJqXhr.status === 0)) {
				//this is an exception, and jquery response
				exception = {
					status: dataOrJqXhr.status
				};
				if (typeof jqXhrOrErrorString == "string") exception.errorString = jqXhrOrErrorString;
				if (dataOrJqXhr.responseText) exception.responseText = dataOrJqXhr.responseText;
				if (dataOrJqXhr.statusText) exception.statusText = dataOrJqXhr.statusText;
			}

			/**
			 * Extract content type info (if there is any)
			 */
			if (dataOrJqXhr.contentType) {
				//this is not a jqXhr object, but a manually generated object (mostly for backwards compatability)
				contentType = dataOrJqXhr.contentType.toLowerCase();
			} else if (dataOrJqXhr.getResponseHeader && dataOrJqXhr.getResponseHeader("content-type")) {
				var ct = dataOrJqXhr.getResponseHeader("content-type").trim().toLowerCase();
				if (ct.length > 0) contentType = ct;
			}

			/**
			 * extract original response
			 */
			if (dataOrJqXhr.response) {
				//this is not a jqXhr object, but a manually generated object (mostly for backwards compatability)
				origResponse = dataOrJqXhr.response;
			} else if (!textStatus && !jqXhrOrErrorString) {
				//not called from jquery, as these other arguments are undefined.
				//so, we can only assume the current object is a proper response (e.g. xml or json) object
				origResponse = dataOrJqXhr;
			}
		}
		if (!exception && !origResponse) {
			//if this is called via a jquery complete callback, we should fetch the result for the jqXHR object
			if (dataOrJqXhr.responseText) {
				origResponse = dataOrJqXhr.responseText;
			} else {
				//if all else fails, assume first arg to be data object
				//(which should be the case for most situations)
				origResponse = dataOrJqXhr;
			}
		}
	};

	var getAsJson = function() {
		if (json) return json;
		if (json === false || exception) return false; //already tried parsing this, and failed. do not try again... 
		var getParserFromContentType = function() {
			if (contentType) {
				if (contentType.indexOf("json") > -1) {
					try {
						json = parsers.json(origResponse);
					} catch (e) {
						exception = e;
					}
					type = "json";
				} else if (contentType.indexOf("xml") > -1) {
					try {
						json = parsers.xml(origResponse);
					} catch (e) {
						exception = e;
					}
					type = "xml";
				} else if (contentType.indexOf("csv") > -1) {
					try {
						json = parsers.csv(origResponse);
					} catch (e) {
						exception = e;
					}
					type = "csv";
				} else if (contentType.indexOf("tab-separated") > -1) {
					try {
						json = parsers.tsv(origResponse);
					} catch (e) {
						exception = e;
					}
					type = "tsv";
				}
			}
		};


		var doLuckyGuess = function() {
			json = parsers.json(origResponse);
			if (json) {
				type = "json";
			} else {
				try {
					json = parsers.xml(origResponse);
					if (json) type = "xml";
				} catch (err) {};
			}
		};


		getParserFromContentType();
		if (!json) {
			doLuckyGuess();
		}
		if (!json) json = false; //explicitly set to false, so we don't try to parse this thing again..
		return json;
	};


	var getVariables = function() {
		var json = getAsJson();
		if (json && "head" in json) {
			return json.head.vars;
		} else {
			return null;
		}
	};

	var getBindings = function() {
		var json = getAsJson();
		if (json && "results" in json) {
			return json.results.bindings;
		} else {
			return null;
		}
	};

	var getBoolean = function() {
		var json = getAsJson();
		if (json && "boolean" in json) {
			return json.boolean;
		} else {
			return null;
		}
	};
	var getOriginalResponse = function() {
		return origResponse;
	};
	var getOriginalResponseAsString = function() {
		var responseString = "";
		if (typeof origResponse == "string") {
			responseString = origResponse;
		} else if (type == "json") {
			responseString = JSON.stringify(origResponse, undefined, 2); //prettifies as well
		} else if (type == "xml") {
			responseString = new XMLSerializer().serializeToString(origResponse);
		}
		return responseString;
	};
	var getException = function() {
		return exception;
	};
	var getType = function() {
		if (type == null) getAsJson(); //detects type as well
		return type;
	};

	//process the input parameters in such a way that we can store it in local storage (i.e., no function)
	//and, make sure we can easily pass it on back to this wrapper function when loading it again from storage
	var getAsStoreObject = function() {
		var storeArray = [];
		var arg1 = {};
		if (dataOrJqXhr.status) {
			//jqXhr object
			arg1.status = dataOrJqXhr.status;
			arg1.responseText = dataOrJqXhr.responseText;
			arg1.statusText = dataOrJqXhr.statusText;
			arg1.contentType = contentType; //this is a function in a jqXhr object (problem for storing). but this wrapper will read it as string as well
		} else {
			//the other instances of this param (whether it is a json, xml, or exception object), we can normally store
			arg1 = dataOrJqXhr;
		}


		var arg2 = textStatus;
		var arg3 = undefined;
		if (typeof jqXhrOrErrorString == "string") arg3 = jqXhrOrErrorString;

		return [arg1, arg2, arg3];
	};



	init();
	json = getAsJson();

	return {
		getAsStoreObject: getAsStoreObject,
		getAsJson: getAsJson,
		getOriginalResponse: getOriginalResponse,
		getOriginalResponseAsString: getOriginalResponseAsString,
		getOriginalContentType: function() {
			return contentType;
		},
		getVariables: getVariables,
		getBindings: getBindings,
		getBoolean: getBoolean,
		getType: getType,
		getException: getException
	};
};
},{"./csv.js":31,"./json.js":33,"./tsv.js":34,"./xml.js":36,"jquery":undefined}],36:[function(require,module,exports){
'use strict';
var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})();
var root = module.exports = function(xml) {



	/**
	 * head
	 */
	var parseHead = function(node) {
		json.head = {};
		for (var headNodeIt = 0; headNodeIt < node.childNodes.length; headNodeIt++) {
			var headNode = node.childNodes[headNodeIt];
			if (headNode.nodeName == "variable") {
				if (!json.head.vars) json.head.vars = [];
				var name = headNode.getAttribute("name");
				if (name) json.head.vars.push(name);
			}
		}
	};

	var parseResults = function(node) {
		json.results = {};
		json.results.bindings = [];
		for (var resultIt = 0; resultIt < node.childNodes.length; resultIt++) {
			var resultNode = node.childNodes[resultIt];
			var jsonResult = null;

			for (var bindingIt = 0; bindingIt < resultNode.childNodes.length; bindingIt++) {
				var bindingNode = resultNode.childNodes[bindingIt];
				if (bindingNode.nodeName == "binding") {
					var varName = bindingNode.getAttribute("name");
					if (varName) {
						jsonResult = jsonResult || {};
						jsonResult[varName] = {};
						for (var bindingInfIt = 0; bindingInfIt < bindingNode.childNodes.length; bindingInfIt++) {
							var bindingInf = bindingNode.childNodes[bindingInfIt];
							var type = bindingInf.nodeName;
							if (type == "#text") continue;
							jsonResult[varName].type = type;
							jsonResult[varName].value = bindingInf.innerHTML;
							var dataType = bindingInf.getAttribute("datatype");
							if (dataType) jsonResult[varName].datatype = dataType;

						}
					}
				}
			}
			if (jsonResult) json.results.bindings.push(jsonResult);
		}
	};

	var parseBoolean = function(node) {
		if (node.innerHTML == "true") {
			json.boolean = true;
		} else {
			json.boolean = false;
		}
	};
	var mainXml = null;
	if (typeof xml == "string") {
		mainXml = $.parseXML(xml);
	} else if ($.isXMLDoc(xml)) {
		mainXml = xml;
	}
	var xml = null;
	if (mainXml.childNodes.length > 0) {
		//enter the main 'sparql' node
		xml = mainXml.childNodes[0];
	} else {
		return null;
	}
	var json = {};


	for (var i = 0; i < xml.childNodes.length; i++) {
		var node = xml.childNodes[i];
		if (node.nodeName == "head") parseHead(node);
		if (node.nodeName == "results") parseResults(node);
		if (node.nodeName == "boolean") parseBoolean(node);
	}

	return json;
};
},{"jquery":undefined}],37:[function(require,module,exports){
'use strict';
var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})(),
	utils = require('./utils.js'),
	yUtils = require('yasgui-utils'),
	imgs = require('./imgs.js');
(function(){try{return require('jquery-ui/sortable')}catch(e){return window.jQuery}})();
(function(){try{return require('pivottable')}catch(e){return window.jQuery}})();

if (!$.fn.pivotUI) throw new Error("Pivot lib not loaded");
var root = module.exports = function(yasr) {
	var plugin = {};
	var options = $.extend(true, {}, root.defaults);

	if (options.useD3Chart) {
		try {
			var d3 = (function(){try{return require('d3')}catch(e){return window.d3}})();
			if (d3) require('../node_modules/pivottable/dist/d3_renderers.js');
		} catch (e) {
			//do nothing. just make sure we don't use this renderer
		}
		if ($.pivotUtilities.d3_renderers) $.extend(true, $.pivotUtilities.renderers, $.pivotUtilities.d3_renderers);
	}



	var $pivotWrapper;
	var mergeLabelPostfix = null;
	var getShownVariables = function() {
		var variables = yasr.results.getVariables();
		if (!options.mergeLabelsWithUris) return variables;
		var shownVariables = [];

		mergeLabelPostfix = (typeof options.mergeLabelsWithUris == "string" ? options.mergeLabelsWithUris : "Label");
		variables.forEach(function(variable) {
			if (variable.indexOf(mergeLabelPostfix, variable.length - mergeLabelPostfix.length) !== -1) {
				//this one ends with a postfix
				if (variables.indexOf(variable.substring(0, variable.length - mergeLabelPostfix.length)) >= 0) {
					//we have a shorter version of this variable. So, do not include the ..<postfix> variable in the table
					return;
				}
			}
			shownVariables.push(variable);
		});
		return shownVariables;
	};

	var formatForPivot = function(callback) {

		var vars = getShownVariables();
		var usedPrefixes = null;
		if (yasr.options.getUsedPrefixes) {
			usedPrefixes = (typeof yasr.options.getUsedPrefixes == "function" ? yasr.options.getUsedPrefixes(yasr) : yasr.options.getUsedPrefixes);
		}
		yasr.results.getBindings().forEach(function(binding) {
			var rowObj = {};
			vars.forEach(function(variable) {
				if (variable in binding) {
					var val = binding[variable].value;
					if (mergeLabelPostfix && binding[variable + mergeLabelPostfix]) {
						val = binding[variable + mergeLabelPostfix].value;
					} else if (binding[variable].type == "uri") {
						val = utils.uriToPrefixed(usedPrefixes, val);
					}
					rowObj[variable] = val;
				} else {
					rowObj[variable] = null;
				}
			});
			callback(rowObj);
		});
	}


	var validatePivotTableOptions = function(pivotOptions) {
		//validate settings. we may have different variables, or renderers might be gone
		if (pivotOptions) {
			if (yasr.results) {
				var vars = yasr.results.getVariables();
				var keepColsAndRows = true;
				pivotOptions.cols.forEach(function(variable) {
					if (vars.indexOf(variable) < 0) keepColsAndRows = false;
				});
				if (keepColsAndRows) {
					pivotOptionse.rows.forEach(function(variable) {
						if (vars.indexOf(variable) < 0) keepColsAndRows = false;
					});
				}
				if (!keepColsAndRows) {
					pivotOptions.cols = [];
					pivotOptions.rows = [];
				}
				if (!$.pivotUtilities.renderers[settings.rendererName]) delete pivotOptions.rendererName;
			}
		} else {
			pivotOptions = {};
		}
		return pivotOptions;
	};
	var draw = function() {
		var doDraw = function() {
			var onRefresh = function(pivotObj) {
				options.pivotTable.cols = pivotObj.cols;
				options.pivotTable.rows = pivotObj.rows;
				options.pivotTable.rendererName = pivotObj.rendererName;
				options.pivotTable.aggregatorName = pivotObj.aggregatorName;
				options.pivotTable.vals = pivotObj.vals;
				yasr.store();

				if (pivotObj.rendererName.toLowerCase().indexOf(' chart') >= 0) {
					openGchartBtn.show();
				} else {
					openGchartBtn.hide();
				}
				yasr.updateHeader();
			};


			var openGchartBtn = $('<button>', {
					class: 'openPivotGchart yasr_btn'
				})
				.text('Chart Config')
				.click(function() {
					$pivotWrapper.find('div[dir="ltr"]').dblclick();
				}).appendTo(yasr.resultsContainer);
			$pivotWrapper = $('<div>', {
				class: 'pivotTable'
			}).appendTo($(yasr.resultsContainer));

			options.pivotTable.onRefresh = (function() {
				var originalRefresh = options.pivotTable.onRefresh;
				return function(pivotObj) {
					onRefresh(pivotObj);
					if (originalRefresh) originalRefresh(pivotObj);
				};
			})();

			window.pivot = $pivotWrapper.pivotUI(formatForPivot, options.pivotTable);

			/**
			 * post process
			 */
			//use 'move' handler for variables. This removes the 'filter' button though. Might want to re-enable this in the future
			var icon = $(yUtils.svg.getElement(imgs.move));
			$pivotWrapper.find('.pvtTriangle').replaceWith(icon);

			//add headers to selector rows
			$('.pvtCols').prepend($('<div>', {
				class: 'containerHeader'
			}).text("Columns"));
			$('.pvtRows').prepend($('<div>', {
				class: 'containerHeader'
			}).text("Rows"));
			$('.pvtUnused').prepend($('<div>', {
				class: 'containerHeader'
			}).text("Available Variables"));
			$('.pvtVals').prepend($('<div>', {
				class: 'containerHeader'
			}).text("Cells"));

			//hmmm, directly after the callback finishes (i.e., directly after this line), the svg is draw.
			//just use a short timeout to update the header
			setTimeout(yasr.updateHeader, 400);
		}

		if (yasr.options.useGoogleCharts && options.useGoogleCharts && !$.pivotUtilities.gchart_renderers) {
			require('./gChartLoader.js')
				.on('done', function() {
					try {
						require('../node_modules/pivottable/dist/gchart_renderers.js');
						$.extend(true, $.pivotUtilities.renderers, $.pivotUtilities.gchart_renderers);
					} catch (e) {
						//hmm, still something went wrong. forget about it;
						options.useGoogleCharts = false;
					}
					doDraw();
				})
				.on('error', function() {
					console.log('could not load gchart');
					options.useGoogleCharts = false;
					doDraw();
				})
				.googleLoad();
		} else {
			//everything is already loaded. just draw
			doDraw();
		}
	};
	var canHandleResults = function() {
		return yasr.results && yasr.results.getVariables && yasr.results.getVariables() && yasr.results.getVariables().length > 0;
	};

	var getDownloadInfo = function() {
		if (!yasr.results) return null;
		var svgEl = yasr.resultsContainer.find('.pvtRendererArea svg');
		if (svgEl.length > 0) {

			return {
				getContent: function() {
					if (svgEl[0].outerHTML) {
						return svgEl[0].outerHTML;
					} else {
						//outerHTML not supported. use workaround
						return $('<div>').append(svgEl.clone()).html();
					}
				},

				filename: "queryResults.svg",
				contentType: "image/svg+xml",
				buttonTitle: "Download SVG Image"
			};
		}

		//ok, not a svg. is it a table?
		var $table = yasr.resultsContainer.find('.pvtRendererArea table');
		if ($table.length > 0) {
			return {
				getContent: function() {
					return $table.tableToCsv();
				},
				filename: "queryResults.csv",
				contentType: "text/csv",
				buttonTitle: "Download as CSV"
			};
		}

	};
	var getEmbedHtml = function() {
		if (!yasr.results) return null;

		var svgEl = yasr.resultsContainer.find('.pvtRendererArea svg')
			.clone() //create clone, as we'd like to remove height/width attributes
			.removeAttr('height').removeAttr('width')
			.css('height', '').css('width', '');
		if (svgEl.length == 0) return null;

		var htmlString = svgEl[0].outerHTML;
		if (!htmlString) {
			//outerHTML not supported. use workaround
			htmlString = $('<div>').append(svgEl.clone()).html();
		}
		//wrap in div, so users can more easily tune width/height
		//don't use jquery, so we can easily influence indentation
		return '<div style="width: 800px; height: 600px;">\n' + htmlString + '\n</div>';
	};
	return {
		getPersistentSettings: function() {
			return {
				pivotTable: options.pivotTable
			};
		},
		setPersistentSettings: function(newSettings) {
			if (newSettings.pivotTable) {
				options.pivotTable = validatePivotTableOptions(newSettings.pivotTable);
			}

		},
		getDownloadInfo: getDownloadInfo,
		getEmbedHtml: getEmbedHtml,
		options: options,
		draw: draw,
		name: "Pivot Table",
		canHandleResults: canHandleResults,
		getPriority: 4,
	}
};



root.defaults = {
	mergeLabelsWithUris: false,
	useGoogleCharts: true,
	useD3Chart: true,
	persistencyId: 'pivot',
	pivotTable: {}
};

root.version = {
	"YASR-rawResponse": require("../package.json").version,
	"jquery": $.fn.jquery,
};
},{"../node_modules/pivottable/dist/d3_renderers.js":12,"../node_modules/pivottable/dist/gchart_renderers.js":13,"../package.json":19,"./gChartLoader.js":25,"./imgs.js":27,"./utils.js":40,"d3":undefined,"jquery":undefined,"jquery-ui/sortable":undefined,"pivottable":undefined,"yasgui-utils":16}],38:[function(require,module,exports){
'use strict';
var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})(),
	CodeMirror = (function(){try{return require('codemirror')}catch(e){return window.CodeMirror}})();

require('codemirror/addon/fold/foldcode.js');
require('codemirror/addon/fold/foldgutter.js');
require('codemirror/addon/fold/xml-fold.js');
require('codemirror/addon/fold/brace-fold.js');

require('codemirror/addon/edit/matchbrackets.js');
require('codemirror/mode/xml/xml.js');
require('codemirror/mode/javascript/javascript.js');

var root = module.exports = function(yasr) {
	var plugin = {};
	var options = $.extend(true, {}, root.defaults);
	var cm = null;
	var draw = function() {
		var cmOptions = options.CodeMirror;
		cmOptions.value = yasr.results.getOriginalResponseAsString();

		var mode = yasr.results.getType();
		if (mode) {
			if (mode == "json") {
				mode = {
					name: "javascript",
					json: true
				};
			}
			cmOptions.mode = mode;
		}

		cm = CodeMirror(yasr.resultsContainer.get()[0], cmOptions);

		//CM has some issues with folding and unfolding (blank parts in the codemirror area, which are only filled after clicking it)
		//so, refresh cm after folding/unfolding
		cm.on('fold', function() {
			cm.refresh();
		});
		cm.on('unfold', function() {
			cm.refresh();
		});

	};
	var canHandleResults = function() {
		if (!yasr.results) return false;
		if (!yasr.results.getOriginalResponseAsString) return false;
		var response = yasr.results.getOriginalResponseAsString();
		if ((!response || response.length == 0) && yasr.results.getException()) return false; //in this case, show exception instead, as we have nothing to show anyway
		return true;
	};

	var getDownloadInfo = function() {
		if (!yasr.results) return null;
		var contentType = yasr.results.getOriginalContentType();
		var type = yasr.results.getType();
		return {
			getContent: function() {
				return yasr.results.getOriginalResponse();
			},
			filename: "queryResults" + (type ? "." + type : ""),
			contentType: (contentType ? contentType : "text/plain"),
			buttonTitle: "Download raw response"
		};
	};

	return {
		draw: draw,
		name: "Raw Response",
		canHandleResults: canHandleResults,
		getPriority: 2,
		getDownloadInfo: getDownloadInfo,

	}
};



root.defaults = {
	CodeMirror: {
		readOnly: true,
		lineNumbers: true,
		lineWrapping: true,
		foldGutter: true,
		gutters: ["CodeMirror-linenumbers", "CodeMirror-foldgutter"]
	}
};

root.version = {
	"YASR-rawResponse": require("../package.json").version,
	"jquery": $.fn.jquery,
	"CodeMirror": CodeMirror.version
};
},{"../package.json":19,"codemirror":undefined,"codemirror/addon/edit/matchbrackets.js":5,"codemirror/addon/fold/brace-fold.js":6,"codemirror/addon/fold/foldcode.js":7,"codemirror/addon/fold/foldgutter.js":8,"codemirror/addon/fold/xml-fold.js":9,"codemirror/mode/javascript/javascript.js":10,"codemirror/mode/xml/xml.js":11,"jquery":undefined}],39:[function(require,module,exports){
'use strict';
var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})(),
	yutils = require("yasgui-utils"),
	utils = require('./utils.js'),
	imgs = require('./imgs.js');
(function(){try{return require('datatables')}catch(e){return window.jQuery}})();
require("../lib/colResizable-1.4.js");



/**
 * Constructor of plugin which displays results as a table
 * 
 * @param yasr {object}
 * @param parent {DOM element}
 * @param options {object}
 * @class YASR.plugins.table
 * @return yasr-table (doc)
 * 
 */
var root = module.exports = function(yasr) {
	var table = null;
	var plugin = {
		name: "Table",
		getPriority: 10,
	};
	var options = plugin.options = $.extend(true, {}, root.defaults);
	var tableLengthPersistencyId = (options.persistency ? yasr.getPersistencyId(options.persistency.tableLength) : null);

	var getRows = function() {
		var rows = [];
		var bindings = yasr.results.getBindings();
		var vars = yasr.results.getVariables();
		var usedPrefixes = null;
		if (yasr.options.getUsedPrefixes) {
			usedPrefixes = (typeof yasr.options.getUsedPrefixes == "function" ? yasr.options.getUsedPrefixes(yasr) : yasr.options.getUsedPrefixes);
		}
		for (var rowId = 0; rowId < bindings.length; rowId++) {
			var row = [];
			row.push(""); //row numbers
			var binding = bindings[rowId];
			for (var colId = 0; colId < vars.length; colId++) {
				var sparqlVar = vars[colId];
				if (sparqlVar in binding) {
					if (options.getCellContent) {
						row.push(options.getCellContent(yasr, plugin, binding, sparqlVar, {
							'rowId': rowId,
							'colId': colId,
							'usedPrefixes': usedPrefixes
						}));
					} else {
						row.push("");
					}
				} else {
					row.push("");
				}
			}
			rows.push(row);
		}
		return rows;
	};

	var eventId = yasr.getPersistencyId('eventId') || "yasr_" + $(yasr.container).closest('[id]').attr('id');
	var addEvents = function() {
		table.on('order.dt', function() {
			drawSvgIcons();
		});
		if (tableLengthPersistencyId) {
			table.on('length.dt', function(e, settings, len) {
				yutils.storage.set(tableLengthPersistencyId, len, "month");
			});
		}
		$.extend(true, options.callbacks, options.handlers);
		table.delegate("td", "click", function(event) {
			if (options.callbacks && options.callbacks.onCellClick) {
				var result = options.callbacks.onCellClick(this, event);
				if (result === false) return false;
			}
		}).delegate("td", 'mouseenter', function(event) {
			if (options.callbacks && options.callbacks.onCellMouseEnter) {
				options.callbacks.onCellMouseEnter(this, event);
			}
			var tdEl = $(this);
			if (options.fetchTitlesFromPreflabel && tdEl.attr("title") === undefined && tdEl.text().trim().indexOf("http") == 0) {
				addPrefLabel(tdEl);
			}
		}).delegate("td", 'mouseleave', function(event) {
			if (options.callbacks && options.callbacks.onCellMouseLeave) {
				options.callbacks.onCellMouseLeave(this, event);

			}
		});
	};

	plugin.draw = function() {
		table = $('<table cellpadding="0" cellspacing="0" border="0" class="resultsTable"></table>');
		$(yasr.resultsContainer).html(table);

		var dataTableConfig = options.datatable;
		dataTableConfig.data = getRows();
		dataTableConfig.columns = options.getColumns(yasr, plugin);

		//fetch stored datatables length value
		var pLength = yutils.storage.get(tableLengthPersistencyId);
		if (pLength) dataTableConfig.pageLength = pLength;



		table.DataTable($.extend(true, {}, dataTableConfig)); //make copy. datatables adds properties for backwards compatability reasons, and don't want this cluttering our own 


		drawSvgIcons();

		addEvents();

		//finally, make the columns dragable:
		table.colResizable();
	};

	var drawSvgIcons = function() {
		var sortings = {
			"sorting": "unsorted",
			"sorting_asc": "sortAsc",
			"sorting_desc": "sortDesc"
		};
		table.find(".sortIcons").remove();
		for (var sorting in sortings) {
			var svgDiv = $("<div class='sortIcons'></div>");
			yutils.svg.draw(svgDiv, imgs[sortings[sorting]]);
			table.find("th." + sorting).append(svgDiv);
		}
	};
	/**
	 * Check whether this plugin can handler the current results
	 * 
	 * @property canHandleResults
	 * @type function
	 * @default If resultset contains variables in the resultset, return true
	 */
	plugin.canHandleResults = function() {
		return yasr.results && yasr.results.getVariables && yasr.results.getVariables() && yasr.results.getVariables().length > 0;
	};


	plugin.getDownloadInfo = function() {
		if (!yasr.results) return null;
		return {
			getContent: function() {
				return require("./bindingsToCsv.js")(yasr.results.getAsJson());
			},
			filename: "queryResults.csv",
			contentType: "text/csv",
			buttonTitle: "Download as CSV"
		};
	};


	return plugin;
};


var formatLiteral = function(yasr, plugin, literalBinding) {
	var stringRepresentation = utils.escapeHtmlEntities(literalBinding.value);
	if (literalBinding["xml:lang"]) {
		stringRepresentation = '"' + stringRepresentation + '"<sup>@' + literalBinding["xml:lang"] + '</sup>';
	} else if (literalBinding.datatype) {
		var xmlSchemaNs = "http://www.w3.org/2001/XMLSchema#";
		var dataType = literalBinding.datatype;
		if (dataType.indexOf(xmlSchemaNs) === 0) {
			dataType = "xsd:" + dataType.substring(xmlSchemaNs.length);
		} else {
			dataType = "&lt;" + dataType + "&gt;";
		}

		stringRepresentation = '"' + stringRepresentation + '"<sup>^^' + dataType + '</sup>';
	}
	return stringRepresentation;
};
var getCellContent = function(yasr, plugin, bindings, sparqlVar, context) {
	var binding = bindings[sparqlVar];
	var value = null;
	if (binding.type == "uri") {
		var title = null;
		var href = binding.value;
		var visibleString = href;
		if (context.usedPrefixes) {
			for (var prefix in context.usedPrefixes) {
				if (visibleString.indexOf(context.usedPrefixes[prefix]) == 0) {
					visibleString = prefix + ':' + href.substring(context.usedPrefixes[prefix].length);
					break;
				}
			}
		}
		if (plugin.options.mergeLabelsWithUris) {
			var postFix = (typeof plugin.options.mergeLabelsWithUris == "string" ? plugin.options.mergeLabelsWithUris : "Label");
			if (bindings[sparqlVar + postFix]) {
				visibleString = formatLiteral(yasr, plugin, bindings[sparqlVar + postFix]);
				title = href;
			}
		}
		value = "<a " + (title ? "title='" + href + "' " : "") + "class='uri' target='_blank' href='" + href + "'>" + visibleString + "</a>";
	} else {
		value = "<span class='nonUri'>" + formatLiteral(yasr, plugin, binding) + "</span>";
	}
	return "<div>" + value + "</div>";
};






var addPrefLabel = function(td) {
	var addEmptyTitle = function() {
		td.attr("title", ""); //this avoids trying to fetch the label again on next hover
	};
	$.get("http://preflabel.org/api/v1/label/" + encodeURIComponent(td.text()) + "?silent=true")
		.success(function(data) {
			if (typeof data == "object" && data.label) {
				td.attr("title", data.label);
			} else if (typeof data == "string" && data.length > 0) {
				td.attr("title", data);
			} else {
				addEmptyTitle();
			}

		})
		.fail(addEmptyTitle);
};

var openCellUriInNewWindow = function(cell) {
	if (cell.className.indexOf("uri") >= 0) {
		window.open(this.innerHTML);
	}
};

/**
 * Defaults for table plugin
 * 
 * @type object
 * @attribute YASR.plugins.table.defaults
 */
root.defaults = {

	/**
	 * Draw the cell content, from a given binding
	 * 
	 * @property drawCellContent
	 * @param binding {object}
	 * @type function
	 * @return string
	 * @default YASR.plugins.table.getFormattedValueFromBinding
	 */
	getCellContent: getCellContent,

	persistency: {
		tableLength: "tableLength",
	},

	getColumns: function(yasr, plugin) {
		var includeVariable = function(variableToCheck) {
			if (!plugin.options.mergeLabelsWithUris) return true;
			var postFix = (typeof plugin.options.mergeLabelsWithUris == "string" ? plugin.options.mergeLabelsWithUris : "Label");
			if (variableToCheck.indexOf(postFix, variableToCheck.length - postFix.length) !== -1) {
				//this one ends with a postfix
				if (yasr.results.getVariables().indexOf(variableToCheck.substring(0, variableToCheck.length - postFix.length)) >= 0) {
					//we have a shorter version of this variable. So, do not include the ..<postfix> variable in the table
					return false;
				}
			}
			return true;
		};

		var cols = [];
		cols.push({
			"title": ""
		}); //row numbers column
		yasr.results.getVariables().forEach(function(variable) {
			cols.push({
				"title": "<span>" + variable + "</span>",
				"visible": includeVariable(variable)
			});
		});
		return cols;
	},
	/**
	 * Try to fetch the label representation for each URI, using the preflabel.org services. (fetching occurs when hovering over the cell)
	 * 
	 * @property fetchTitlesFromPreflabel
	 * @type boolean
	 * @default true
	 */
	fetchTitlesFromPreflabel: true,

	mergeLabelsWithUris: false,
	/**
	 * Set a number of handlers for the table
	 * 
	 * @property handlers
	 * @type object
	 */
	callbacks: {
		/**
		 * Mouse-enter-cell event
		 * 
		 * @property handlers.onCellMouseEnter
		 * @type function
		 * @param td-element
		 * @default null
		 */
		onCellMouseEnter: null,
		/**
		 * Mouse-leave-cell event
		 * 
		 * @property handlers.onCellMouseLeave
		 * @type function
		 * @param td-element
		 * @default null
		 */
		onCellMouseLeave: null,
		/**
		 * Cell clicked event
		 * 
		 * @property handlers.onCellClick
		 * @type function
		 * @param td-element
		 * @default null
		 */
		onCellClick: null
	},
	/**
	 * This plugin uses the datatables jquery plugin (See datatables.net). For any datatables specific defaults, change this object. 
	 * See the datatables reference for more information
	 * 
	 * @property datatable
	 * @type object
	 */
	datatable: {
		"autoWidth": false,
		"dom": '<"dtTopHeader"ilf>rtip',
		"order": [], //disable initial sorting
		"pageLength": 50, //default page length
		"lengthMenu": [
			[10, 50, 100, 1000, -1],
			[10, 50, 100, 1000, "All"]
		], //possible page lengths
		"lengthChange": true, //allow changing page length
		"pagingType": "full_numbers", //how to show the pagination options
		"drawCallback": function(oSettings) {
			//trick to show row numbers
			for (var i = 0; i < oSettings.aiDisplay.length; i++) {
				$('td:eq(0)', oSettings.aoData[oSettings.aiDisplay[i]].nTr).html(i + 1);
			}

			//Hide pagination when we have a single page
			var activePaginateButton = false;
			$(oSettings.nTableWrapper).find(".paginate_button").each(function() {
				if ($(this).attr("class").indexOf("current") == -1 && $(this).attr("class").indexOf("disabled") == -1) {
					activePaginateButton = true;
				}
			});
			if (activePaginateButton) {
				$(oSettings.nTableWrapper).find(".dataTables_paginate").show();
			} else {
				$(oSettings.nTableWrapper).find(".dataTables_paginate").hide();
			}
		},
		"columnDefs": [{
				"width": "32px",
				"orderable": false,
				"targets": 0
			} //disable row sorting for first col
		],
	},
};

root.version = {
	"YASR-table": require("../package.json").version,
	"jquery": $.fn.jquery,
	"jquery-datatables": $.fn.DataTable.version
};
},{"../lib/colResizable-1.4.js":2,"../package.json":19,"./bindingsToCsv.js":20,"./imgs.js":27,"./utils.js":40,"datatables":undefined,"jquery":undefined,"yasgui-utils":16}],40:[function(require,module,exports){
'use strict';
var $ = (function(){try{return require('jquery')}catch(e){return window.jQuery}})(),
	GoogleTypeException = require('./exceptions.js').GoogleTypeException;

module.exports = {
	escapeHtmlEntities: function(unescaped) {
		//taken from http://stackoverflow.com/questions/5499078/fastest-method-to-escape-html-tags-as-html-entities
		return unescaped.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
	},
	uriToPrefixed: function(prefixes, uri) {
		if (prefixes) {
			for (var prefix in prefixes) {
				if (uri.indexOf(prefixes[prefix]) == 0) {
					uri = prefix + ':' + uri.substring(prefixes[prefix].length);
					break;
				}
			}
		}
		return uri;
	},
	getGoogleTypeForBinding: function(binding) {
		if (binding == null) return null;
		if (binding.type != null && (binding.type === 'typed-literal' || binding.type === 'literal')) {
			switch (binding.datatype) {
				case 'http://www.w3.org/2001/XMLSchema#float':
				case 'http://www.w3.org/2001/XMLSchema#decimal':
				case 'http://www.w3.org/2001/XMLSchema#int':
				case 'http://www.w3.org/2001/XMLSchema#integer':
				case 'http://www.w3.org/2001/XMLSchema#long':
				case 'http://www.w3.org/2001/XMLSchema#gYearMonth':
				case 'http://www.w3.org/2001/XMLSchema#gYear':
				case 'http://www.w3.org/2001/XMLSchema#gMonthDay':
				case 'http://www.w3.org/2001/XMLSchema#gDay':
				case 'http://www.w3.org/2001/XMLSchema#gMonth':
					return "number";
				case 'http://www.w3.org/2001/XMLSchema#date':
					return "date";
				case 'http://www.w3.org/2001/XMLSchema#dateTime':
					return "datetime";
				case 'http://www.w3.org/2001/XMLSchema#time':
					return "timeofday";
				default:
					return "string";
			}
		} else {
			return "string";
		}
	},
	getGoogleTypeForBindings: function(bindings, varName) {
		var types = {};
		var typeCount = 0;
		bindings.forEach(function(binding) {
			var type = module.exports.getGoogleTypeForBinding(binding[varName]);
			if (type != null) {
				if (!(type in types)) {
					types[type] = 0;
					typeCount++;
				}
				types[type]++;
			}
		});
		if (typeCount == 0) {
			return 'string';
		} else if (typeCount == 1) {
			for (var type in types) {
				return type; //just return this one
			}
		} else {
			//we have conflicting types. Throw error
			throw new GoogleTypeException(types, varName);
		}
	},

	castGoogleType: function(binding, prefixes, googleType) {
		if (binding == null) {
			return null;
		}

		if (googleType != 'string' && binding.type != null && (binding.type === 'typed-literal' || binding.type === 'literal')) {
			switch (binding.datatype) {
				case 'http://www.w3.org/2001/XMLSchema#float':
				case 'http://www.w3.org/2001/XMLSchema#decimal':
				case 'http://www.w3.org/2001/XMLSchema#int':
				case 'http://www.w3.org/2001/XMLSchema#integer':
				case 'http://www.w3.org/2001/XMLSchema#long':
				case 'http://www.w3.org/2001/XMLSchema#gYearMonth':
				case 'http://www.w3.org/2001/XMLSchema#gYear':
				case 'http://www.w3.org/2001/XMLSchema#gMonthDay':
				case 'http://www.w3.org/2001/XMLSchema#gDay':
				case 'http://www.w3.org/2001/XMLSchema#gMonth':
					return Number(binding.value);
				case 'http://www.w3.org/2001/XMLSchema#date':
					//grrr, the date function does not parse -any- date (including most xsd dates!)
					//datetime and time seem to be fine though.
					//so, first try our custom parser. if that does not work, try the regular date parser anyway
					var date = parseXmlSchemaDate(binding.value);
					if (date) return date;
				case 'http://www.w3.org/2001/XMLSchema#dateTime':
				case 'http://www.w3.org/2001/XMLSchema#time':
					return new Date(binding.value);
				default:
					return binding.value;
			}
		} else {
			if (binding.type = 'uri') {
				return module.exports.uriToPrefixed(prefixes, binding.value);
			} else {
				return binding.value;
			}
		}
	},
	fireClick: function($els) {
		if (!$els)
			return;
		$els.each(function(i, el) {
			var $el = $(el);
			if (document.dispatchEvent) { // W3C
				var oEvent = document.createEvent("MouseEvents");
				oEvent.initMouseEvent("click", true, true, window, 1, 1, 1, 1, 1,
					false, false, false, false, 0, $el[0]);
				$el[0].dispatchEvent(oEvent);
			} else if (document.fireEvent) { // IE
				$el[0].click();
			}
		});
	}
};
//There are no PROPER xml schema to js date parsers
//A few libraries exist: moment, jsdate, Xdate, but none of them parse valid xml schema dates (e.g. 1999-11-05+02:00).
//And: I'm not going to write one myself
//There are other hacky solutions (regular expressions based on trial/error) such as http://stackoverflow.com/questions/2731579/convert-an-xml-schema-date-string-to-a-javascript-date
//But if we're doing hacky stuff, I at least want to do it MYSELF!
var parseXmlSchemaDate = function(dateString) {
	//change +02:00 to Z+02:00 (something which is parseable by js date)
	var date = new Date(dateString.replace(/(\d)([\+-]\d{2}:\d{2})/, '$1Z$2'));
	if (isNaN(date)) return null;
	return date;
};
},{"./exceptions.js":24,"jquery":undefined}]},{},[1])(1)
});


//# sourceMappingURL=yasr.js.map