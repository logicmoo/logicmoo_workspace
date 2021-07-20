function char_size() {
    var span = $('<span>&nbsp;' +
                 '</span>').appendTo($('.terminal-output'));
    var result = {
        width: span.width(),
        height: span.outerHeight()
    };
    span.remove();
    return result;
}

function nSpaces(n){
    //+ 1 because join inserts between
    return Array(n + 1).join(" ");
}

function encodeSpaces(string){
    return string.replace(/ /g, "&nbsp;");
}

function htmlEncode(value){
    return encodeSpaces($('<div />').text(value).html());
}

/*
In emacs:
Frame = entire emacs window
window = a portion of the window rendering a certain buffer

Naive way to render the visible parts of a buffer
Doesn't do most of the rendering done by emacs.
E.g. doesn't handle:
* invisible text
* overlay
* display table
* color
* ...

*/
function FrameRenderer(width, height){
    this.width = width;
    this.height = height;
    //index i = line i of frame
    //each value is an array of fragments of buffers which together represent text at index i
    //fragment = one line of one window
    this.lines = [];
}

FrameRenderer.prototype.getLine = function(index){
    if(index > this.height - 1){
	var error = new RangeError(index + 
			       " is outside valid lines (min 0, max " + 
			       (this.height - 1) + ")");
        error.lineRequested = index;
	throw error;
    }
    if(this.lines[index])
	return this.lines[index];
    else{
	this.lines[index] = [];
	return this.lines[index];
    } 
}

/*
@throws RangeError no segment corresponding to x, y
@returns {object} segment including where x, y is located
*/
FrameRenderer.prototype.getFragmentAt = function(x, y){
    if(!this.lines[y])
	throw new RangeError("no line " + y);
    var result;
    if(this.lines[y].some(function(fragment){
	if(fragment.columnLeft <= x && 
	   x <= fragment.columnLeft + fragment.text.length - 1){
	    result = fragment;
	    return true;
	}
    })){
	return result;
    } else
	throw new RangeError(
	    "no fragment at column " + x + " for the line " + y);
}

/*
Add data to the segment corresponding to x, y
@param {object} data key/values to be added to the segment
@throws RangeError no segment corresponding to x, y
*/
FrameRenderer.prototype.addDataToFragment = function(x, y, data){
    $.extend(this.getFragmentAt(x, y), data);
}

FrameRenderer.prototype.processData = function(displayData){
    this.addWindows(displayData.windows);
}

FrameRenderer.prototype.addWindows = function(windows){
    windows.forEach(function(window){
	this.addWindow(window);
    }, this);
}

//TODO missing hscroll handling
FrameRenderer.prototype.addWindow = function(window){
    //split. this excludes "\\n" (backslash n typed in a buffer)
    var lines = window.text.replace(/\t/g, nSpaces(window.tabWidth)).split(/\n/g);
    var windowWidth = window.right - window.left + 1;
    //split lines too big. Emulates wrap
    //the place at which line are split can vary
    //saw case where it is width (w/ gui) anohther width -1 (terminal)
    lines = $.map(lines, function(line){//map flattens
	if(line == "") return line;
	return line.split(
	    //parenthesis to include match in results
	    RegExp("(.{" + (windowWidth - 2) + "})")
	).filter(Boolean);//remove "" from the array. "" is falsey.
    });

    lines[window.bottom - window.top] = window.modeLine.slice(0, windowWidth - 1);

    //make sure lines don't expand past :bottom
    //Our rendering algorithm (line split, horizontal scroll,...) might
    //not fit the one used by emacs. Make sure we don't have too many lines
    lines = lines.splice(0, window.bottom - window.top + 1)

    lines.forEach(function(line, index){
	var fragment = {columnLeft: window.left, 
			columnRight: window.right, 
			text: line};
	if(window.point && window.point.y == index + window.top){
	    fragment.point = window.point;
	}
	this.getLine(window.top + index).push(fragment);
	//TODO ordered insertion
	this.getLine(window.top + index).sort(function(a, b){
	    return a.columnLeft - b.columnLeft
	});
    }, this);
}

FrameRenderer.prototype.renderLine = function(line, target){
    var that = this;
    function reduceLine(acc, windowSegment){
	target.append(
	    htmlEncode(nSpaces((windowSegment.columnLeft - acc.charCount))));
	target.append(that.renderSegment(windowSegment));
	return {charCount:
		    acc.charCount + //previously added
		    windowSegment.text.length + //length of actual text in this segment
	            (windowSegment.columnLeft - acc.charCount)}; //spaces added
    }
    //accumulator is the rendered string + the number of chars in the rendered string
    //must keep track of number of chars handled because
    //one char in the source may be rendered with multiple chars. e.g.: ' ' => &nbsp;
    var result = line.reduce(reduceLine, {charCount:0});
    return target.append('<br>\n');
}

/*
 Render the segment
 @param {object} segment
 @return {string} the string representation of the segment
 */
FrameRenderer.prototype.renderSegment = function(segment){
    var span = $('<span>');
    span.addClass('fragment');
    span.css('max-width', 
	     ""+(char_size().width * 
		 (segment.columnRight - segment.columnLeft + 1)) + "px");
    //debug columns
    //var color = segment.columnLeft > 5? 'white' :'red';
    //if(segment.columnLeft > 110) color = 'green';
    //span.css('background-color', color);
    
    /*
     Point (cursor) needs a span to be visible.
     Alter string of the segment containing the point (if there is one)
     to add the span
     */
    if(!segment.point){
	span.append(htmlEncode(segment.text));
	return span;
    }

    var x = segment.point.x;
    //if no character at point : add one space character and make it selected
    //The position where emacs tells us the point is might not exist:
    //-cursor is at end of line, after the last character
    //-our renderer doesn't match exactly emacs,
    // there might not be a character at text[x].
    var selectedChar = (segment.text[x])? segment.text[x] : " ";

    //does not check x validity in the segment, must have been correctly set
    var content = $.parseHTML([htmlEncode(segment.text.slice(0, x)),
			 '<span class="cursor">' + 
			 htmlEncode(selectedChar) +
			 '</span>',
			 htmlEncode(
			     segment.text.slice(
				 x+1,
				 segment.text.length))].join(''));
    span.append(content)
    return span;
}

FrameRenderer.prototype.render = function(target){
    for(var i = 0;i < this.height;i++){
	this.renderLine(this.getLine(i), target);
    }
}

function displayScreen(displayData){
    var terminal = $('.terminal-output');
    terminal.empty();
    $('.terminal').width(Math.ceil(displayData.width * char_size().width));
    //terminal.width(Math.ceil(displayData.width * char_size().width));

    var renderer = new FrameRenderer(displayData.width, displayData.height);
    renderer.processData(displayData);
    renderer.render(terminal);
}
