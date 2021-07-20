function serverMessage(msg){
    displayData = JSON.parse(msg.data);
    displayScreen(displayData);
 }

var ws;
function openedConnection(){
    $('.not-connected').hide();
    $('.connected').show();
    ws.send('{}');    
}
function closedConnection(){
    $('.not-connected').show();
    $('.connected').hide();
    setTimeout(connect, 3 * 1000);
}
function setupConnection(){
    connect();    
}
function connect(){
    if(ws){
	ws.onopen = null;
	ws.onclose = null;
    }
    var emacsURL = window.location.host + '/socket';
    console.log("Trying to connect to " + emacsURL);
    ws = new WebSocket('ws://' + emacsURL);
    ws.onopen = openedConnection;
    ws.onmessage = serverMessage;
    ws.onclose = closedConnection;
}

function execute(code){
    JSON.stringify(ws.send(JSON.stringify({code: code})));
}


//-----------------------------------------------------------------------------
//Code for the dynamic part of the page below
//Typical messy intertwined jquery code. Would be nice o user angular/ember

/*
 Called when there has potentially been a change in the list of buttons
*/
function buttonsMayHaveChanged(){
    if($('.buttons *').length)
	$('button[name=openRemoveButtonDialog]').removeClass('disabled');
    else
	$('button[name=openRemoveButtonDialog]').addClass('disabled');
}

function closeAddButtonDialog(){
    enableInputOverride();
    $('#buttonCode').val('');
    $('#buttonName').val('');
    $('#newButtonDialog').hide();
}

function addButton(name, code){
    if(code.trim()){
	var newButton = $('<button class="btn btn-primary btn-lg"></button>');
	newButton[0].code = code;
	if(!name || !name.trim()){
	    var match = code.match(/^[\s()]*([^\t\r\n\f()]+)/);
	    if(match)
		name = match[1];
	    else
		name = code.replace(/[\(\)\s]+/g, '');
	}
	if(name.length > 20){
	    name = name.slice(0, 17) + '…';
	}
	newButton.text(name);
	newButton.on('click', function(){
	    execute(this.code);
	});
	$('.buttons').append(newButton);
    }
}

function addButtons(buttons){
    $.each(buttons, function(){
	addButton(this.name, this.code);
    });
}

function addNavigationButtonSet(){
    var navigationButtons = [
	{name: "Recenter", code: "(recenter-top-bottom)"},
	//scroll-down = scroll text down = cursor up
	{name: "Page up", code: "(scroll-down-command)"},
	{name: "Page down", code: "(scroll-up-command)"},
	{name: "First line", code: "(beginning-of-buffer)"},
	{name: "Last line", code: "(end-of-buffer)"},
	{name: "Previous buffer", code: "(previous-buffer)"},
	{name: "Next buffer", code: "(next-buffer)"},
    ];
    addButtons(navigationButtons);
}

function addMultipleWindowsButtonSet(){
    var navigationButtons = [
	{name: "Previous window", code: "(other-window -1)"},
	{name: "Next window", code: "(other-window 1)"},
	{name: "Split Horizontally", code: "(split-window)"},
	{name: "Split Vertically", code: "(split-window nil nil t)"},
	{name: "Close window", code: "(delete-window)"},
    ];
    addButtons(navigationButtons);
}

function openRemoveButtonDialog(){
    disableInputOverride();
    var buttonList = $('#removeButtonDialog div.buttonList');
    $('#removeButtonDialog .modal-body').append(buttonList);
    buttonList.append(
	$('.buttons button').map(function(){
	    var checkDiv = $('<div class="form-group">');
	    var checkLabel = $('<label>');
	    var checkbox = $('<input type="checkbox"></input>');
	    checkbox.data('divButton', $(this));
	    checkLabel.append(checkbox[0]);
	    checkLabel.append($(this).text());
	    checkDiv.append(checkLabel[0]);
	    return checkDiv[0];
	})
    );
    $('#removeButtonDialog').show();
}

function closeRemoveButtonDialog(){
    enableInputOverride();
    $('#removeButtonDialog .buttonList').empty();
    $('#removeButtonDialog').hide();
}

function removeButtons(){
    $('#removeButtonDialog .buttonList :checkbox:checked').each(function(){
	$(this).data('divButton').remove();
    });
    buttonsMayHaveChanged();
}

/*
 Normal page interaction are de-activated to pass interaction data directly
 To emacs. This disable interaction stealing and give it back to the browser
*/
function disableInputOverride(){
    removeKeyEvents();
    $('body').swipe('disable');
}

function enableInputOverride(){
    addKeyEvents();
    $('body').swipe('enable');
}

$(function(){
    initkey();
    setupConnection();

    //TODO refactor dialogs to automate opening/closing/... 
    //and reduce duplication
    $('button[name=openButtonDialog]').on('click', function(){
	disableInputOverride();
	$('#newButtonDialog').show();
	$('#newButtonDialog #buttonCode').focus();
	
    });
    $('#newButtonDialogClose').on('click', function(){	
	closeAddButtonDialog();
    });
    $('button[name=buttonAdd]').on('click', function(){
	addButton($('#buttonName').val(), $('#buttonCode').val());
	buttonsMayHaveChanged();
	closeAddButtonDialog();
    });
    $('button[name=navigationButtonSetAdd]').on('click', function(){
	addNavigationButtonSet();
	buttonsMayHaveChanged();
	closeAddButtonDialog();
    });
    $('button[name=multipleWindowsButtonSetAdd]').on('click', function(){
	addMultipleWindowsButtonSet();
	buttonsMayHaveChanged();
	closeAddButtonDialog();
    });


    $('button[name=openRemoveButtonDialog]').on('click', function(){
	openRemoveButtonDialog();
    });
    $('#removeButtonDialogClose').on('click', function(){	
	closeRemoveButtonDialog();
    });
    $('#removeButtonDialog button[name=buttonRemove]').on('click', function(){
	removeButtons();
	closeRemoveButtonDialog();
    });

    //jquery.touchSwipe
    $('body').swipe({
	swipeUp:function(){
	    execute("(scroll-up-command)");
	},
	swipeDown:function(){
	    execute("(scroll-down-command)");
	},
	swipeLeft:function(){
	    execute("(previous-buffer)");
	},
	swipeRight:function(){
	    execute("(next-buffer)");
	},

	/*
	 don't work so well

	 tap:function(event, target) {},
        doubleTap:function(event, target) {
	    console.log("double");
	    //split vertically
	    execute("(split-window nil nil t)");
        },
        longTap:function(event, target) {
	    console.log("long");
	    execute("(delete-window)");
        },
	pinchIn:function(event, direction, distance, duration, fingerCount, pinchZoom){
	    execute("(shrink-window-Horizontally 5)");
        },
        pinchOut:function(event, direction, distance, duration, fingerCount, pinchZoom){
	    execute("(enlarge-window-horizontally 5)");
        },
	 */
    });
});
