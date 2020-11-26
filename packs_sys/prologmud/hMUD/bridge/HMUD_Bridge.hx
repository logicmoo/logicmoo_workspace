/*
 * The MIT License
 *
 * Copyright (c) 2009 Alonso Andres
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/*
 * Based on socketBridge
 * http://matthaynes.net/blog/2008/07/17/socketbridge-flash-javascript-socket-bridge/
 */
import AnsiToHtml;

class HMUD_Bridge {

    static var socket = new flash.net.Socket();
    static var JSObj = "HMUD_Client";

    static function main()
    {
        if (flash.external.ExternalInterface.available) {
            /* Calls the javascript load method once the SWF has loaded */
            sendMessage("loaded");

            /* \xA0 is the NBSP character. oddly enough, that's the only way
               I could make the line breaks work correctly in IE7. So here
               we are telling AnsiToHtml to use this char instead of common
               spaces when generating the HTML. */
            if (sendMessage("usingIE"))
                AnsiToHtml.spaceString = "\xA0";
            else
                AnsiToHtml.spaceString = " ";

            /*
             * Event listeners
             */

            /* CONNECT */
            socket.addEventListener(flash.events.Event.CONNECT, function(e) : Void {
                    trace("CONNECT");
                    sendMessage("connected");
                }
            );

            /* CLOSE */
            socket.addEventListener(flash.events.Event.CLOSE, function(e) : Void {
                    trace("CLOSE");
                    sendMessage("disconnected");
                }
            );


            /* IO_ERROR */
            socket.addEventListener(flash.events.IOErrorEvent.IO_ERROR, function(e) : Void {
                    trace("IO_ERROR: " +  e.text);
                    sendMessage("ioError", e.text);
                }
            );

            /* SECURITY_ERROR */
            socket.addEventListener(flash.events.SecurityErrorEvent.SECURITY_ERROR, function(e) : Void {
                    trace("SECURITY_ERROR: " +  e.text);
                    sendMessage("securityError", e.text);
                }
            );

            /* SOCKET_DATA */
            socket.addEventListener(flash.events.ProgressEvent.SOCKET_DATA, function(e) : Void {
                    //var msg = socket.readUTFBytes(socket.bytesAvailable);

                    if (socket.bytesAvailable < 1)
                        return;

                    trace("SOCKET_DATA: " + socket.bytesAvailable + " bytes.");
                    var bytes = new flash.utils.ByteArray();
                    socket.readBytes(bytes);
                    var msg = bytes.toString();

                    /*
                     * { START of ugly section
                     *
                     * FIXME: perform a TRUE telnet protocol parsing here, and reproduce beeps etc.
                     */
                    // Parse telnet options (I'm lazy, just get last telnet option)
                    // IAC WILL ECHO 
                    if (~/(^|[^\xFF])\xFF\xFB\x01[^\xFF]*$/.match(msg))
                        sendMessage("echoOff");
                    // IAC WONT ECHO
                    else if (~/(^|[^\xFF])\xFF\xFC\x01[^\xFF]*$/.match(msg))
                        sendMessage("echoOn");

                    // now get rid of those telnet chars.
                    msg = ~/\xFF[\xFC\xFB]\x01/g.replace(msg, "");
                    // escaped IAC, that is: IAC IAC = \xFF
                    msg = ~/\xFF\xFF/g.replace(msg, "\xFF");
                    /*
                     * } END of ugly section
                     */

                    // Ansi To Html!
                    msg = AnsiToHtml.parse(msg);
                    sendMessage("receive", ~/\\/g.replace(msg, "\\\\"));
                }
            );

            /*
             * Set External Interface Callbacks
             */
            flash.external.ExternalInterface.addCallback("connected", connected);
            flash.external.ExternalInterface.addCallback("connect", connect);
            flash.external.ExternalInterface.addCallback("close", close);
            flash.external.ExternalInterface.addCallback("command", command);
            //flash.external.ExternalInterface.addCallback("saveLog", saveLog);
        } else {
            trace("Flash external interface not available");
        }   
    }
    
    static function sendMessage(msg, ?data) {
        return flash.external.ExternalInterface.call(JSObj+".handleMessage", msg, data);
    }

    static function connected() {
    	return socket.connected;
    }

    static function connect(host, port, policyPort)
    {
        sendMessage("connecting");

        trace("Load policy from xmlsocket://" + host + ":" + policyPort);
        flash.system.Security.loadPolicyFile("xmlsocket://" + host + ":" + policyPort);

    	trace("Connecting to socket server at " + host + ":" + port);
        socket.connect(host, port);    	
    }
    
    static function close() {
    	if (socket.connected) {
            trace("Closing current connection");
            socket.close();
        } else {
            trace("Cannot disconnect to server because there is no connection!");
        }
    }

    static function command(msg) {
    	if (socket.connected) {
            var iac = ~/\xFF/g;
            var nl = ~/[\r\n]/g;

            msg = iac.replace(msg, "\xFF\xFF");	/* doubling IAC character (Telnet Protocol) */
            msg = nl.replace(msg, "");		/* removing newlines */
            msg += "\r\n";			/* appending newline */
            trace("Writing '" + msg + "' to server");

            /*
             * Flash 10 on Linux: socket.writeMultiByte is *broken*.
             * So here we use this nasty hack from
             * http://www.flexiblefactory.co.uk/flexible/?p=75
             * 
             * Flash 9 works fine on all platforms, I just hope this will be fixed
             * on Flash 11, so this test just checks for version 10.
             */
            var flash10OnLinux = ~/^LNX 10,/.match(flash.system.Capabilities.version);
            var i = 0;
            if (flash10OnLinux)
                for (i in 0 ... msg.length)
                    socket.writeByte(msg.charCodeAt(i));
            else
                socket.writeMultiByte(msg, "iso-8859-1");
            socket.flush();
        } else {
            trace("Cannot write to server because there is no connection!");		
        }
    }    

    /*
     * FIXME: Damn! Flash 10 will only open a file dialog if the code runs under a
     *        user event, the problem is: we are using external interface, Flash
     *        does not recognize user events from there! Think in a solution...
     *        (positioning the flash player under the mouse cursor and making it
     *        accept a click?)

    static function saveLog(log) {
        try {
            var file = new flash.net.FileReference();
            //configureListeners(file);
            //file.save(log, "log-" + Date.now().toString());
            file.save(log, "log.html");
        } catch( unknown : Dynamic ) {
           trace("Unknown exception : "+Std.string(unknown));
           sendMessage("receive", Std.string(unknown));
        }

    }
    */
}
