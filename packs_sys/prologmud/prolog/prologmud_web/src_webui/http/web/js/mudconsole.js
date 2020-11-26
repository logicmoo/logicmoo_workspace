function read_messages()
{ $.ajax({
	   url: "/mudconsole/mc_message",
	   success: function(data, status, xhr) {
	     var clear   = xhr.getResponseHeader("X-Clear");
	     var id      = xhr.getResponseHeader("X-Id");
	     var timeout = xhr.getResponseHeader("X-Timeout");

	     if ( !timeout )
	     { if ( clear )
	       { $("#"+id).empty();
		 $("#"+id).removeClass("inactive");
		 $("#preview").empty();
	       }
	       $("#"+id).append(data);
	       $("#error").empty();
	     }
	     read_messages();
	   },
	   error: function(jqXHDR, why, error) {
	     { $("#error").empty();
	       $("#error").append('<h4 class="error">Error:</h4>');
	       if ( jqXHDR.status )
	       { $("#error").append("Status:" + jqXHDR.status);
		 $("#error").append(jqXHDR.responseText);
		 read_messages();
	       } else
	       { $("#error").append("Lost connection; stopped!");
	       }
	     }
	   }
	 });
}

read_messages();
