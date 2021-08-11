/* Author: Wouter Beek

*/

/* http_post(url, about, form, method)

@param url    is the url to post to (/news/)
@param about  is the annotated object or null
@param form   is the form for editing the post
@param method is the HTTP REST method to use
*/

function error(text)
{ $("#dialog").html(text);
  $("#dialog").dialog();
}

function http_post(url, about, form, method)
{ var title1 = form.find(".title").val();
  var title2;

  if (title1 === undefined)
  { title2 = null;
  } else
  { title2 = title1;
  }

  $.ajax(url,
	 { "contentType": "application/json; charset=utf-8",
	   "data": JSON.stringify(
           { "content": form.find(".markItUpEditor").val(),
	     "meta": {
                "about": about,
                "importance": parseFloat(form.find(".importance").val()),
                "time": {
                  "freshness-lifetime":
		     parseInt(form.find(".freshness-lifetime").val(), 10),
                  "type": "time"
                },
                "type": "meta"
              },
	     "title": title2,
	     "type": "post"
	   }),
	   "dataType": "json",
	   "success": function() {location.reload();},
	   "error": function(xhr, textStatus, errorThrown)
		    { error(xhr.responseText);
		    },
	   "type": method
          });
}


function vote(URL, id, vote)
{ $.ajax(URL,
	 { "contentType": "application/json; charset=utf-8",
	   "dataType": "json",
	   "data": JSON.stringify({ "id": id,
	                            "vote": vote
				  }),
	   "success": function()
		      { location.reload();
		      },
	   "error": function(xhr, textStatus, errorThrown)
		    { error(xhr.responseText);
		    },
	   "type": "POST"
	 });
}


function prepare_post(RESTURL, VoteURL, About)
{ // Clicking this removes the UI for entering a new post.
  $("#add-post-cancel").click(function(e)
  { e.preventDefault();
    $("#add-post-links").css("display","none");
    $("#add-post-content").css("display","none");
    $("#add-post-link").css("display","block");
  });

  // Clicking this brings up the UI for entering a new post.
  $("#add-post-link").click(function(e)
  { e.preventDefault();
    $(this).css("display","none");
    $("#add-post-content").css("display","block");
    $("#add-post-links").css("display","block");
  });

  // Clicking this submits the current content as a new post.
  $("#add-post-submit").click(function(e)
  { e.preventDefault();
    http_post(RESTURL, About, $("#add-post-content"), "POST");
  });

  // Clicking this removes the UI for editing an existing post.
  $(".save-post-cancel").click(function(e)
  { e.preventDefault();
    var article = $(this).closest("article");
    article.children(".edit-post-links").css("display","none");
    article.children("form").css("display","none");
    article.children("header").css("display","block");
    article.children("section").css("display","block");
  });

  // Clicking this brings up the UI for editing an existing post.
  // Notice that this first hides the readable content element,
  //  and then shows the writable content element.
  $(".edit-post-link").click(function(e)
  { e.preventDefault();
    var article = $(this).closest("article");
    article.children("header").css("display","none");
    article.children("section").css("display","none");
    article.children("form").css("display","block");
    article.find("textarea").css("display","block");
    article.find(".save-post-links").css("display","block");
  });

  // Clicking this submits the current changes to a post.
  $(".save-post-submit").click(function(e)
  { e.preventDefault();
    var id = $(this).closest(".post").attr("id");
    http_post(RESTURL+id,
	      About,
	      $(this).closest(".edit-post-content"),
	      "PUT");
  });

  // Mark-it-up support for entering post content.
  $(".markItUp").markItUp(pldoc_settings);

  // Clicking this removes an existing post.
  $(".delete-post-link").click(function(e)
  { e.preventDefault();
    var id = $(this).parents(".post").attr("id");
    $.ajax(RESTURL+id,
	   { "contentType": "application/json; charset=utf-8",
	     "dataType": "json",
	     "success": function() {location.reload();},
	     "type": "DELETE"
	   });
  });

  // Clicking this increases the number of votes by one.
  $(".post-vote-up").click(function(e)
  { e.preventDefault();
    var id = $(this).parents(".post").attr("id");

    vote(VoteURL, id, 1);
  });

  // Clicking this decreases the number of votes by one.
  $(".post-vote-down").click(function(e)
  { e.preventDefault();
    var id = $(this).parents(".post").attr("id");

    vote(VoteURL, id, -1);
  });
}
