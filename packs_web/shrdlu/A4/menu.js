// CHOICE-BASED MENU: --------------------------------------------------------

function Menu (x,y,speed,items,effects,cancelAction) {
	this.centerx = x;
	this.centery = y;
	this.speed = speed;
	this.state = 0;
	this.cycle = 0;
	this.f1 = 0.0;
	this.f2 = 0.0;
	this.items = items;
	this.effects = effects;
	this.selected = 0;
	this.menuHeight = this.items.length * 12 + 12;
	this.menuWidth = 24;
	this.effectFunction = null;
	this.cancelAction = cancelAction;
	for(var i = 0;i<this.items.length;i++) {
		var tmp = this.items[i].length*11+24;
		if (tmp>this.menuWidth) this.menuWidth = tmp;
	}

	this.draw = function() {
    	var menuItemTransparency = 255;
    	var tmpMenuHeight = this.menuHeight;
	    switch(this.state) {
	      case 0: this.f1 = this.cycle / this.speed;
	              this.f2 = (this.cycle / this.speed)-1;
	              if (this.f1>1) this.f1 = 1;
	              if (this.f2<0) this.f2 = 0;
	              tmpMenuHeight = this.menuHeight * this.f1;
	              menuItemTransparency*=this.f2;
	              break;
	      case 1: this.f1 = this.f2 = 1.0;
	              break;
	      case 2: this.f1 = 2 - this.cycle / this.speed;
	              this.f2 = 1 - (this.cycle / this.speed);
	              if (this.f1>1) this.f1 = 1;
	              if (this.f2<0) this.f2 = 0;
	              tmpMenuHeight = this.menuHeight * this.f1;
	              menuItemTransparency*=this.f2;
	              break;
	    }    
	    ctx.fillStyle = '#000000';  
	    ctx.strokeStyle = '#ffffff';  
	    ctx.beginPath();  
	    ctx.rect(width/2 + this.centerx - this.menuWidth/2, height/2 + this.centery-tmpMenuHeight/2, this.menuWidth, tmpMenuHeight);  
	    ctx.closePath();  
	    ctx.fill();  
	    ctx.stroke();  

	    if (menuItemTransparency>0) {
	      for(var i = 0;i<this.items.length;i++) {
	        var item = this.items[i];
	        var r = menuItemTransparency;
	        var g = menuItemTransparency;
	        var b = menuItemTransparency;

	        if (i==this.selected) {
	          var a = this.cycle/5;
	          r = 128 + Math.sin(a)*127;
	          g = 128 + Math.sin(a)*127;
	          b = 196 + Math.sin(a)*63;
	        }

	        ctx.font = "11px Emulogic";  
	        ctx.fillStyle = "rgb(" + Math.floor(r) + "," + Math.floor(g) + "," + + Math.floor(b) + ")";
	        ctx.fillText(item, width/2 + this.centerx - this.menuWidth/2 + 12, height/2 + this.centery-this.menuHeight/2 + 16 + i*12);
	      }
	    }
	}

	this.update = function(game) {
	    switch(this.state) {
	      case 0: 
	              if (this.cycle>this.speed*2) {
	                this.state = 1;
	                this.cycle = 0;
	                this.selected = 0;
	              }
	              break;
	      case 1: 
	              // keyboard controls to navigate menu:
	              if (keyPress(KEY_UP)) {
	              	if (this.selected>0) {
	            		this.selected--;
	            	} else {
	            		this.selected = this.items.length-1;
	            	}
	              }
	              if (keyPress(KEY_DOWN)) {

	                if (this.selected<this.items.length-1) {
	              		this.selected++;
	              	} else {
	              		this.selected = 0;
	              	}
	              }
	              if (keyPress(32) || keyPress(13)) {
	                this.state = 2;
	                this.cycle = 0;
	              }
	              if (keyPress(27) && cancelAction!=null) {
	              	this.selected = cancelAction;
	              	this.state = 2;
	              	this.cycle = 0;
	              }

	              // check mouse:
			      for(i = 0;i<this.items.length;i++) {
			      	if (mouseState.x >= width/2 + this.centerx - this.menuWidth/2 && 
			      		mouseState.x < width/2 + this.centerx + this.menuWidth/2 && 
			      		mouseState.y >= height/2 + this.centery - this.menuHeight/2 + 8 + i*12 &&
			      		mouseState.y < height/2 + this.centery - this.menuHeight/2 + 20 + i*12) {
			      		this.selected = i;

			      		if (mouseState.button == 1 && mouseState.oldButton ==0) {
			      			this.state = 2;
			      			this.cycle = 0;
			      		}
			      	}
			      }	
	              break;
	      case 2:
	              if (this.cycle>this.speed*2) return this.effects[this.selected];
	    }
	    this.cycle++;
		return null;
	}
}


// INPUT DIALOGUE: --------------------------------------------------------


function InputDialogue (x,y,speed,message,def,maxlength) {
	this.centerx = x;
	this.centery = y;
	this.speed = speed;
	this.state = 0;
	this.cycle = 0;
	this.f1 = 0.0;
	this.f2 = 0.0;
	this.message = message;
	this.buffer = def;
	this.effectFunction = null;
	this.maxlength = maxlength;
	this.menuHeight = 2 * 12 + 12;
	this.menuWidth = Math.max(this.message.length*11+24,maxlength*11+24);

	this.draw = function() {
    	var menuItemTransparency = 255;
    	var tmpMenuHeight = this.menuHeight;
	    switch(this.state) {
	      case 0: this.f1 = this.cycle / this.speed;
	              this.f2 = (this.cycle / this.speed)-1;
	              if (this.f1>1) this.f1 = 1;
	              if (this.f2<0) this.f2 = 0;
	              tmpMenuHeight = this.menuHeight * this.f1;
	              menuItemTransparency*=this.f2;
	              break;
	      case 1: this.f1 = this.f2 = 1.0;
	              break;
	      case 2: this.f1 = 2 - this.cycle / this.speed;
	              this.f2 = 1 - (this.cycle / this.speed);
	              if (this.f1>1) this.f1 = 1;
	              if (this.f2<0) this.f2 = 0;
	              tmpMenuHeight = this.menuHeight * this.f1;
	              menuItemTransparency*=this.f2;
	              break;
	    }    
	    ctx.fillStyle = '#000000';  
	    ctx.strokeStyle = '#ffffff';  
	    ctx.beginPath();  
	    ctx.rect(width/2 + this.centerx - this.menuWidth/2, height/2 + this.centery-tmpMenuHeight/2, this.menuWidth, tmpMenuHeight);  
	    ctx.closePath();  
	    ctx.fill();  
	    ctx.stroke();  

	    if (menuItemTransparency>0) {
	        var r = menuItemTransparency;
	        var g = menuItemTransparency;
	        var b = menuItemTransparency;

	        ctx.font = "11px Emulogic";  
	        ctx.fillStyle = "rgb(" + Math.floor(r) + "," + Math.floor(g) + "," + + Math.floor(b) + ")";
	        ctx.fillText(this.message, width/2 + this.centerx - this.menuWidth/2 + 12, height/2 + this.centery-this.menuHeight/2 + 16);
	        ctx.fillText(this.buffer, width/2 + this.centerx - this.menuWidth/2 + 12, height/2 + this.centery-this.menuHeight/2 + 16 + 12);
            
            var a = this.cycle/5;
            r = 128 + Math.sin(a)*127;
            g = 128 + Math.sin(a)*127;
            b = 196 + Math.sin(a)*63;
	        ctx.fillStyle = "rgb(" + Math.floor(r) + "," + Math.floor(g) + "," + + Math.floor(b) + ")";
		    ctx.beginPath();  
            ctx.rect(width/2 + this.centerx - this.menuWidth/2 + 14 + this.buffer.length*11, 
            		 height/2 + this.centery-this.menuHeight/2 + 16 + 2,
            		 8, 12);
		    ctx.closePath();  
		    ctx.fill();  
	    }
	}

	this.update = function(game) {
	    switch(this.state) {
	      case 0: 
	              if (this.cycle>this.speed*2) {
	                this.state = 1;
	                this.cycle = 0;
	              }
	              break;
	      case 1: // keyboard controls:
	      		  if (this.buffer.length<this.maxlength) {
		      		  for(i = 65;i<=90;i++) {
		      		  	if (keyPress(i)) {
		      		  		this.buffer = this.buffer + String.fromCharCode(i);
		      		  	}
		      		  }
		      		  for(i = 48;i<=57;i++) {
		      		  	if (keyPress(i)) {
		      		  		this.buffer = this.buffer + String.fromCharCode(i);
		      		  	}
		      		  }
		      	  }
		      	  if (this.buffer.length>0) {
		      	  	if (keyPress(8)) this.buffer = this.buffer.slice(0,this.buffer.length-1);
		      	  }

	      		  if (keyPress(27)) {
	      		  	this.state = 2;
	      		  	this.cycle = 0;
	      		  	this.buffer = "";
	      		  }
	              if (keyPress(13)) {
	                this.state = 2;
	                this.cycle = 0;
	              }
	              break;
	      case 2:
	              if (this.cycle>this.speed*2) return this.buffer;
	    }
	    this.cycle++;
		return null;
	}
}



// SPELL DIRECTION DIALOGUE: --------------------------------------------------------


function SpellDirectionDialogue (x,y,speed,message,spell) {
	this.centerx = x;
	this.centery = y;
	this.speed = speed;
	this.state = 0;
	this.cycle = 0;
	this.f1 = 0.0;
	this.f2 = 0.0;
	this.message = message;
	this.retvalue = 0;
	this.menuHeight = 2 * 12;
	this.menuWidth = this.message.length*11+24;
	this.spell = spell;

	this.draw = function() {
    	var menuItemTransparency = 255;
    	var tmpMenuHeight = this.menuHeight;
	    switch(this.state) {
	      case 0: this.f1 = this.cycle / this.speed;
	              this.f2 = (this.cycle / this.speed)-1;
	              if (this.f1>1) this.f1 = 1;
	              if (this.f2<0) this.f2 = 0;
	              tmpMenuHeight = this.menuHeight * this.f1;
	              menuItemTransparency*=this.f2;
	              break;
	      case 1: this.f1 = this.f2 = 1.0;
	              break;
	      case 2: this.f1 = 2 - this.cycle / this.speed;
	              this.f2 = 1 - (this.cycle / this.speed);
	              if (this.f1>1) this.f1 = 1;
	              if (this.f2<0) this.f2 = 0;
	              tmpMenuHeight = this.menuHeight * this.f1;
	              menuItemTransparency*=this.f2;
	              break;
	    }    
	    ctx.fillStyle = '#000000';  
	    ctx.strokeStyle = '#ffffff';  
	    ctx.beginPath();  
	    ctx.rect(width/2 + this.centerx - this.menuWidth/2, height/2 + this.centery-tmpMenuHeight/2, this.menuWidth, tmpMenuHeight);  
	    ctx.closePath();  
	    ctx.fill();  
	    ctx.stroke();  

	    if (menuItemTransparency>0) {
	        var r = menuItemTransparency;
	        var g = menuItemTransparency;
	        var b = menuItemTransparency;

	        ctx.font = "11px Emulogic";  
	        ctx.fillStyle = "rgb(" + Math.floor(r) + "," + Math.floor(g) + "," + + Math.floor(b) + ")";
	        ctx.fillText(this.message, width/2 + this.centerx - this.menuWidth/2 + 12, height/2 + this.centery-this.menuHeight/2 + 16);
	    }
	}

	this.update = function(game) {
	    switch(this.state) {
	      case 0: 
	              if (this.cycle>this.speed*2) {
	                this.state = 1;
	                this.cycle = 0;
	              }
	              break;
	      case 1: // keyboard controls:
	      		  if (keyPress(13)) {
	      		  	this.state = 2;
	      		  	this.cycle = 0;
	      		  	this.retvalue = {spell:this.spell, direction:DIRECTION_NONE};
	      		  } else if (keyPress(KEY_LEFT)) {
	      		  	this.state = 2;
	      		  	this.cycle = 0;
	      		  	this.retvalue = {spell:this.spell, direction:DIRECTION_LEFT};
	      		  } else if (keyPress(KEY_UP)) {
	      		  	this.state = 2;
	      		  	this.cycle = 0;
	      		  	this.retvalue = {spell:this.spell, direction:DIRECTION_UP};
	      		  } else if (keyPress(KEY_RIGHT)) {
	      		  	this.state = 2;
	      		  	this.cycle = 0;
	      		  	this.retvalue = {spell:this.spell, direction:DIRECTION_RIGHT};
	      		  } else if (keyPress()) {
	      		  	this.state = 2;
	      		  	this.cycle = 0;
	      		  	this.retvalue = {spell:this.spell, direction:DIRECTION_DOWN};
	      		  } else if (keyPress(27)) {
	      		  	this.state = 2;
	      		  	this.cycle = 0;
	      		  	this.retvalue = {spell:this.spell, direction:null};
	      		  }
	              break;
	      case 2:
	              if (this.cycle>this.speed*2) {
	              	return this.retvalue;
	              }
	    }
	    this.cycle++;
		return null;
	}
}


// TRADE DIALOGUE: --------------------------------------------------------


function TradeDialogue(x,y,speed,characterLeft,characterRight,map) {
	this.maxItemsToDisplay = 10;
	this.centerx = x;
	this.centery = y;
	this.speed = speed;
	this.state = 0;
	this.cycle = 0;
	this.f1 = 0.0;
	this.f2 = 0.0;
	this.menuHeight = 108+this.maxItemsToDisplay*12;
	this.menuWidth = 528;
	this.characterLeft = characterLeft;
	this.characterRight = characterRight;
	this.columnSelected = 3;	// 0: give, 1: sell, 2: buy, 3: done
	this.itemSelected = 0;
	this.map = map;

	this.draw = function() {
    	var menuItemTransparency = 255;
    	var tmpMenuHeight = this.menuHeight;
	    switch(this.state) {
	      case 0: this.f1 = this.cycle / this.speed;
	              this.f2 = (this.cycle / this.speed)-1;
	              if (this.f1>1) this.f1 = 1;
	              if (this.f2<0) this.f2 = 0;
	              tmpMenuHeight = this.menuHeight * this.f1;
	              menuItemTransparency*=this.f2;
	              break;
	      case 1: this.f1 = this.f2 = 1.0;
	              break;
	      case 2: this.f1 = 2 - this.cycle / this.speed;
	              this.f2 = 1 - (this.cycle / this.speed);
	              if (this.f1>1) this.f1 = 1;
	              if (this.f2<0) this.f2 = 0;
	              tmpMenuHeight = this.menuHeight * this.f1;
	              menuItemTransparency*=this.f2;
	              break;
	    }    

	    var x1 = width/2 + this.centerx - this.menuWidth/2;
	    var y1 = height/2 + this.centery-tmpMenuHeight/2;

        ctx.font = "11px Emulogic";  

	    ctx.fillStyle = '#000000';  
	    ctx.strokeStyle = '#ffffff';  
	    ctx.beginPath();  
	    ctx.rect(width/2 + this.centerx - this.menuWidth/2, height/2 + this.centery-tmpMenuHeight/2, this.menuWidth, tmpMenuHeight);  
	    ctx.closePath();  
	    ctx.fill();  
	    ctx.stroke();  

	    if (menuItemTransparency>0) {
		    // draw the characters at the top with their names
	        var r = menuItemTransparency;
	        var g = menuItemTransparency;
	        var b = menuItemTransparency;
	        var style = "rgb(" + Math.floor(r) + "," + Math.floor(g) + "," + + Math.floor(b) + ")";
	        var a = this.cycle/5;
            r = 128 + Math.sin(a)*127;
            g = 128 + Math.sin(a)*127;
            b = 196 + Math.sin(a)*63;
	        var hl_style = "rgb(" + Math.floor(r) + "," + Math.floor(g) + "," + + Math.floor(b) + ")";

		    this.characterLeft.drawInSpecificCoordinates(menuItemTransparency,x1+8,y1+8,null);
		    this.characterRight.drawInSpecificCoordinates(menuItemTransparency,x1+this.menuWidth-8-TILE_WIDTH,y1+8,null);
		    // find if they are saying something and draw it:
			for(var i = this.map.textOverlays.length-1;i>=0;i--) {
				var msg = this.map.textOverlays[i];
			    var f1 = msg.cycle/25.0;
			    var f2 = (msg.duration-msg.cycle)/25.0;
			    if (f1<0) f1 = 0;
			    if (f1>1) f1 = 1;
			    if (f2<0) f2 = 0;
			    if (f2>1) f2 = 1;
			    var f = f1*f2*menuItemTransparency;

			    if (msg.bubble && msg.speaker==this.characterLeft) {
				    var tmp = ctx.globalAlpha;
				    ctx.globalAlpha = f;
					ctx.font = "8px Emulogic";  
			    	this.map.drawTextBubble(1,x1+8 + TILE_WIDTH/2,y1+8,msg);   	
			        ctx.font = "11px Emulogic";  
				    ctx.globalAlpha = tmp;
			    }
			    if (msg.bubble && msg.speaker==this.characterRight) {
				    var tmp = ctx.globalAlpha;
				    ctx.globalAlpha = f;
					ctx.font = "8px Emulogic";  
			    	this.map.drawTextBubble(1,x1+this.menuWidth-8-TILE_WIDTH/2,y1+8,msg);   	
			        ctx.font = "11px Emulogic";  
				    ctx.globalAlpha = tmp;
			    }
			}		    

		    
	        ctx.fillStyle = style;
		    ctx.strokeStyle = '#ffffff';  
	        ctx.fillText(this.characterLeft.name, x1 + TILE_WIDTH +16 , y1+24);
	        ctx.fillText(this.characterRight.name, x1+this.menuWidth-16-TILE_WIDTH - this.characterRight.name.length*11 , y1+24);

		    // draw the 3 columns with separators
		    ctx.beginPath();  
		    ctx.moveTo(x1 + 8, y1 + TILE_HEIGHT + 32);  
		    ctx.lineTo(x1 + this.menuWidth - 14, y1 + TILE_HEIGHT + 32);  
		    ctx.closePath();  
		    ctx.stroke();  

		    ctx.beginPath();  
		    ctx.moveTo(x1 + 8, y1 + this.menuHeight - 32);  
		    ctx.lineTo(x1 + this.menuWidth - 14, y1 + this.menuHeight - 32);  
		    ctx.closePath();  
		    ctx.stroke();  

		    ctx.beginPath();  
		    ctx.moveTo(x1 + 8 + 266, y1 + TILE_HEIGHT + 24 + 8);  
		    ctx.lineTo(x1 + 8 + 266, y1 + this.menuHeight - 32);  
		    ctx.closePath();  
		    ctx.stroke();  		    

		    var cx = x1 + 8;
		    var cy = y1 + TILE_HEIGHT + 24;
		    ctx.fillText("GIVE", cx, cy);
		    ctx.fillText("SELL", cx + 184, cy);
		    for(var i = 0;i<this.characterLeft.inventory.length && i<this.maxItemsToDisplay;i++) {
		      if (this.columnSelected==0 && this.itemSelected==i) ctx.fillStyle = hl_style;
		      											     else ctx.fillStyle = style;
		      ctx.fillText(this.characterLeft.inventory[i].name, cx,cy + 24 + 12*i);
		      if (this.characterLeft.inventory[i].value>0) {
			    if (this.columnSelected==1 && this.itemSelected==i) ctx.fillStyle = hl_style;
			      										       else ctx.fillStyle = style;
		      	ctx.fillText("" + this.characterLeft.inventory[i].value, cx + 184,cy + 24 + 12*i);
		      } else {
			    if (this.columnSelected==1 && this.itemSelected==i) ctx.fillStyle = hl_style;
			      										       else ctx.fillStyle = style;
		      	ctx.fillText("-", cx + 184,cy + 24 + 12*i);
		      }
		    }

		    cx += 278;
		    ctx.fillStyle = style;
		    ctx.fillText("BUY", cx, cy);
		    for(var i = 0;i<this.characterRight.inventory.length && i<this.maxItemsToDisplay;i++) {
			  if (this.columnSelected==2 && this.itemSelected==i) ctx.fillStyle = hl_style;
			      										     else ctx.fillStyle = style;
		      ctx.fillText(this.characterRight.inventory[i].name, cx,cy + 24 + 12*i);
		      if (this.characterRight.inventory[i].value>0) {
		      	ctx.fillText("" + this.characterRight.inventory[i].value, cx + 184,cy + 24 + 12*i);
		      } else {
		      	ctx.fillText("-", cx + 184,cy + 24 + 12*i);
		      }
		    }

		    // draw the "done" button
		    if (this.columnSelected==3) ctx.fillStyle = hl_style;
			      				   else ctx.fillStyle = style;
		    ctx.fillText("DONE", width/2 + this.centerx - 22, y1 + this.menuHeight - 8);
		}
	}

	this.update = function(game) {
	    switch(this.state) {
	      	case 0: 
	            if (this.cycle>this.speed*2) {
	                this.state = 1;
	                this.cycle = 0;
	            }
	            break;
	      	case 1: // keyboard controls:
				if (this.columnSelected==0) {
					if (keyPress(KEY_UP) && this.itemSelected>0) {
					  	this.itemSelected--;
					  	break;
					}
					if (keyPress(KEY_RIGHT)) {
					  	this.columnSelected++;
					  	break;
					}
					if (keyPress(KEY_DOWN)) {
					  	if (this.itemSelected<this.characterLeft.inventory.length-1 && this.itemSelected<this.maxItemsToDisplay-1) {
					      	this.itemSelected++;
					    } else {
					      	this.columnSelected = 3;
					    }
					}
				  	if (keyPress(32) || keyPress(13)) {
				  		// try to give something:
				  		this.transaction(game,
				  						 this.characterRight, this.characterLeft, 
				  						 this.characterLeft.inventory[this.itemSelected],
				  						 0);
				  	}
				} else if (this.columnSelected==1) {
					if (keyPress(KEY_LEFT)) {
					  	this.columnSelected--;
					  	break;
					}
					if (keyPress(KEY_UP) && this.itemSelected>0) {
					  	this.itemSelected--;
					  	break;
					}
					if (keyPress(KEY_RIGHT) && this.characterRight.inventory.length>0) {
					  	this.columnSelected++;
					  	if (this.itemSelected>=this.characterRight.inventory.length) this.itemSelected = this.characterRight.inventory.length-1;
					  	break;
					}
					if (keyPress(KEY_DOWN)) {
					  	if (this.itemSelected<this.characterLeft.inventory.length-1 && this.itemSelected<this.maxItemsToDisplay-1) {
					      	this.itemSelected++;
					    } else {
					      	this.columnSelected = 3;
					    }
					}
				  	if (keyPress(32) || keyPress(13)) {
				  		// try to sell something:
				  		this.transaction(game,
				  						 this.characterRight, this.characterLeft, 
				  						 this.characterLeft.inventory[this.itemSelected],
				  						 this.characterLeft.inventory[this.itemSelected].value);
				  	}
				} else if (this.columnSelected==2) {
					if (keyPress(KEY_LEFT) && this.characterLeft.inventory.length>0) {
					  	this.columnSelected--;
					  	if (this.itemSelected>=this.characterLeft.inventory.length) this.itemSelected = this.characterLeft.inventory.length-1;
					  	break;
					}
					if (keyPress(KEY_UP) && this.itemSelected>0) this.itemSelected--;
					if (keyPress(KEY_DOWN)) {
					  	if (this.itemSelected<this.characterRight.inventory.length-1 && this.itemSelected<this.maxItemsToDisplay-1) {
					      	this.itemSelected++;
					    } else {
					      	this.columnSelected = 3;
					    }
				    }
				  	if (keyPress(32) || keyPress(13)) {
				  		// try to buy something:
				  		if (this.characterRight.inventory[this.itemSelected].value>0) {
					  		this.transaction(game,
					  						 this.characterLeft, this.characterRight, 
					  						 this.characterRight.inventory[this.itemSelected],
					  						 this.characterRight.inventory[this.itemSelected].value);
				  		} else {
	  						game.pushMessage(this.characterRight.name + " doesn't want to give you that...");
					  	}
				  	}
				} else {
				    if (keyPress(KEY_UP)) {
				  		if (this.characterRight.inventory.length>0) {
				  			this.columnSelected = 2;
				  			this.itemSelected = Math.min(this.characterRight.inventory.length-1, this.maxItemsToDisplay-1);
				  		} else if (characterLeft.inventory.length>0) {
				  			this.columnSelected = 0;
				  			this.itemSelected = Math.min(this.characterLeft.inventory.length-1, this.maxItemsToDisplay-1);
				  		}
				  	}
				  	if (keyPress(32) || keyPress(13)) {
				    	this.state = 2;
				    	this.cycle = 0;
				  	}
				}

				// check mouse:
				var x1 = width/2 + this.centerx - this.menuWidth/2;
				var y1 = height/2 + this.centery-this.menuHeight/2;
				var cx = x1 + 8;
				var cy = y1 + TILE_HEIGHT + 24;
				for(var i = 0;i<this.characterLeft.inventory.length && i<this.maxItemsToDisplay;i++) {
					if (mouseState.x >= cx && 
						mouseState.x < cx + 148 && 
						mouseState.y >= cy + 12 + i*12 &&
						mouseState.y < cy + 12 + (i+1)*12) {
						this.columnSelected = 0;
						this.itemSelected = i;

						if (mouseState.button == 1 && mouseState.oldButton ==0) {
					  		// try to give something:
					  		this.transaction(game,
				  						 	 this.characterRight, this.characterLeft, 
					  						 this.characterLeft.inventory[this.itemSelected],
					  						 0);
						}
					}
					if (mouseState.x >= cx + 184 && 
						mouseState.x < cx + 244 && 
						mouseState.y >= cy + 12 + i*12 &&
						mouseState.y < cy + 12 + (i+1)*12) {
						this.columnSelected = 1;
						this.itemSelected = i;

						if (mouseState.button == 1 && mouseState.oldButton ==0) {
					  		// try to sell something:
					  		this.transaction(game,
				  						 	 this.characterRight, this.characterLeft, 
					  						 this.characterLeft.inventory[this.itemSelected],
					  						 this.characterLeft.inventory[this.itemSelected].value);
						}
					}
				}
				cx += 278;
				for(var i = 0;i<this.characterRight.inventory.length && i<this.maxItemsToDisplay;i++) {
					if (mouseState.x >= cx && 
						mouseState.x < cx + 148 && 
						mouseState.y >= cy + 12 + i*12 &&
						mouseState.y < cy + 12 + (i+1)*12) {
						this.columnSelected = 2;
						this.itemSelected = i;

						if (mouseState.button == 1 && mouseState.oldButton ==0) {
					  		// try to buy something:
					  		this.transaction(game,
				  						 	 this.characterLeft, this.characterRight, 
					  						 this.characterRight.inventory[this.itemSelected],
					  						 this.characterRight.inventory[this.itemSelected].value);
						}
					}
				}
		      	if (mouseState.x >= width/2 + this.centerx - 22 && 
		      		mouseState.x < width/2 + this.centerx + 22 && 
		      		mouseState.y >= y1 + this.menuHeight - 20 &&
		      		mouseState.y < y1 + this.menuHeight - 8) {
		      		this.columnSelected = 3;
		      		this.itemSelected = 0;

		      		if (mouseState.button == 1 && mouseState.oldButton ==0) {
		                this.state = 2;
		                this.cycle = 0;
		      		}
		      	}
	            break;
	      case 2:
	            if (this.cycle>this.speed*2) {
	            	return true;	// anything different than 'null' would do
	          	}
	    }
	    this.cycle++;
		return null;	
	}

	this.transaction = function(game, buyer, seller, item, price) {
		if (buyer.gold >= price) {
			if (buyer.inventory.length<INVENTORY_SIZE) {
				// create confirmation dialog:
				var msg = "";
				if (price==0) {
					msg = "Give " + item.name + " to " + buyer.name;
				} else if (seller == this.characterLeft) {
					msg = "Sell " + item.name + " for " + price;
				} else {
					msg = "Buy " + item.name + " for " + price;
				}
		      	var m = new Menu(-HUD_WIDTH/2,0,15.0,
		                            ["Cancel",msg],
		                            [[],[this,buyer,seller,item,price]],null);
			    m.effectFunction = function(game, map, character, value) {
			    						if (value.length>0) value[0].transactionPostConfirm(game,map,value[1],value[2],value[3],value[4]);
			    						return true;
			                       }
		      	game.pushMenu(m);
		      	this.columnSelected = 3;
		      	this.itemSelected = 0;
			} else {
				game.pushMessage(buyer.name + " has the inventory full...");
			}
		} else {
			game.pushMessage(buyer.name + " doesn't have enough gold...");
		}
	}

	this.transactionPostConfirm = function(game, map, buyer, seller, item, price) {
		if (price>0) {
			executeRuleEffect({action:"sell",inventory:item.name,newItem:null}, seller, map, game, buyer);
		} else {
			executeRuleEffect({action:"give",inventory:item.name,newItem:null}, seller, map, game, buyer);
		}
/*
		buyer.gold-=price;
		seller.gold+=price;
		buyer.inventory.push(item);
		seller.inventory.splice(seller.inventory.indexOf(item),1);
*/
	}	
}


