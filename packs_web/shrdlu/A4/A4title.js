Game.drawTitle = function() {
  this.fadeOut>=this.fadeOutTime
  if (this.fadeOutTime>0) {
    var f = this.fadeOut/this.fadeOutTime;
    ctx.globalAlpha = 1 - f;
    if (this.titleIMG_bg!=null) ctx.drawImage(this.titleIMG_bg, 0, 0,width,height);
    return;
  }

  if (this.titleIMG_bg!=null) ctx.drawImage(this.titleIMG_bg, 0, 0, width, height);
  ctx.font = "11px Emulogic";  
  ctx.fillStyle = '#888888';  
  ctx.fillText("A4 Engine v1.2.2 - Santiago Ontanon (2014)", 165,580);
  if (this.titleMenu) this.titleMenu.draw();

  if (this.currentMenuID==1) {
    // draw the instructions:
    var f1 = this.titleMenu.f1;
    var f2 = this.titleMenu.f2*255;
    var h = this.instructions.length*18;

    ctx.fillStyle = '#000000';  
    ctx.strokeStyle = '#ffffff';  
    ctx.beginPath();  
    ctx.rect((width/2) - 296, (height/2) - (h/2 + 30), 592, (h+30)*f1);  
    ctx.closePath();  
    ctx.fill();  
    ctx.stroke();  

    if (f1==1.0) {
      var x = (width/2) - 288;
      var y = (height/2) - (h/2);
      ctx.font = "11px Emulogic";  
      ctx.fillStyle = "rgb(" + Math.floor(f2) + "," + Math.floor(f2) + "," + + Math.floor(f2) + ")";

      for(var i = 0;i<this.instructions.length;i++) {
        ctx.fillText(this.instructions[i], x,y); y+=18;
      }
    }
  }

  if (this.currentMenuID==2) {
    // draw the story:
    var f1 = this.titleMenu.f1;
    var f2 = this.titleMenu.f2*255;
    var h = this.story.length*18;

    ctx.fillStyle = '#000000';  
    ctx.strokeStyle = '#ffffff';  
    ctx.beginPath();  
    ctx.rect((width/2) - 296, (height/2) - (h/2 + 30), 592, (h+30)*f1);  
    ctx.closePath();  
    ctx.fill();  
    ctx.stroke();  

    if (f1==1.0) {
      var x = (width/2) - 288;
      var y = (height/2) - (h/2);
      ctx.font = "11px Emulogic";  
      ctx.fillStyle = "rgb(" + Math.floor(f2) + "," + Math.floor(f2) + "," + + Math.floor(f2) + ")";
      
      for(var i = 0;i<this.story.length;i++) {
        ctx.fillText(this.story[i], x,y); y+=18;
      }
    }
  }

};


Game.updateTitle = function() {
  if (this.state_cycle==0) {
    this.currentMenuID = 0;
    if (localStorage.getItem(this.gameName + "slot1")==null &&
        localStorage.getItem(this.gameName + "slot2")==null &&
        localStorage.getItem(this.gameName + "slot3")==null &&
        localStorage.getItem(this.gameName + "slot4")==null) {
      this.titleMenu = new Menu(0,200,15.0,["New Game","Instructions","Story"],[1,2,3],null);    
    } else {
      this.titleMenu = new Menu(0,200,15.0,["Continue","New Game","Instructions","Story"],[0,1,2,3],null);
    }
    this.fadeOut = 0;
    this.fadeOutTime = 0;
  }

  if (this.fadeOutTime>0) {
    this.fadeOut++;
    if (this.fadeOut>=this.fadeOutTime) {
        if (this.startNew) {
          this.resetGame();
        } else {
          this.loadGame(this.slotToLoad);          
        }
        return GAMESTATE_GAME;
    }
    return GAMESTATE_TITLE;
  }

  // update menu:
  var ret = this.titleMenu.update();

  var f = this.state_cycle/25.0;
  if (f>1) f = 1;
  ctx.globalAlpha = f;

  switch(ret) {
    case 0: // Load a previous game;
            var tmp1 = [];
            var tmp2 = [];
            if (localStorage.getItem(this.gameName + "slot1summary")!=null) {
              tmp1.push("save slot 1: " + this.getSaveGameSummary(1));
              tmp2.push(5);
            }
            if (localStorage.getItem(this.gameName + "slot2summary")!=null) {
              tmp1.push("save slot 2: " + this.getSaveGameSummary(2));
              tmp2.push(6);
            }
            if (localStorage.getItem(this.gameName + "slot3summary")!=null) {
              tmp1.push("save slot 3: " + this.getSaveGameSummary(3));
              tmp2.push(7);
            }
            if (localStorage.getItem(this.gameName + "slot4summary")!=null) {
              tmp1.push("save slot 4: " + this.getSaveGameSummary(4));
              tmp2.push(8);
            }
            tmp1.push("back");
            tmp2.push(4);
            this.titleMenu = new Menu(0,200,15.0,tmp1,tmp2,tmp1.length-1);
            this.currentMenuID = 0;
            break;
    case 1: // create a new game:
            this.fadeOut = 0;
            this.fadeOutTime = 25;
            this.startNew = true;
            break;
    case 2: this.titleMenu = new Menu(0,240,15.0,["Back"],[4],0);
            this.currentMenuID = 1;
            break;
    case 3: this.titleMenu = new Menu(0,240,15.0,["Back"],[4],0);
            this.currentMenuID = 2;
            break;
    case 4: 
            if (localStorage.getItem(this.gameName + "slot1")==null &&
                localStorage.getItem(this.gameName + "slot2")==null &&
                localStorage.getItem(this.gameName + "slot3")==null &&
                localStorage.getItem(this.gameName + "slot4")==null) {
              this.titleMenu = new Menu(0,200,15.0,["New Game","Instructions","Story"],[1,2,3],null);    
            } else {
              this.titleMenu = new Menu(0,200,15.0,["Continue","New Game","Instructions","Story"],[0,1,2,3],null);
            }
            this.currentMenuID = 3;
            break;
    case 5:
    case 6:
    case 7:
    case 8:
            this.fadeOut = 0;
            this.fadeOutTime = 25;
            this.startNew = false;
            this.slotToLoad = ret-4;
            break;

  }

  return GAMESTATE_TITLE;
};