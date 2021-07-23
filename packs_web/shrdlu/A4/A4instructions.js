Game.drawInstructions = function() {
  this.drawGame();

  // draw the ending messages and menus
  if (this.endingMenu != null) {
    this.endingMenu.draw();
    var f1 = this.endingMenu.f1;
    var f2 = this.endingMenu.f2*255;
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
};



Game.updateInstructions = function() {
//  this.updateGame();

  if (this.state_cycle==0) {
    this.endingMenu = new Menu(0,230,15.0,["Continue"],[1],0);
  }

  // update menu:
  var ret = this.endingMenu.update();

  switch(ret) {
    case 1: // Keep playing
            return GAMESTATE_GAME;
            break;
  }


  return GAMESTATE_INSTRUCTIONS;
};
