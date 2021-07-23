function Animation() {
	this.dx = 1;
	this.dy = 1;
	this.period = 1;
	this.file = null;
	this.looping = false;
	this.tileEntry = undefined;
	this.sequence = [0];

	// reset the animation:
	this.cycle = 0;
	this.state = 0;
	this.completed = false;

	this.clone = function() {
		var a = new Animation();
		a.dx = this.dx;
		a.dy = this.dy;
		a.period = this.period;
		a.file = this.file;
		a.looping = this.looping;
		a.tileEntry = this.tileEntry;
		a.sequence = this.sequence;
		a.cycle = this.cycle;
		a.state = this.state;
		a.completed = this.completed;
		return a;
	}

	this.reset = function() {
		this.cycle = 0;
		this.state = 0;
	}

	this.update = function() {
		this.cycle++;
		if (this.cycle>=this.period) {
			this.cycle-=this.period;
			this.state++;
			if (this.state>=this.sequence.length) {
				if (this.looping) {
					this.state = 0;
				} else {
				 	this.state = this.sequence.length-1;
				 	this.completed = true;
				}
			}
		}
	}

	this.isCompleted = function() {
		return this.completed;
	}

	this.getWidth = function() {
		return this.dx;
	}

	this.getHeight = function() {
		return this.dy;
	}

	this.getTile = function() {
		return this.sequence[this.state];
	}

	this.getTileEntry = function(game) {
		if (this.tileEntry==undefined) {
			if (game==null) return;
			this.tileEntry = game.tiles[this.file]
		}
		return this.tileEntry;
	}


	this.draw = function(x,y,alpha,game) {
		if (this.tileEntry==undefined) {
			if (game==null) return;
			this.tileEntry = game.tiles[this.file];
		}
		if (this.sequence[this.state]!=-1) {
			for(var i = 0;i<this.dy;i++) {
				for(var j = 0;j<this.dx;j++) {
					drawTile(this.sequence[this.state]+j+i*this.tileEntry.tilesPerRow,x+j*TILE_WIDTH,y+i*TILE_HEIGHT,alpha,this.tileEntry);			
				}
			}
		}
	}

	this.drawRedTinted = function(x,y,alpha,game) {
		if (this.tileEntry==undefined) {
			if (game==null) return;
			this.tileEntry = game.tiles[this.file]
		}
		if (this.sequence[this.state]!=-1) {
			for(var i = 0;i<this.dy;i++) {
				for(var j = 0;j<this.dx;j++) {
					drawRedTintedTile(this.sequence[this.state]+j+i*this.tileEntry.tilesPerRow,x+j*TILE_WIDTH,y+i*TILE_HEIGHT,alpha,this.tileEntry);			
				}
			}
		}
	}

	this.generateSaveString = function() {
		return "Animation.fromSequence(["+this.sequence+"],"+this.period+","+this.looping+","+this.dx+","+this.dy+",\""+this.file+"\")";
	}

}

Animation.fromXML = function(xml) {
	var a = new Animation();
	a.dx = parseInt(xml.attributes.getNamedItem("dx").nodeValue);
	a.dy = parseInt(xml.attributes.getNamedItem("dy").nodeValue);
	a.period = 1;
	var tmp = xml.attributes.getNamedItem("period");
	if (tmp!=null) a.period = parseInt(tmp.nodeValue);
	a.file = xml.attributes.getNamedItem("file").nodeValue;
	a.looping = eval(xml.attributes.getNamedItem("looping").nodeValue);
	a.tileEntry = undefined;
	a.sequence = [];
	tmp = xml.textContent.split(",");
	for(var i = 0;i<tmp.length;i++) {
		a.sequence.push(parseInt(tmp[i]));
	}
	return a;
}


Animation.fromTile = function(tile,file) {
	var a = new Animation();
	a.file = file;
	a.sequence = [tile];
	return a;
}


Animation.fromTile2 = function(tile,file,dx,dy) {
	var a = new Animation();
	a.file = file;
	a.sequence = [tile];
	a.dx = dx;
	a.dy = dy;
	return a;
}


Animation.fromSequence = function(sequence, period, looping, dx, dy, file) {
	var a = new Animation();
	a.file = file;
	a.dx = dx;
	a.dy = dy;
	a.sequence = sequence;
	a.period = period;
	a.looping = looping;
	return a;	
}


function generateRedTintedImage(tileEntry) 
{
	var tmpCanvas = document.createElement('canvas');
	tmpCanvas.width = tileEntry.image.width;
	tmpCanvas.height = tileEntry.image.height;
//	console.log("Tmp canvas size: " + tmpCanvas.width + "," + tmpCanvas.height);
	var tmpContext = tmpCanvas.getContext('2d');
	tmpContext.drawImage(tileEntry.image, 0, 0 );
	var tmpData = tmpContext.getImageData(0, 0, tileEntry.image.width, tileEntry.image.height);
	for (var i=0;i<tmpData.data.length;i+=4)
	  {
	  if (tmpData.data[i+3]!=0) {
		  tmpData.data[i]=255;
		  tmpData.data[i+1]=0;
		  tmpData.data[i+2]=0;
		  tmpData.data[i+3]=255;
	  } else {
		  tmpData.data[i]=0;
		  tmpData.data[i+1]=0;
		  tmpData.data[i+2]=0;
		  tmpData.data[i+3]=0;
	  }
	}
	tmpContext.putImageData(tmpData,0,0);
	tileEntry.imageRedTinted = new Image();
	tileEntry.imageRedTinted.src = tmpCanvas.toDataURL("image/png");
	tileEntry.tilesPerRow = tileEntry.image.width/TILE_WIDTH_ORIGINAL;
	console.log("Finished loading image... " + tileEntry.image.src);
}


function drawTile(tile, x, y, alpha, tileEntry) {
//	if (tileEntry==undefined) tileEntry = Game.defaultTileEntry;
	if (tileEntry==undefined || tileEntry==null) return;
	var tmp = ctx.globalAlpha;
	var gridx = (tile%tileEntry.tilesPerRow)*TILE_WIDTH_ORIGINAL;
	var gridy = Math.floor(tile/tileEntry.tilesPerRow)*TILE_HEIGHT_ORIGINAL;

	ctx.globalAlpha *= alpha;
	ctx.drawImage(tileEntry.image, gridx, gridy, TILE_WIDTH_ORIGINAL, TILE_HEIGHT_ORIGINAL, x, y, TILE_WIDTH, TILE_HEIGHT);
	ctx.globalAlpha = tmp;
}

function drawRedTintedTile(tile, x, y, alpha, tileEntry) {
//	if (tileEntry==undefined) tileEntry = Game.defaultTileEntry;
	if (tileEntry==undefined || tileEntry==null) return;
	var tmp = ctx.globalAlpha;
	var gridx = (tile%tileEntry.tilesPerRow)*TILE_WIDTH_ORIGINAL;
	var gridy = Math.floor(tile/tileEntry.tilesPerRow)*TILE_HEIGHT_ORIGINAL;

	ctx.globalAlpha *= alpha;
	ctx.drawImage(tileEntry.imageRedTinted, gridx, gridy, TILE_WIDTH_ORIGINAL, TILE_HEIGHT_ORIGINAL, x, y, TILE_WIDTH, TILE_HEIGHT);
	ctx.globalAlpha = tmp;	
}
