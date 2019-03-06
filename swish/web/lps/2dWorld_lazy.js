// used by the display page in lps_server_UI.pl; depends on 2dWorld.js
function sampler_for2d(ID,myWorld){
	var SAMPLE_URL = '/lps_server/d_sample/'+ID
	var self = {
		
		lastCycle: -1,
		isFirst: function(){
			return (self.lastCycle == -1);
		},
		load: function(){
			var isFirst = self.isFirst();
			// get timeless specifications only for the first time:
			var url = (isFirst?SAMPLE_URL+"?timeless=true":SAMPLE_URL);
			jQuery.ajax(url,{}).done(
				function(data){
					//console.log("data:"+JSON.stringify(data));
					jQuery(debug_output).text(JSON.stringify(data));
					self.lastCycle = data.cycle;
					var ops = self.assignOps(data.ops);
					console.log(JSON.stringify(ops));
					console.log("cycle:"+self.lastCycle);
					myWorld.displayFluentsForOne(ops);
					if (isFirst)
						myWorld.resizeWorld();
					myWorld.updatePaper();
				}
			).fail(
				function(data){
					self.last = null;
					console.log("failed:"+JSON.stringify(data));
				}
			);
		},
		
		// returns newProps with props wrapped in {Op:Props}, where Op is create/update/kill, depending
		// on whether the objects already exist in our 2d world
		assignOps: function(ops){
			if (!ops) 
				return [];
			var paperFluents = myWorld.getPaperFluents();
			var paperEvents =  myWorld.getPaperEvents();
			var myIDs = {};
			for (var i=0; i<ops.length; i++){
				var op = ops[i];
				var ID = op["id"];
				myIDs[ID] = true; // remember all in ops
				if (op["event"]){
					ops[i] = {create:op};
				} else { // fluent:
					if (paperFluents[ID])
						ops[i] = {update:ID, oldProps:{ /*we don't know them... but we don't really care */ }, newProps:op};
					else
						ops[i] = {create:op}; // new fluent
				}
			}
			// now kill old fluents absent from the new sample:
			for (ID in paperFluents){
				if (ID!=="timeless" && !myIDs[ID]) // timeless objects never die
					ops.push({kill:ID});
			}
			return ops;
		}
	}
	return self;
}
