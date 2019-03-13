// used by the display page in lps_server_UI.pl; depends on 2dWorld.js
// TODO: the bulk of this file may want to miggrate to 2dWorld.js
function sampler_for2d(ID,myWorld){
	var SAMPLE_URL = '/lps_server/d_sample/'+ID
	var self = {
		
		lastCycle: -1,
		isFirst: true,
		load: function(events_for_lps){
			// get timeless specifications only for the first time:
			var url = (self.isFirst?SAMPLE_URL+"?timeless=true":SAMPLE_URL);
			//console.log("sampling "+url);
			//console.log("should POST "+JSON.stringify(events_for_lps));
			self.isFirst=false;
			jQuery.ajax(url,{
				method:"POST", contentType:"application/json", data: JSON.stringify({events:events_for_lps,cycle:self.lastCycle} )
				}).done(
				function(data){
					//console.log("data:"+JSON.stringify(data));
					var out = jQuery("#debug_output");
					if (out) 
						out.text(JSON.stringify(data));
					self.lastCycle = data.cycle;
					var ops = self.assignOps(data.ops);
					//console.log("cycle:"+self.lastCycle);
					myWorld.displayForOneCycle_lazy(ops,self.lastCycle);
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
			var isTimeless = myWorld.isTimeless;
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
			// now kill old fluents absent from the new sample; events are assumed atomic and thus killing by themselves a bit later
			for (ID in paperFluents){
				if (isTimeless(ID)) // timeless objects never die
					continue;
				if (!myIDs[ID]) 
					ops.push({kill:ID});
			}
			return ops;
		}
	}
	myWorld.setSampler(self);
	return self;
}
