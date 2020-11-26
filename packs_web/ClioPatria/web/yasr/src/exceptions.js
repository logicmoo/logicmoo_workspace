module.exports = {
	GoogleTypeException: function(foundTypes, varName) {
		this.foundTypes = foundTypes;
		this.varName = varName;
		this.toString = function() {
			var string = 'Conflicting data types found for variable ' + this.varName + '. Assuming all values of this variable are "string".';
			string += ' To avoid this issue, cast the values in your SPARQL query to the intended xsd datatype';

			return string;
		};
		this.toHtml = function() {
			var string = 'Conflicting data types found for variable <i>' + this.varName + '</i>. Assuming all values of this variable are "string".';
			string += ' As a result, several Google Charts will not render values of this particular variable.';
			string += ' To avoid this issue, cast the values in your SPARQL query to the intended xsd datatype';

			return string;
		};
	}
}