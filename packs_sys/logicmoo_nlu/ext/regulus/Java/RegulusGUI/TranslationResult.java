package RegulusGUI;

import se.sics.prologbeans.PBString;
import se.sics.prologbeans.Term;

/*
 * 
 * Used to return information from a translation operation. In the command-line 
 * version of the top-level, this information would be presented as in the
 * following example:
 * 
 * 
 * 

>> do you get headaches in the morning

>> do you get headaches in the evening

Source: do you get headaches in the evening+*no_preceding_utterance*
Target: ban atama wa itami masu ka
Other info:
n_parses = 1
parse_time = 0.047
source_representation = [[prep,in_time], [pronoun,you], [secondary_symptom,headache], 
                         [state,get_symptom], [tense,present], [time,evening], 
                         [utterance_type,ynq], [voice,active]]
source_discourse = [[prep,in_time], [pronoun,you], [symptom,headache], [state,have_symptom], 
                    [tense,present], [time,evening], [utterance_type,ynq], [voice,active]]
resolved_source_discourse = [[prep,in_time], [pronoun,you], [symptom,headache], 
                             [state,have_symptom], [tense,present], [time,evening], 
                             [utterance_type,ynq], [voice,active]]
resolution_processing = trivial
interlingua = [[prep,in_time], [pronoun,you], [state,have_symptom], [symptom,headache], 
               [tense,present], [time,evening], [utterance_type,ynq], [voice,active]]
interlingua_surface = YN-QUESTION have you headache in-time evening PRESENT ACTIVE
target_representation = [[body_part,atama], [path_proc,itamu], [temporal,ban], [tense,present],
                         [utterance_type,sentence]]
gloss_translation = night head TOPIC hurt POLITE-PRESENT Q
original_script_translation = ÈÕÆ¬¤ÏÄË¤ß¤Þ¤¹¤«
n_generations = 2
generation_time = 0.125
other_translations = [[ban,atama,no,hou,wa,itami,masu,ka]]
tagged_translations = [[*start*,ban/pp,atama/n,wa/wa_or_ga,[itami,masu]/v,ka/null,*end*], 
                       [*start*, ban/pp, [atama,no,hou]/n, wa/wa_or_ga, [itami,masu]/v, ka/null,
                        *end*]]
character_encoding = "JISAutoDetect"

 */

public class TranslationResult {
	
	private String source;
	private String target;
	private String n_parses;
	private String parse_time;
	private String source_representation;
	private String source_discourse;
	private String resolved_source_discourse;
	private String resolution_processing;
	private String interlingua;
	private String interlingua_surface;
	private String original_script_translation;
	private String character_encoding;
	private String gloss_translation;
	private String target_representation;
	private String n_generations;
	private String generation_time;
	private String other_translations;
	private String tagged_translations;
	private String to_source_discourse_trace;
	private String to_interlingua_trace;
	private String from_interlingua_trace;
	private String context;
	private String judgement;
	
	public String getSource() {
		return source;
	}
	
	public String getTarget() {
		return target;
	}
	
	public String getNParses() {
		return n_parses;
	}
	
	public String getParseTime() {
		return parse_time;
	}
	
	public String getSourceRepresentation() {
		return source_representation;
	}
	
	public String getSourceDiscourse() {
		return source_discourse;
	}
	
	public String getResolvedSourceDiscourse() {
		return resolved_source_discourse;
	}
	public String getResolutionProcessing() {
		return resolution_processing;
	}
	
	public String getInterlingua() {
		return interlingua;
	}
	
	public String getInterlinguaSurface() {
		return interlingua_surface;
	}
	
	public String getOriginalScriptTranslation() {
		return original_script_translation;
	}
	
	public String getCharacterEncoding() {
		return character_encoding;
	}
	
	public String getGlossTranslation() {
		return gloss_translation;
	}
	
	public String getTargetRepresentation() {
		return target_representation;
	}
	
	public String getNGenerations() {
		return n_generations;
	}
	
	public String getGenerationTime() {
		return generation_time;
	}
	
	public String getOtherTranslations() {
		return other_translations;
	}
	
	public String getTaggedTranslations() {
		return tagged_translations;
	}
	
	public String getToSourceDiscourseTrace() {
		return to_source_discourse_trace;
	}
	
	public String getToInterlinguaTrace() {
		return to_interlingua_trace;
	}
	
	public String getFromInterlinguaTrace() {
		return from_interlingua_trace;
	}
	
	public String getContext() {
		return context;
	}
	
	public String getJudgement() {
		return judgement;
	}
	
	public void setSource(String s) {
		source = s;
	}
	
	public void setTarget(String s) {
		target = s;
	}
	
	public void setNParses(String s) {
		n_parses = s;
	}
	
	public void setParseTime(String s) {
		parse_time = s;
	}
	
	public void setSourceRepresentation(String s) {
		source_representation = s;
	}
	
	public void setSourceDiscourse(String s) {
		source_discourse = s;
	}
	
	public void setResolvedSourceDiscourse(String s) {
		resolved_source_discourse = s;
	}
	public void setResolutionProcessing(String s) {
		resolution_processing = s;
	}
	
	public void setInterlingua(String s) {
		interlingua = s;
	}
	
	public void setInterlinguaSurface(String s) {
		interlingua_surface = s;
	}
	
	public void setOriginalScriptTranslation(String s) {
		original_script_translation = s;
	}
	
	public void setCharacterEncoding(String s) {
		character_encoding = s;
	}
	
	public void setGlossTranslation(String s) {
		gloss_translation = s;
	}
	public void setTargetRepresentation(String s) {
		target_representation = s;
	}
	
	public void setNGenerations(String s) {
		n_generations = s;
	}
	
	public void setGenerationTime(String s) {
		generation_time = s;
	}
	
	public void setOtherTranslations(String s) {
		other_translations = s;
	}
	
	public void setTaggedTranslations(String s) {
		tagged_translations = s;
	}
	
	public void setToSourceDiscourseTrace(String s) {
		to_source_discourse_trace = s;
	}
	
	public void setToInterlinguaTrace(String s) {
		to_interlingua_trace = s;
	}
	
	public void setFromInterlinguaTrace(String s) {
		from_interlingua_trace = s;
	}
	
	public void setContext(String s) {
		context = s;
	}
	
	public void setJudgement(String s) {
		judgement = s;
	}
	
	public void setKeyValue(String key, String value) {
		if ( key.equals("source") ) {
			setSource(value);
			}
		else if ( key.equals("target") ) {
			setTarget(value);
			}
		else if ( key.equals("n_parses") ) {	
			setNParses(value);
			}
		else if ( key.equals("parse_time") ) {
			setParseTime(value);
			}
		else if ( key.equals("source_representation") ) {
			setSourceRepresentation(value);
			}
		else if ( key.equals("source_discourse") ) {
			setSourceDiscourse(value);
			}
		else if ( key.equals("resolved_source_discourse") ) {
			setResolvedSourceDiscourse(value);
			}
		else if ( key.equals("resolution_processing") ) {
			setResolutionProcessing(value); 
			}
		else if ( key.equals("interlingua") ) {
			setInterlingua(value);
			}
		else if ( key.equals("interlingua_surface") ) {
			setInterlinguaSurface(value);
			}
		else if ( key.equals("original_script_translation") ) {
			setOriginalScriptTranslation(value);
			}
		else if ( key.equals("character_encoding") ) {
			setCharacterEncoding(value);
			}
		else if ( key.equals("gloss_translation") ) {
			setGlossTranslation(value);
			}
		else if ( key.equals("target_representation") ) {
			setTargetRepresentation(value);
			}
		else if ( key.equals("n_generations") ) {
			setNGenerations(value);
			}
		else if ( key.equals("generation_time") ) {
			setGenerationTime(value);
			}
		else if ( key.equals("other_translations") ) {
			setOtherTranslations(value); 
			}
		else if ( key.equals("tagged_translations") ) {
			setTaggedTranslations(value); 
			}
		else if ( key.equals("to_source_discourse_trace") ) {
			setToSourceDiscourseTrace(value);
			}
		else if ( key.equals("to_interlingua_trace") ) {
			setToInterlinguaTrace(value);
			}
		else if ( key.equals("from_interlingua_trace") ) {
			setFromInterlinguaTrace(value);
			}
		else if ( key.equals("context") ) {
			setContext(value);
			}
		else if ( key.equals("judgement") ) {
			setJudgement(value);
			}
		}
	
	public String toString() {
		String s = "";
		
		s += "\nsource:\n";
		s += getSource();
			
		s += "\n\ncontext:\n";
		s += getContext();
		
		s += "\n\ntarget:\n";
		s += getTarget();
			
		s += "\n\nn_parses:\n";	
		s += getNParses();

		s += "\n\nparse_time:\n";
		s += getParseTime();

		s += "\n\nsource_representation:\n";
		s += getSourceRepresentation();
		
		s += "\n\nto_source_discourse_trace:\n";
		s += getToSourceDiscourseTrace();

		s += "\n\nsource_discourse:\n";
		s += getSourceDiscourse();

		s += "\n\nresolved_source_discourse:\n";
		s += getResolvedSourceDiscourse();

		s += "\n\nresolution_processing:\n";
		s += getResolutionProcessing(); 

		s += "\n\nto_interlingua_trace:\n";
		s += getToInterlinguaTrace();

		s += "\n\ninterlingua:\n";
		s += getInterlingua();

		s += "\n\ninterlingua_surface:\n";
		s += getInterlinguaSurface();

		s += "\n\nfrom_interlingua_trace:\n";
		s += getFromInterlinguaTrace();
		
		s += "\n\ntarget_representation:\n";
		s += getTargetRepresentation();

		s += "\n\noriginal_script_translation:\n";
		s += getOriginalScriptTranslation();

		s += "\n\ncharacter_encoding:\n";
		s += getCharacterEncoding();

		s += "\n\ngloss_translation:\n";
		s += getGlossTranslation();

		s += "\n\nn_generations:\n";
		s += getNGenerations();

		s += "\n\ngeneration_time:\n";
		s += getGenerationTime();

		s += "\n\nother_translations:\n";
		s += getOtherTranslations(); 

		s += "\n\ntagged_translations:\n";
		s += getTaggedTranslations(); 
		
		s += "\n\njudgement:\n";
		s += getJudgement(); 

		return s;
	}
	
	public String toStringForJudgements() {
		String s = "translation(";

		s += "'" + getSource().replaceAll("'", "\\\\'") + "',";
		
		s += "'" + getTarget().replaceAll("'", "\\\\'") + "',";
			
		s += "[context='" + getContext().replaceAll("'", "\\\\'") + "'],";
		
		s += "'" + getJudgement() + "'";
		
		s += ")";

		return s;
	}
	
	public boolean initFromProlog(Term prologItem) {
		if ( prologItem!= null && prologItem.isCompound() ) {
			int arity = prologItem.getArity();
			for ( int i = 1; i <= arity; i++ ) {
				Term arg = prologItem.getArgument(i);
				if ( arg != null && arg.isCompound() && arg.getArity() == 2 ) {
					String key = unpackPBString(arg.getArgument(1));
					String value = unpackPBString(arg.getArgument(2));
					setKeyValue(key, value);
				}
			}
			return true;
		}
		else {
			return false;
		}
	}
	
	private static String[][] conversionTable = 
		{
			{"NL", "\n"},
			{"a1", "á"},
		    {"a2", "â"},
			{"a3", "à"},
			{"a4", "ä"},
			{"a5", "å"},

			{"c1", "ç"},

			{"e1", "é"},
			{"e2", "ê"},
			{"e3", "è"},
			{"e4", "ë"},
			{"e6", "æ"},

			{"i1", "í"},
			{"i2", "î"},
			{"i3", "ì"},
			{"i4", "ï"},

			{"n1", "ñ"},

			{"o1", "ó"},
			{"o2", "ô"},
			{"o3", "ò"},
			{"o4", "ö"},

			{"u1", "ú"},
			{"u2", "û"},
			{"u3", "ù"},
			{"u4", "ü"},
		};
	
	private static String unpackPBString(Term PBStringTerm) {
		if ( PBStringTerm == null ) {
			return null;
		}
		else if ( !PBStringTerm.isString() ) {
			return PBStringTerm.toString();
		}
		else {
			String s = ((PBString) PBStringTerm).getString();
			if ( s.equals("*empty_string*") ) {
				return "";
			}
			else {
				String s1;
				for ( int i = 0; i < conversionTable.length; i++ ) {
					String fromString = "!" + conversionTable[i][0] + "!";
					String toString = conversionTable[i][1];
					s1 = s.replaceAll(fromString, toString);
					s = s1;
				}
				return s;
			}
		}
	}
}
