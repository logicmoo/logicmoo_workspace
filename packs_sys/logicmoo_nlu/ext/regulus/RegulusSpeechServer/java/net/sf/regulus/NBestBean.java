package net.sf.regulus;

public class NBestBean{
	private String recognition;
	private String confidence;
	private String value;
	
	public NBestBean(String score, String trans, String val){
		this.confidence = score;
		this.recognition = trans;
		this.value = val;
			
	}
	
	public String getRec(){
		return recognition;
	}
	public String getConfidence(){
		return confidence;
	}
	public String getValue(){
		return value;
	}

}
