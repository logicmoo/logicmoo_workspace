package issco.nbest;

public class Feature {
	int id;
	boolean used;
	String name;
	int weight;
	
	Feature(int id, boolean use_it, String name, int weight){
		this.id = id;
		this.used = use_it;
		this.name= name;
		this.weight = weight;
	}
	

	public boolean isUsed(){
		return used;
	}
	
	public int getID(){
		return id;
	}
	
	public void setUsed(Boolean isUsed){
		used = isUsed;
	}
}
