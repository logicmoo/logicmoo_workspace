package shef.nlp.supple.category;

public class FeatureValue
{
	static final String unspec = "_";

	String feature;
	public String Feature() { return feature; }

	String value = unspec;
	public String Value() { return value; }

	public FeatureValue(String Feature) { feature = Feature; }

	public FeatureValue(String Feature, String Value)
	{
		feature = Feature;
		value = Value;
	}

	public void showFeatureValue()
	{
		System.out.println("Feature: "+this.feature+", Value: "+this.value);
	}
}