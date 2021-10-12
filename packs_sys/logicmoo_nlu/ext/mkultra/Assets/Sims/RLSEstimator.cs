using System;

/// <summary>
/// Recursive least squares estimator.
/// </summary>
[Serializable]
public class RLSEstimator
{
    public RLSEstimator(float initialValue, float initialVariance, float lambda)
    {
        Value = initialValue;
        InitialVariance = initialVariance;
        Lambda = lambda;
    }
    public float Value;
    public float InitialVariance;
    public float Lambda;

    public void Update(float newValue, float newVariance)
    {
        var weightedVariance = Lambda*Lambda*InitialVariance;
        var gain = (weightedVariance)/(newVariance + weightedVariance);
        var weightedValue = Lambda*Value;
        Value = weightedValue + gain*(newValue - weightedValue);
        InitialVariance = newVariance*gain;
    }
}