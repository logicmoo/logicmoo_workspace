using System;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using UnityEngine;

public class Sonifier : BindingBehaviour
{
    public bool Mute;
    public FMSynthesizer[] Patches;
    private int samplingRate=48000;

    private DumbGranularSynthesizer syn;

    AudioSource audioSource;
    GameObject pc;
#pragma warning disable 649
    [Bind] SimController simController;
#pragma warning restore 649
    bool running;

    internal void Start()
    {
        samplingRate = AudioSettings.outputSampleRate;
        syn = new DumbGranularSynthesizer(samplingRate, 0.5f);
        audioSource = GetComponent<AudioSource>();
        pc = GameObject.Find("pc");
    }

    internal void Update()
    {
        running = true;
        if (pc != gameObject)
        {
            UpdateVolumePan();
        }
    }

    private void UpdateVolumePan()
    {
        var offset = gameObject.Position() - pc.Position();
        var dist = offset.sqrMagnitude;
        dist *= dist;
        var volume = Math.Min(1, 10/dist);
        audioSource.panStereo = offset.normalized.x;
        audioSource.volume = volume;
    }

    int heartBeatPhase;
    System.Random random = new System.Random();

    void AddHeartBeat(float[] data)
    {
        int heartBeatPeriod = (int) (3000/(0.1f + 20*simController.Arousal));
        double variability = Math.Max(0, -simController.Valence);

        while (heartBeatPhase < data.Length - 1)
        {
            data[heartBeatPhase] = data[heartBeatPhase + 1] = 1;
            int spacing = (int) ((1 + ((random.NextDouble() - 0.5)*variability*2))*heartBeatPeriod);
            //int spacing = heartBeatPeriod;
            heartBeatPhase += 2*spacing;
            ;
        }
        heartBeatPhase -= data.Length;
    }

    public void EmitGrain(string patchName, int ms)
    {
        if (Mute)
            return;
        var patch = Patches.First(p => p.Name == patchName)??Patches[0];
        syn.EmitFMGrain(patch, 0.001f*ms);
        //syn.EmitRest(0.002f);
    }

    internal void OnAudioFilterRead(float[] data, int channels)
    {
        if (running)
        {
            // Can't do this until syn has been set up in the Start() routine
            if (syn != null)
            {
                if (!syn.IsEmpty)
                    syn.WriteBuffer(data);
                if (syn.IsEmpty)
                {
                    syn.Reset();
                }
            }

            AddHeartBeat(data);
        }
    }
}

internal class DumbGranularSynthesizer
{
    static readonly System.Random Rand = new System.Random();
    public float[] Data;

    public int SamplingRate=48000;

    private int nextIn;

    private int nextOut;

    public DumbGranularSynthesizer(int samplingRate, float duration)
    {
        this.SamplingRate = samplingRate;
        this.Data = new float[(int)(samplingRate * duration)];
    }

    public bool IsFull
    {
        get
        {
            return this.nextIn >= this.Data.Length;
        }
    }

    public bool IsEmpty
    {
        get
        {
            return nextOut == nextIn;
        }
    }

    public void Reset()
    {
        Array.Clear(this.Data, 0, this.nextIn);
        this.nextIn = this.nextOut = 0;
    }

    public void EmitNoiseGrain(int length, int dutyCycle, float amplitude)
    {
        var end = Math.Min(this.nextIn + dutyCycle, this.Data.Length);

        for (int i = this.nextIn; i < end; i++)
        {
            var phase = (float)i/dutyCycle;
            this.Data[i] = (float)(Rand.NextDouble() - 0.5) * amplitude * phase;
        }

        this.nextIn += length;
    }

    public void EmitFMGrain(FMSynthesizer syn, float time)
    {
        var length = (int)(time * this.SamplingRate);
        var realLength = Math.Min(length, this.Data.Length - this.nextIn);
        syn.GenerateGrain(this.Data, this.nextIn, realLength, this.SamplingRate);
        this.nextIn += realLength;
    }

    public void EmitRest(float time)
    {
        var length = (int)(time * this.SamplingRate);
        var realLength = Math.Min(length, this.Data.Length - this.nextIn);
        this.nextIn += realLength;
    }

    public void WriteBuffer(float[] buffer)
    {
        int samplesToCopy = Math.Min(buffer.Length/2, nextIn - nextOut);
        int bufferIndex = 0;
        for (int i = 0; i < samplesToCopy; i++)
        {
            var sample = this.Data[nextOut++];
            buffer[bufferIndex++] = sample;
            buffer[bufferIndex++] = sample;
        }
    }
}

[Serializable]
public class FMSynthesizer
{
    public string Name;

    public float CarrierFrequency = 100;

    public float ModulationFrequency = 10;

    public float ModulationLevel = 0;

    public float Amplitude = 1;

    public void GenerateGrain(float[] buffer, int start, int length, int samplingRate)
    {
        var carrierConstant = 2 * Math.PI * CarrierFrequency / samplingRate;
        var modulationConstant = 2 * Math.PI * ModulationFrequency / samplingRate;
        for (int i = 0; i < length; i++)
        {
            var sample = Amplitude*Math.Sin(i*carrierConstant+ModulationLevel*Math.Sin(i*modulationConstant));
            buffer[start + i] = (float)sample;
        }
    }
}
