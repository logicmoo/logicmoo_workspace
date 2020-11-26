package socket;

public class ProcessHandler {

	static boolean isLibraryLoaded = false;

	private native long _startProcess(String process);
	private native boolean _stopProcess(long pID);

	private final String nativeLib;

	public ProcessHandler(String nativeLib){
		this.nativeLib = nativeLib;
		
		System.loadLibrary(nativeLib);
		isLibraryLoaded = true;
	}

	public boolean stopProcess(long processID){
		return _stopProcess(processID);
	}
	
	public long startProcess(String process){
		return _startProcess(process);
	}
}
