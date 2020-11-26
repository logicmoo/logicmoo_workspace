package issco.calendar.socket;

public interface IProcessHandler {
	void startup() throws Exception;
	boolean shutdown() throws Exception;
}
