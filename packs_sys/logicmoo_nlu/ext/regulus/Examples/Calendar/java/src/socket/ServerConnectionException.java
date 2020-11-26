package socket;

public class ServerConnectionException extends Exception {

	ServerConnectionException(String message, Exception cause){
		super(" ServerConnectionException: " + message, cause);
	}

	ServerConnectionException(String message){
		super(" ServerConnectionException: " + message);
	}
}

