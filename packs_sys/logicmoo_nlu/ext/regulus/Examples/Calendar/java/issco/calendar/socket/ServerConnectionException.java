package issco.calendar.socket;

public class ServerConnectionException extends Exception {

	ServerConnectionException(String message, Exception cause){
		// super(message, cause);
		System.out.println("Server connection exception because " + message);
	}

	ServerConnectionException(String message){
		// super(message);
		System.out.println("Server connection error " + message);
	}
}

