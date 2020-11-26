package socket;

import static org.junit.Assert.*;
import gui.ApplicationState;
import gui.CalendarConfiguration;

import java.util.logging.Level;

import org.junit.Test;
/**
 * For this class to execute, an "calendar.prop" file must be in the  
 * "D:/Workspace/Calendar/calendar.prop" directory.
 */
public class DialogueServerHandlerTest {
	String configFile = "D:/Workspace/Calendar/calendar.prop";	
	CalendarConfiguration calendarConfig = new CalendarConfiguration(configFile);
	ApplicationState calendarApplicationState = new ApplicationState(calendarConfig);
	DialogueServerHandler dialogServer = new DialogueServerHandler(calendarConfig, calendarApplicationState);			


	@Test
	public final void testStartup() {
		// debug only 
		System.out.println("Dialog server command: " + calendarConfig.getDialogServerCommand());
				try{
			dialogServer.startup();
		}			
		catch(Exception e){
			System.out.println( "Couldn't create dialog server process! ");
		}
		
        try{
        	ApplicationState newApplicationState = new ApplicationState(calendarApplicationState);
        	newApplicationState.setLanguageIndex(1);	               
        	try{
        		dialogServer.reconfigure(newApplicationState);
        	}
        	catch(Exception e){
        		System.out.println("Dialog server was NOT reconfigured ! ");
        	}
        }
        catch(Exception ex){
        	ex.printStackTrace();
        };
	}
	
	@Test
	public final void testShutdown() {		
		try{
			dialogServer.shutdown();
		}
		catch(Exception e){
			System.out.println("Could shutdown dialog server process! ");
		}
	}
	

}
