package socket;

import static org.junit.Assert.*;

import org.junit.Test;

public class DialogueHelpResultTest {

	@Test
	public final void testMain() {
		DialogueHelpResult test = new DialogueHelpResult("help('who was at the last meeting\nwhere was the last meeting\nwhen was the last meeting in geneva\nwhen was the last meeting in england\nwhen was the last meeting').");
		java.util.Vector<String> it = test.helpSentList;
		System.out.println("it = " + it);
		int i = 0;
		for (String helpSent : it){
			System.out.println("i = " + i);
			if (i == 1){
				System.out.println("i = " + i);
				assertEquals("where was the last meeting"+"bla bla", helpSent);
				
			}
			System.out.println(helpSent);
			i++;
		}
	}

}
