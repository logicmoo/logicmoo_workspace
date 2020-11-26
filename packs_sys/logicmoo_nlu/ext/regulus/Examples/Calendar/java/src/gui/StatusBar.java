package gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.border.EtchedBorder;
import java.awt.Dimension;

class StatusBar extends JPanel {

    public final static Logger logger = Logger.getLogger(StatusBar.class.getPackage().getName());

    //private final Calendar calendarRootComponent;
    // private final CalendarConfiguration calendarConfig;

    private JLabel statusMessage = null;
    private JPanel appStateInfo = null;
    private JLabel appRecTimingInfoLabel = null;
    //private JLabel filler = null;

    /**
     * This is the default constructor
     */
    public StatusBar() {
        super();
        initialize();
    }



    private void initialize() {
        setBorder(BorderFactory.createEtchedBorder(EtchedBorder.RAISED));
        this.setLayout(new BorderLayout());
        getStatusMessageLabel().setText(" ");
        add(getStatusMessageLabel(), BorderLayout.WEST);

        appStateInfo = new JPanel();
        BoxLayout bl = new BoxLayout(appStateInfo, BoxLayout.X_AXIS);
        appStateInfo.setLayout(bl);
        
        appRecTimingInfoLabel = new JLabel("");
        setRecognitionTimings(0);
        this.setSize(new Dimension(320, 54));
        appRecTimingInfoLabel.setVerticalAlignment(SwingConstants.CENTER);
        appRecTimingInfoLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        appRecTimingInfoLabel.setToolTipText("Time it took to recognize the last utterance.");

        // appGrammarInfoLabel.setBorder(BorderFactory.createMatteBorder(0, 0, 0, 1, Color.BLACK));

        appStateInfo.add(Box.createHorizontalStrut(3), null);
        appStateInfo.add(new JSeparator(JSeparator.VERTICAL), null);
        appStateInfo.add(Box.createHorizontalStrut(3), null);
        appStateInfo.add(appRecTimingInfoLabel, null);
        
        appStateInfo.add(Box.createHorizontalStrut(3), null);
        appStateInfo.add(new JSeparator(JSeparator.VERTICAL), null);
        appStateInfo.add(Box.createHorizontalStrut(3), null);
        
        appStateInfo.add(new JSeparator(JSeparator.VERTICAL), null);
        appStateInfo.add(Box.createHorizontalStrut(3), null);
         
        appStateInfo.add(Box.createHorizontalStrut(3), null);
        appStateInfo.add(new JSeparator(JSeparator.VERTICAL), null);
        appStateInfo.add(Box.createHorizontalStrut(3), null);

        appStateInfo.add(Box.createHorizontalStrut(3), null);

        add(appStateInfo, BorderLayout.EAST);
        
           
    }

    /**
     * @param long The amount of time recognition took in msecs
     */
    public synchronized void setRecognitionTimings(long timingInfo) {
        if ( timingInfo <= 0 ){
            appRecTimingInfoLabel.setText("              ");
        }
        else{
            String finalText = Long.toString(timingInfo);
            finalText = finalText + " msec";
            int charsToTen = 14 - finalText.length();
            String padding = "";
            for (int i = 0; i < charsToTen; i++) {
                padding = padding + " ";
            }
            
            appRecTimingInfoLabel.setText(padding + finalText);
        }
    }
    
    /**
     * @param long The amount of time recognition took in msecs
     */
    public synchronized void setRecognitionTimings(float xCPURT) {
        if ( xCPURT <= 0 ){
            appRecTimingInfoLabel.setText("              ");
        }
        else{
            String finalText = Float.toString(xCPURT);
            finalText = finalText + " xCPU";
            int charsToTen = 14 - finalText.length();
            String padding = "";
            for (int i = 0; i < charsToTen; i++) {
                padding = padding + " ";
            }
            
            appRecTimingInfoLabel.setText(padding + finalText);
        }
    }


    public void setStatusMessage(String newMessage) {
        if (!Utils.isStringEmpty(newMessage)) {
            statusMessage.setText(newMessage);
        } else {
            statusMessage.setText(" ");
        }
    }

    public String getStatusMessage() {
        return getStatusMessageLabel().getText().trim();
    }

    /**
     * @return
     */
    private JLabel getStatusMessageLabel() {
        if (statusMessage == null) {
            statusMessage = new JLabel();
            statusMessage.setAlignmentX(Component.LEFT_ALIGNMENT);
            statusMessage.setHorizontalTextPosition(SwingConstants.LEFT);
        }
        return statusMessage;
    }

}  //  @jve:decl-index=0:visual-constraint="10,10"