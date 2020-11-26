package issco.calendar.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.Font;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import java.awt.Dimension;
import javax.swing.JList;
import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;

import net.sf.regulus.NBestBean;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;

/**
 * Implements the GUI components.
 *
 */
public class CalendarPanel extends JPanel {
    private static final long serialVersionUID = 9100232634579182486L;
    private static Logger logger = Logger.getLogger(CalendarPanel.class.getName());  

    private final Calendar calendarRootComponent;
    private final CalendarConfiguration calendarConfig;

    private static final String CMD_START_RECOGNITION = "start_recognition";  //  @jve:decl-index=0:
    private static final String CMD_ABORT_RECOGNITION = "abort_recognition";  //  @jve:decl-index=0:

    private static final String CMD_REMOVE_CONTEXT = "action(return_to_initial_context).";  //  @jve:decl-index=0:
    private static final String OK_RESPONSE_REMOVE_CONTEXT = "ok";  //  @jve:decl-index=0:
    
    private JButton jButtonStartRecognition = null;
    
    public boolean propagateHelpJudgmentChangeEvent = true;
    
    private JLabel jLabelHelpSentences = null;
    private JScrollPane jScrollPaneTalkBack = null;
    /** used to synchronize access to jLabelHelpSentences */
    // private Object helpSynchObject = null;
    /** used to synchronize enabling/disabling of buttons */
    private Object buttonSynchObject = new Object();
    
	private JScrollPane jScrollPaneRecognition1 = null;
	private  JTextArea textAreaSelectedInterpretation = null;
	private  JTextArea textAreaDialogueResponse = null;
	//private Box msgFromDS = null;
	private  JTextArea textAreaMsg4DialServ1 = null;
	private JScrollPane jScrollPaneDialogueContext = null;
	private  JTextArea textAreaNBestRecResult1 = null;
	private JButton jButtonRemoveContext = null;
	private JScrollPane jScrollPaneHelpSystem = null;
	private JList jListHelpSystem = null;
	private JScrollPane jScrollPaneRawRecResult = null;
	private JButton jButtonTest = null;
	private JScrollPane jScrollPaneAnswer = null;
	private JTextArea jTextAreaDialogueHistory = null;
	private JTable jTableRawRecResult = null;
	private TableColumn jTableColumnScore = null;  //  @jve:decl-index=0:
	private TableColumn jTableColumnTrans = null;  //  @jve:decl-index=0:
	private TableColumn jTableColumnValue = null;  //  @jve:decl-index=0:
	private JPanel jPanelButtons = null;
	private JPanel jPanelResults = null;
	/**
     * This is the default constructor
     */
	
	protected static long REC_DURATION;   
	
    public CalendarPanel(final Calendar calendarRootComponent, CalendarConfiguration calendarConfig) {
        super();
		
        this.calendarRootComponent = calendarRootComponent;
        this.calendarConfig = calendarConfig;
        this.buttonSynchObject = new Object();

        initialize();
        // init();
    }

    /**
	 * This method initializes this
	 * 
	 */
	private void initialize() {
        BoxLayout boxLayout = new BoxLayout(this, BoxLayout.Y_AXIS);
        this.setLayout(boxLayout);
        this.setAlignmentY(Component.LEFT_ALIGNMENT);

        this.setSize(new Dimension(568, 729));
        this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        
        this.add(Box.createHorizontalStrut(7));
        this.add(Box.createVerticalStrut(7));
        this.add(Box.createVerticalGlue(), null);
        this.add(Box.createHorizontalGlue(), null);
        
        
        this.add(getJPanelButtons(), null);
        this.add(Box.createRigidArea(new Dimension(50,0)));
        this.add(getJPanelResults(), null);

        setRecognitionInProgress(false);
              		
	}


    /**
     * This method initializes jButtonStartRecognition
     * 
     * @return javax.swing.JButton
     */
    private javax.swing.JButton getJButtonStartRecognition() {
    	if (jButtonStartRecognition == null) {
            jButtonStartRecognition = new javax.swing.JButton();
            jButtonStartRecognition.setText("Start Recognition ");
            jButtonStartRecognition.setToolTipText("Utter your question after recognition has been initiated");
            jButtonStartRecognition.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    recognitionButtonPressed(e);
                }
            });
 
        }
        return jButtonStartRecognition;
    }

    public void setRecognitionButtonEnabled(boolean b) {
        synchronized(buttonSynchObject){
            jButtonStartRecognition.setEnabled(b);
        }
    }

    private void recognitionButtonPressed(ActionEvent e) {   	
        if ((e != null) && (e.getActionCommand() != null)) {
            if (e.getActionCommand().equals(CMD_START_RECOGNITION)) {
                calendarRootComponent.processRecognitionRequest();
            }
            else if (e.getActionCommand().equals(CMD_ABORT_RECOGNITION)) {
                calendarRootComponent.processRecognitionAbortRequest();
            }
        }
    }

    public void setRecognitionInProgress(boolean isInProgress) {
        synchronized(buttonSynchObject){
	        if (isInProgress) {
	            jButtonStartRecognition.setText("Abort Recognition");
	            jButtonStartRecognition.setActionCommand(CMD_ABORT_RECOGNITION);	                            
	        }
	        else {
	            jButtonStartRecognition.setText("Start Recognition");
	            jButtonStartRecognition.setActionCommand(CMD_START_RECOGNITION);
             }
        }
    }


/**
 * 
 * @param nbestArray
 */   
    public void setRawResultTable(java.util.ArrayList<NBestBean> nbestArray) {
        if (nbestArray != null) {
        	String textToShow = "";
        	java.util.Iterator it = nbestArray.iterator();
        	int i = 0;
    		while(it.hasNext()){
    			NBestBean nbest = (NBestBean)it.next();  			    			
    			textToShow = textToShow + nbest.getConfidence() + "\n";
    			textToShow = textToShow + nbest.getRec() + "\n";
    			textToShow = textToShow + nbest.getValue() + "\n";
    			
				TableModel model =getJTableRawRecResult().getModel();
				model.setValueAt(nbest.getConfidence(), i, 0);
				model.setValueAt(nbest.getRec(), i, 1);
				model.setValueAt(nbest.getValue(), i, 2);
				
				i++;
    		}
            // textAreaNBestRecResult.setText(textToShow);
        }
        else {
        	TableModel model =getJTableRawRecResult().getModel();
        	for (int i = 0; i< 6; i++){
        		model.setValueAt("", i, 0);
        		model.setValueAt("", i, 1);
        		model.setValueAt("", i, 2);
        	}
        }
    }
       
    public void setTextAreaSelectedInterpretation(String text) {
        if (text != null) {
            textAreaSelectedInterpretation.setText(text);
        }
        else {
        	textAreaSelectedInterpretation.setText("?");
        }
    }
    
    public void setMsgFromDialServTextArea(String text) {
        if (text != null) {
            textAreaDialogueResponse.setText(text);
        }
        else {
        	textAreaDialogueResponse.setText("?");
        }
    }
    
    /**
     * Add a new element (of type String) in the list 
     */
        public void addTextToDialogueHistoryTextArea(String text) {
            if (text != null) {            	
            	jTextAreaDialogueHistory.append(text +"\n");
            	jTextAreaDialogueHistory.setCaretPosition(jTextAreaDialogueHistory.getText().length());
            }
            
        }

    /**
     * Add a new element (of type String) in the list 
     */
        public void addTextToHelpJList(java.util.ArrayList<String> helpSentList) {
            if (helpSentList != null) {
            	int currentListSize = jListHelpSystem.getModel().getSize();
            	
            	Object[] newListData = new Object[currentListSize + helpSentList.size() + 1];
            	int i = 0;
            	java.util.Iterator it = helpSentList.iterator();
        		while(it.hasNext()){
        			String tmp = (String)it.next();
        			newListData[i] = tmp;
        			i++;
        		}
        		int noNewElems = i;
        		
        		newListData[i] = "------------";
        		i++;
            	for (; i< (currentListSize+noNewElems+1); i++){
            		newListData[i] = jListHelpSystem.getModel().getElementAt(i-noNewElems-1);            		
            	}
            	
            	jListHelpSystem.setListData(newListData);            	
            }
            else{
            	String[] newListData = new String[1];
            	newListData[0] = "?";
            	jListHelpSystem.setListData(newListData);
            }
            
        }


        public void setTextForHelpJList(java.util.ArrayList<String> helpSentList) {
            if (helpSentList != null) {
           	
            	Object[] newListData = new Object[helpSentList.size()];
            	int i = 0;
            	java.util.Iterator it = helpSentList.iterator();
        		while(it.hasNext()){
        			String tmp = (String)it.next();
        			newListData[i] = tmp;
        			i++;
        		}
        		          	
            	jListHelpSystem.setListData(newListData);            	
            }
            else{
            	String[] newListData = new String[1];
            	newListData[0] = "?";
            	jListHelpSystem.setListData(newListData);
            }
            
        }

    /**
     * Add a new element (of type String) in the list 
     */
        public void removeContentDialogueContextJList() {
            //Object[] newListString = new Object[0];
           	//jListDialogueContext.setListData(newListString);
        }
                    
    
    /**
     * Add a new element (of type String) in the list 
     */
        public void addTextToHelpSystemJList(String text) {
            if (text != null) {
            	int currentListSize = jListHelpSystem.getModel().getSize();
            	
            	Object[] newListString = new Object[currentListSize + 1];
            	for (int i = 0; i< currentListSize; i++){
            		newListString[i] = jListHelpSystem.getModel().getElementAt(i);
            	}
            	newListString[currentListSize] = text;
            	jListHelpSystem.setListData(newListString);
            	//add(text);
            }
            
        }
       
     
	/**
	 * This method initializes textAreaSelectedInterpretation	
	 * 	
	 * @return issco.regulus.gui.CustomTextArea	
	 */
	private  JTextArea getTextAreaSelectedInterpretation() {
		if (textAreaSelectedInterpretation == null) {
			textAreaSelectedInterpretation = new JTextArea(3, 20);
			textAreaSelectedInterpretation.setEditable(false);
			textAreaSelectedInterpretation.setLineWrap(true);
			textAreaSelectedInterpretation.setWrapStyleWord(true);	
			textAreaSelectedInterpretation.setFont(new Font("Serif", Font.TRUETYPE_FONT, 16));
		}
		return textAreaSelectedInterpretation;
	}


	/**
	 * This method initializes textAreaDialogueResponse	
	 * 	
	 * @return issco.regulus.gui.JTextArea	
	 */
	private JTextArea getTextAreaDialogueResponse() {
		if (textAreaDialogueResponse == null) {
			textAreaDialogueResponse = new JTextArea(3,50);
			textAreaDialogueResponse.setEditable(false);
			textAreaDialogueResponse.setLineWrap(true);
			textAreaDialogueResponse.setWrapStyleWord(true);
			textAreaDialogueResponse.setFont(new Font("Serif", Font.BOLD, 16));

		}
		return textAreaDialogueResponse;
	}

	/**
	 * This method initializes jScrollPaneDialogueContext	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	protected JScrollPane getJScrollPaneDialogueContext() {
		if (jScrollPaneDialogueContext == null) {
			jScrollPaneDialogueContext = new JScrollPane();
			jScrollPaneDialogueContext.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Dialogue History : "));
			jScrollPaneDialogueContext.setViewportView(getJTextAreaDialogueHistory());
			// jScrollPaneDialogueContext.getViewport().setViewPosition(0);			 
		}
		return jScrollPaneDialogueContext;
	}

	/**
	 * This method initializes jButtonRemoveContext	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getJButtonRemoveContext() {
		if (jButtonRemoveContext == null) {
			jButtonRemoveContext = new JButton();
			jButtonRemoveContext.setText("Remove Context");	
			jButtonRemoveContext.addMouseListener(new java.awt.event.MouseAdapter() {
				public void mouseClicked(java.awt.event.MouseEvent e) {
					String dialogServerReply = calendarRootComponent.sendRequestToDialogueServer(CMD_REMOVE_CONTEXT);
					if (dialogServerReply.indexOf(OK_RESPONSE_REMOVE_CONTEXT) > -1){
						logger.info("Dialogue context removed. ");
					}
				}
			});
		}
		return jButtonRemoveContext;
	}

	/**
	 * This method initializes jScrollPaneHelpSystem	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getJScrollPaneHelpSystem() {
		if (jScrollPaneHelpSystem == null) {
			jScrollPaneHelpSystem = new JScrollPane();
			jScrollPaneHelpSystem.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Help System : "));
			jScrollPaneHelpSystem.setViewportView(getJListHelpSystem());
		}
		return jScrollPaneHelpSystem;
	}

	/**
	 * This method initializes jListHelpSystem	
	 * 	
	 * @return javax.swing.JList	
	 */
	private JList getJListHelpSystem() {
		if (jListHelpSystem == null) {
			jListHelpSystem = new JList();
			jListHelpSystem.setFont(new Font("Serif", Font.TRUETYPE_FONT, 16));
			jListHelpSystem
					.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
						public void valueChanged(javax.swing.event.ListSelectionEvent e) {
							//System.out.println(" jListHelpSystem valueChanged()"); // TODO Auto-generated Event stub valueChanged()
						}
					});
		}
		return jListHelpSystem;
	}

	/**
	 * This method initializes jScrollPaneRawRecResult	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	protected JScrollPane getJScrollPaneRawRecResult() {
		if (jScrollPaneRawRecResult == null) {
			jScrollPaneRawRecResult = new JScrollPane();
			jScrollPaneRawRecResult.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Raw Recognition Result : "));
			
			jScrollPaneRawRecResult.setViewportView(getJTableRawRecResult());
			jScrollPaneRawRecResult.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);			
		}
		return jScrollPaneRawRecResult;
	}

	
	/**
	 * This method initializes jScrollPaneAnswer	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getJScrollPaneAnswer() {
		if (jScrollPaneAnswer == null) {
			jScrollPaneAnswer = new JScrollPane();
			jScrollPaneAnswer.setViewportView(getTextAreaDialogueResponse());
			jScrollPaneAnswer.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Answer : "));
			// jScrollPaneAnswer.setPreferredSize(new Dimension());
//			jScrollPaneAnswer.setVerticalScrollBarPolicy( JScrollPane.VERTICAL_SCROLLBAR_ALWAYS );
		}
		return jScrollPaneAnswer;
	}

	
	/**
	 * This method initializes jTextAreaDialogueHistory	
	 * 	
	 * @return javax.swing.JTextArea	
	 */
	private JTextArea getJTextAreaDialogueHistory() {
		if (jTextAreaDialogueHistory == null) {
			jTextAreaDialogueHistory = new JTextArea(5,50);
			jTextAreaDialogueHistory.setEditable(false);
		}
		return jTextAreaDialogueHistory;
	}

	/**
	 * This method initializes jTableRawRecResult	
	 * 	
	 * @return javax.swing.JTable	
	 */
	private JTable getJTableRawRecResult() {
		if (jTableRawRecResult == null) {
			jTableRawRecResult = new JTable();
			jTableRawRecResult.setRowHeight(30);
			jTableRawRecResult.addColumn(getTableColumnScore()); 
			jTableRawRecResult.addColumn(getTableColumnTrans());
			jTableRawRecResult.addColumn(getTableColumnValue());
			
			jTableRawRecResult.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
			
			jTableRawRecResult.setAutoCreateColumnsFromModel(false);			
			jTableRawRecResult.setModel(new RawRecResListModel());
			
		}
		return jTableRawRecResult;
	}

	/**
	 * This method initializes ivjTableColumn
	 * 
	 * @return table.TableColumn
	 */
	private TableColumn getTableColumnScore() {
		if (jTableColumnScore == null) {
			jTableColumnScore = new TableColumn(); 
			jTableColumnScore.setHeaderValue("Confidence");
			jTableColumnScore.setMaxWidth(100);
			jTableColumnScore.setResizable(true); 
			jTableColumnScore.setModelIndex(0);
			
		}
		return jTableColumnScore;
	}

	/**
	 * This method initializes ivjTableColumn
	 * 
	 * @return table.TableColumn
	 */
	private TableColumn getTableColumnTrans() {
		if (jTableColumnTrans == null) {
			jTableColumnTrans = new TableColumn(); 
			jTableColumnTrans.setHeaderValue("Transcription");
			jTableColumnTrans.setPreferredWidth(400);
			// jTableColumnTrans.setMinWidth(400);
			
			jTableColumnTrans.setResizable(true); 
			jTableColumnTrans.setModelIndex(1);
		}
		return jTableColumnTrans;
	}

	/**
	 * This method initializes ivjTableColumn
	 * 
	 * @return table.TableColumn
	 */
	private TableColumn getTableColumnValue() {
		if (jTableColumnValue == null) {
			jTableColumnValue = new TableColumn(); 
			jTableColumnValue.setHeaderValue("Logic Form ");
			jTableColumnValue.setPreferredWidth(1000);
			
			jTableColumnValue.setResizable(true); 
			jTableColumnValue.setModelIndex(2);
		}
		return jTableColumnValue;
	}
	
	/**
	 * Static inner class for the table model
	 */
	public static class RawRecResListModel extends AbstractTableModel {
		
		Object[][] data = { { "", "", "" }, { "", "", "" }, { "", "", "" },
				{ "", "", "" },{ "", "", "" },	{ "", "", "" }};

		public Object getValueAt(int rowIndex, int columnIndex) {
			return data[rowIndex][columnIndex];
		}

		public int getColumnCount() {
			return 3;
		}

		public int getRowCount() {
			return 6;
		}

		public String getColumnName(int c) {
			return ("");
		}

		public void setValueAt(Object arg0, int arg1, int arg2) {
			
			if (arg1 > data.length){
				Object[][] tmpData = new Object[arg1][data[0].length];
				data = tmpData;
			}			
			data[arg1][arg2] = arg0;
			fireTableDataChanged();
		}
	}
	
	/**
	 * This method initializes jPanelButtons	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanelButtons() {
		if (jPanelButtons == null) {
			jPanelButtons = new JPanel();			
			jPanelButtons.setLayout(new GridBagLayout());
			jPanelButtons.setMaximumSize(new Dimension(this.getMaximumSize().width, 100));
			jPanelButtons.add(getJButtonStartRecognition(), new GridBagConstraints());
			jPanelButtons.add(Box.createRigidArea(new Dimension(100,0)));
			jPanelButtons.add(getJButtonRemoveContext(), new GridBagConstraints());
		}
		return jPanelButtons;
	}

	/**
	 * This method initializes jPanelResults	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanelResults() {
		if (jPanelResults == null) {
			GridBagConstraints gridBagConstraints1 = new GridBagConstraints();
			gridBagConstraints1.fill = GridBagConstraints.BOTH;
			gridBagConstraints1.weighty = 0.1;
			gridBagConstraints1.weightx = 1.0;
			gridBagConstraints1.gridx = 0;
			gridBagConstraints1.gridy = 0;

			GridBagConstraints gridBagConstraints2 = new GridBagConstraints();
			gridBagConstraints2.fill = GridBagConstraints.BOTH;
			gridBagConstraints2.weighty = 0.1;
			gridBagConstraints2.weightx = 1.0;
	        gridBagConstraints2.gridx = 0;
			gridBagConstraints2.gridy = 1;

			GridBagConstraints gridBagConstraints3 = new GridBagConstraints();
			gridBagConstraints3.fill = GridBagConstraints.BOTH;
			gridBagConstraints3.weighty = 0.6;
			gridBagConstraints3.weightx = 1.0;
			gridBagConstraints3.gridx = 0;
			gridBagConstraints3.gridy = 2;

			GridBagConstraints gridBagConstraints4 = new GridBagConstraints();
			gridBagConstraints4.fill = GridBagConstraints.BOTH;
			gridBagConstraints4.weighty = 0.9;
			gridBagConstraints4.weightx = 1.0;
			gridBagConstraints4.gridx = 0;
			gridBagConstraints4.gridy = 3;


			GridBagConstraints gridBagConstraints5 = new GridBagConstraints();
			gridBagConstraints5.fill = GridBagConstraints.BOTH;
			gridBagConstraints5.weighty = 1.0;
			gridBagConstraints5.weightx = 1.0;
			gridBagConstraints5.gridx = 0;
			gridBagConstraints5.gridy = 4;

			jPanelResults = new JPanel();
			jPanelResults.setBorder(BorderFactory.createTitledBorder(BorderFactory.createBevelBorder(1), "Dialogue : "));
			jPanelResults.setLayout(new GridBagLayout());
 
	        JScrollPane scrollPane = new JScrollPane();
	        scrollPane.setViewportView(getTextAreaSelectedInterpretation());
	        scrollPane.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "What the system understood: "));
	        jPanelResults.add(scrollPane, gridBagConstraints1);

	        jPanelResults.add(getJScrollPaneAnswer(), gridBagConstraints2);
	        jPanelResults.add(getJScrollPaneHelpSystem(), gridBagConstraints3);
			jPanelResults.add(getJScrollPaneDialogueContext(), gridBagConstraints4);
			jPanelResults.add(getJScrollPaneRawRecResult(), gridBagConstraints5);
			
		}
		return jPanelResults;
	}

	public static void main(String args[]){
		// createSwingComponents();
		JFrame calendarFrame = new JFrame("Calendar - Regulus Application");
		CalendarPanel calendarPanel = new CalendarPanel(null, null);

		//
		// Glue up visual components
		//
		Box boxCenter = Box.createVerticalBox();
		boxCenter.add(calendarPanel, null);
		boxCenter.add(Box.createVerticalGlue(), null);

		Box debugBox = Box.createHorizontalBox();
		Box textComponents = Box.createVerticalBox();
		debugBox.add(textComponents);
		boxCenter.add(debugBox);

		calendarFrame.getContentPane().setLayout(new BorderLayout());
		calendarFrame.getContentPane().add(boxCenter, BorderLayout.CENTER);
		
		calendarFrame.pack();
		//
		// resize the window
		//
		Dimension currentSize = calendarFrame.getSize();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		calendarFrame.setSize((int) (screenSize.getWidth() * 0.8), (int)(screenSize.getHeight()*0.9) );

		calendarFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		
		calendarFrame.setVisible(true);
	}
} //  @jve:visual-info decl-index=0 visual-constraint="10,10"
