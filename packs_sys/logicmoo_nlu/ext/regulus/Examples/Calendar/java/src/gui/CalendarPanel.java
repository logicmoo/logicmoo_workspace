package gui;

import gui.transact.TrConstants;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.Font;
import java.util.ArrayList;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import java.util.Vector;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
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

import net.sf.regulus.NBestRegResult;
import net.sf.regulus.RegResult;

import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;

/**
 * Implements the GUI component creation. <br/>
 * Components pass events to a Calendar object. <br/>
 * This class registers for updates when the Dialogue object changes.<br/>
 *
 */
class CalendarPanel extends JPanel implements Observer {
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

	private static final long serialVersionUID = 9100232634579182486L;
    private static Logger logger = Logger.getLogger(CalendarPanel.class.getName());  //  @jve:decl-index=0:

    private final Calendar calendarRootComponent;
    private Dialogue dialogue;

    private static final String CMD_START_RECOGNITION = "start_recognition";  //  @jve:decl-index=0:
    private static final String CMD_ABORT_RECOGNITION = "abort_recognition";  //  @jve:decl-index=0:

	private static final String ELEM_LIST_DELIM = "------------";  //  @jve:decl-index=0:
	private static final String NO_ANSWER = " ? ";  //  @jve:decl-index=0:
    
    private JButton jButtonStartRecognition ;
      /** used to synchronize enabling/disabling of buttons */
    private Object buttonSynchObject = new Object();
    
	private  JTextArea textAreaSelInterpret ;
	private  JTextArea textAreaDialResp ;

	private JScrollPane jScrollPaneDialContext;
	private JButton jButtonRemoveContext ;
	private JScrollPane jScrollPaneHelpSystem ;
	private JList jListHelp ;
	private DefaultListModel modelJList;
	
	private JScrollPane jScrollPaneRawRecRes;

	private JScrollPane jScrollPaneAnswer ;
	private JTextArea jTextAreaDialHistory ;
	private JTable jTableRawRecRes ;
	private TableColumn jTableColumnScore;  //  @jve:decl-index=0:
	private TableColumn jTableColumnTrans ;  //  @jve:decl-index=0:
	private TableColumn jTableColumnValue ;  //  @jve:decl-index=0:
	private JPanel jPanelButtons ;
	private JPanel jPanelResults ;
	
	
	/**
     * This is the default constructor
     */	
    public CalendarPanel(final Calendar calendarRootComponent) {    	
        super();
               
        this.calendarRootComponent = calendarRootComponent;
        this.dialogue = calendarRootComponent.getDialogue();

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

        this.dialogue.addObserver(this);
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

    private void setRecognitionButtonEnabled(boolean b) {
        synchronized(buttonSynchObject){
            jButtonStartRecognition.setEnabled(b);
        }
    }

    private void recognitionButtonPressed(ActionEvent e) {   	
        if ((e != null) && (e.getActionCommand() != null)) {
            if (e.getActionCommand().equals(CMD_START_RECOGNITION)) {
            	this.setRecognitionInProgress(true);
            	
                // start calendarRootComponent.processRecRequest();
                if (calendarRootComponent.getTransact().status().equalsIgnoreCase("TrRecognise") ){
                }
                else{
                	System.out.println("The transaction state is " + 
                			calendarRootComponent.getTransact().status());
                	calendarRootComponent.getTransact().setState(TrConstants.READY_TO_RECOGNISE.getState());
                }
            	calendarRootComponent.getTransact().runTasks(); // here should run TrRecognise
                this.setRecognitionInProgress(false);

                // end calendarRootComponent.processRecRequest();
            }
            else if (e.getActionCommand().equals(CMD_ABORT_RECOGNITION)) {
                calendarRootComponent.processRecognitionAbortRequest();
            }
            
        }
    }

    private void setRecognitionInProgress(boolean isInProgress) {
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
        private void setRawResultTable(NBestRegResult nbestRegResult) {
            if (nbestRegResult != null) {
            	java.util.Iterator<RegResult> it = nbestRegResult.getNBestResults().iterator();
            	int i = 0;
        		while(it.hasNext()){
        			RegResult rr = it.next();  
        			
    				TableModel model =getJTableRawRecResult().getModel();
    				model.setValueAt(rr.getConfidence(), i, 0);
    				model.setValueAt(rr.getRecognition(), i, 1);
    				model.setValueAt(rr.getInterpretation().replace("[(value =", ""), i, 2);
    				
    				i++;
        		}
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

       
    private void setTextAreaSelectedInterpretation(String text) {
        if ((text == null) || (text.isEmpty())) {
        	textAreaSelInterpret.setText(NO_ANSWER);
        }
        else {        	
        	textAreaSelInterpret.setText(text);
        }
    }
    
    private void setTextAreaDialogueResponse(String text) {
        if ((text == null) || text.isEmpty() ) {
        	textAreaDialResp.setText(NO_ANSWER);
        }
        else {
        	textAreaDialResp.setText(text);
        	
        }
    }
    
    /**
     * Add a new element (of type String) in the list 
     */
        public void addToDialHistTextArea(String text) {
            if (text != null) {            	
            	jTextAreaDialHistory.append(text +"\n");
            	jTextAreaDialHistory.setCaretPosition(jTextAreaDialHistory.getText().length());
            }
            
        }

        
        private void setTextForHelpJList(Vector<String> helpSentList) {      	
        	if (jListHelp == null){
        		jListHelp = new JList(helpSentList);
        	}
        	else
        		jListHelp.setListData(helpSentList);        
        }

                          
     
	/**
	 * This method initialises textAreaSelInterpret	
	 */
	JTextArea getTextAreaSelInterpret() {
		if (textAreaSelInterpret == null) {
			textAreaSelInterpret = new JTextArea(3, 20);
			textAreaSelInterpret.setEditable(false);
			textAreaSelInterpret.setLineWrap(true);
			textAreaSelInterpret.setWrapStyleWord(true);	
			textAreaSelInterpret.setFont(new Font("Serif", Font.TRUETYPE_FONT, 16));
		}
		return textAreaSelInterpret;
	}


	/**
	 * This method initializes textAreaDialogueResponse	
	 * 	
	 * @return issco.regulus.gui.JTextArea	
	 */
	JTextArea getTextAreaDialogueResponse() {
		if (textAreaDialResp == null) {
			textAreaDialResp = new JTextArea(3,50);
			textAreaDialResp.setEditable(false);
			textAreaDialResp.setLineWrap(true);
			textAreaDialResp.setWrapStyleWord(true);
			textAreaDialResp.setFont(new Font("Serif", Font.BOLD, 16));

		}
		return textAreaDialResp;
	}

	/**
	 * This method initializes jScrollPaneDialogueContext	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	protected JScrollPane getJScrollPaneDialogueContext() {
		if (jScrollPaneDialContext == null) {
			jScrollPaneDialContext = new JScrollPane();
			jScrollPaneDialContext.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Dialogue History : "));
			jScrollPaneDialContext.setViewportView(getJTextAreaDialogueHistory());
			// jScrollPaneDialogueContext.getViewport().setViewPosition(0);			 
		}
		return jScrollPaneDialContext;
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
					calendarRootComponent.getTransact().sendRemoveContextCmdToDialogueServer();
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
	JList getJListHelpSystem() {
		if (jListHelp == null) {
			modelJList = new DefaultListModel(); 
			jListHelp = new JList(modelJList);
			jListHelp.setFont(new Font("Serif", Font.TRUETYPE_FONT, 16));
			//TODO Take the selected help sentence and send it to the Dialogue System 
			// in order to obtain the response.
		}
		return jListHelp;
	}

	/**
	 * This method initializes jScrollPaneRawRecResult	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	protected JScrollPane getJScrollPaneRawRecResult() {
		if (jScrollPaneRawRecRes == null) {
			jScrollPaneRawRecRes = new JScrollPane();
			jScrollPaneRawRecRes.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Raw Recognition Result : "));
			
			jScrollPaneRawRecRes.setViewportView(getJTableRawRecResult());
			jScrollPaneRawRecRes.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);			
		}
		return jScrollPaneRawRecRes;
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
		if (jTextAreaDialHistory == null) {
			jTextAreaDialHistory = new JTextArea(5,50);
			jTextAreaDialHistory.setEditable(false);
		}
		return jTextAreaDialHistory;
	}

	/**
	 * This method initializes jTableRawRecResult	
	 * 	
	 * @return javax.swing.JTable	
	 */
	private JTable getJTableRawRecResult() {
		if (jTableRawRecRes == null) {
			jTableRawRecRes = new JTable();
			jTableRawRecRes.setRowHeight(30);
			jTableRawRecRes.addColumn(getTableColumnScore()); 
			jTableRawRecRes.addColumn(getTableColumnTrans());
			jTableRawRecRes.addColumn(getTableColumnValue());
			
			jTableRawRecRes.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
			
			jTableRawRecRes.setAutoCreateColumnsFromModel(false);			
			jTableRawRecRes.setModel(new RawRecResListModel());
			
		}
		return jTableRawRecRes;
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
	        scrollPane.setViewportView(getTextAreaSelInterpret());
	        scrollPane.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "What the system understood: "));
	        jPanelResults.add(scrollPane, gridBagConstraints1);

	        jPanelResults.add(getJScrollPaneAnswer(), gridBagConstraints2);
	        jPanelResults.add(getJScrollPaneHelpSystem(), gridBagConstraints3);
			jPanelResults.add(getJScrollPaneDialogueContext(), gridBagConstraints4);
			jPanelResults.add(getJScrollPaneRawRecResult(), gridBagConstraints5);
			
		}
		return jPanelResults;
	}

	/**
	 * This method will receive the Dialogue object as its Observable argument.
	 */
	public void update(Observable o, Object arg) {
		Dialogue d = (Dialogue) o;
		int id = ((Integer)arg).intValue();
		switch (id){
			case Dialogue.ID_REC_TEXT:
				{
					setTextAreaSelectedInterpretation(d.getRecognizedText());
					addToDialHistTextArea(ELEM_LIST_DELIM);
					addToDialHistTextArea("User   : " + d.getRecognizedText());
				}
			case Dialogue.ID_ANSWER : 
				{
					setTextAreaDialogueResponse(d.getAnswer());
					addToDialHistTextArea("System : " + d.getAnswer());					
				}
			case Dialogue.ID_HELP : setTextForHelpJList(d.getHelp());
			case Dialogue.Id_RAW_REC: 		
					//setRawResultTable(d.getRawRecResult());			
				setRawResultTable(d.getRawRecResult());
		}				
		repaint();
		
	}
	
	
} //  @jve:visual-info decl-index=0 visual-constraint="10,10"
