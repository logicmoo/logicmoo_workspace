package gamegui;
import lib.*;
import java.awt.Cursor;
import java.io.File;
import java.util.Dictionary;
import java.util.Hashtable;
import javax.swing.JLayeredPane;


/**
 * Cella della scacchiera
 * @author Baioni, Grandi, Tallevi Diotallevi
 */
public class Cell extends javax.swing.JPanel{

    final static public int DIM_IMG = 64;
    private JLayeredPane jLayeredPane1;
    private javax.swing.JLabel lblIcon;
    private javax.swing.JLabel lblNumber;
    private int x, y;
    private String color;
    private String figure;

    private static Dictionary<String, Object> catalogCF = new Hashtable<String, Object>();
   
    /**
     * 
     * @param x
     * @param y
     */
    public Cell(int x, int y) {
         this(x, y, "default", "empty");
    }

    /**
     *
     * @param x
     * @param y
     * @param color
     * @param figure
     */
    public Cell(int x, int y, String color, String figure) {
        super();
        this.x = x;
        this.y = y;
        
        this.setSize(DIM_IMG, DIM_IMG);
        this.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));
        this.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));

        initInternalComponents();

        this.setColor(color);
        this.setFigure(figure);
        this.resetNumber();
    }

    private void initInternalComponents() {
        lblIcon = new javax.swing.JLabel();
        lblNumber = new javax.swing.JLabel();
        jLayeredPane1 = new javax.swing.JLayeredPane();
        jLayeredPane1.setPreferredSize(new java.awt.Dimension(64, 64));
        lblNumber.setFont(new java.awt.Font("Arial", 1, 16));
        lblNumber.setForeground(new java.awt.Color(102, 204, 0));
        lblNumber.setBounds(0, 0, 60, 16);
        jLayeredPane1.add(lblNumber, javax.swing.JLayeredPane.DEFAULT_LAYER);
        lblIcon.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        lblIcon.setVerticalAlignment(javax.swing.SwingConstants.TOP);
        lblIcon.setBounds(0, 0, 64, 64);
        jLayeredPane1.add(lblIcon, javax.swing.JLayeredPane.DEFAULT_LAYER);
        org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING).add(org.jdesktop.layout.GroupLayout.TRAILING, layout.createSequentialGroup().add(jLayeredPane1, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)));
        layout.setVerticalGroup(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING).add(layout.createSequentialGroup().add(jLayeredPane1, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE).addContainerGap()));
        addMouseListener(new java.awt.event.MouseAdapter() {

            public void mouseClicked(java.awt.event.MouseEvent evt) {
                formMouseClicked(evt);
            }
        });
    }


    

    private void formMouseClicked(java.awt.event.MouseEvent evt) {
        // inizializziamo in modo che non matchino
        int xLastMove = ChessBoard.DIM +1;
        int yLastMove = ChessBoard.DIM +1;

        if (!ChessBoard.getInstance().getTrace().isEmpty()){
            Move lastMove = ChessBoard.getInstance().getTrace().get(ChessBoard.getInstance().getTrace().size()-1);
            xLastMove = lastMove.getX();
            yLastMove = lastMove.getY();
        }
        if (xLastMove==getPosX() && yLastMove==getPosY()){
            //l'utente ha cliccato di nuovo sull'ultima cella quindi la deselezioniamo
            ChessBoard.getInstance().undoMove();
        }else{
            setHighlight(true);
            ChessBoard.getInstance().addMove(x, y, color, figure);
        }  
    }

    /**
     * Set della figura
     * @param figure nome della figura
     */
    public void setFigure(String figure) {
        this.figure = figure;
        if (figure.equals("empty")) {
            lblIcon.setIcon(null);
        }
        else {
            String figureFile = (String) Cell.catalogCF.get(figure);
            lblIcon.setIcon(new javax.swing.ImageIcon(figureFile));
        }
    }

    /**
     * Set del colore
     * @param color nome del colore in inglese
     */
    public void setColor(String color) {
        this.color = color;
        setBackground((java.awt.Color)Cell.catalogCF.get(color));
    }

    /**
     * Set dell'evidenziazione. Se la cella è già selezionata essa verrà deselezionata
     * @param highlight
     */
    public void setHighlight(boolean highlight) {
        if (highlight)
            setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 255, 0), 3));
        else
            setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0,  0, 0), 1));
        
    }

    /**
     * Aggiunta di un numero alla label
     * @param number
     */
    public void addNumber(int number) {
        if (!lblNumber.getText().isEmpty()) {
            lblNumber.setText(lblNumber.getText() + "," + number);
        }
        else {
            lblNumber.setText("" + number);
        }
    }

    /**
     * Reset del testo della label
     */
    public void resetNumber() {
        lblNumber.setText(""); 
    }

    /**
     *
     * @return
     */
    public String getColor(){
        return this.color;
    }

    /**
     *
     * @return
     */
    public String getFigure(){
        return this.figure;
    }
    /**
     *
     * @return
     */
    public int getPosX(){
        return this.x;
    }
    /**
     *
     * @return
     */
    public int getPosY(){
        return this.y;
    }

    /**
     * Inizializzazione del dizionario interno utilizzato per recuperare i path delle immagini e dei colori
     */
    public static void initDictionary() {
       File dir = new File (".");
        try{
            String projectPath = dir.getCanonicalPath();
            catalogCF.put("arm", projectPath + "/img/arm.png");
            catalogCF.put("arms", projectPath + "/img/arms-lancia.png");
            catalogCF.put("bin", projectPath + "/img/bin.png");
            catalogCF.put("book", projectPath + "/img/book.png");
            catalogCF.put("disk", projectPath + "/img/disk.png");
            catalogCF.put("ipod", projectPath + "/img/ipod.png");
            catalogCF.put("monkey", projectPath + "/img/monkey.png");
            catalogCF.put("mouse", projectPath + "/img/mouse.png");
            catalogCF.put("paper", projectPath + "/img/paper.png");
            catalogCF.put("pen", projectPath + "/img/pen.png");
            catalogCF.put("poison", projectPath + "/img/poison.png");
            catalogCF.put("skull", projectPath + "/img/skull-empty.png");
            catalogCF.put("star", projectPath + "/img/star.png");
            catalogCF.put("troll", projectPath + "/img/troll.png");
            catalogCF.put("hammer", projectPath + "/img/martello.png");
            catalogCF.put("arrow", projectPath + "/img/arrow.png");
            catalogCF.put("money", projectPath + "/img/money.png");
            catalogCF.put("treasure", projectPath + "/img/treasure.png");
            catalogCF.put("home", projectPath + "/img/home.png");
            catalogCF.put("default", new java.awt.Color(255, 255, 255));
            catalogCF.put("yellow", new java.awt.Color(255, 255, 0));
            catalogCF.put("blue", new java.awt.Color(0,0,255));
            catalogCF.put("red", new java.awt.Color(255,0,0));
            catalogCF.put("black", new java.awt.Color(0,0,0));

        }catch(Exception e){
            e.printStackTrace();
        }
    }

}
