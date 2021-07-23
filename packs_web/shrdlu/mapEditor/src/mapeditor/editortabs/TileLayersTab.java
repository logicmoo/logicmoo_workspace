/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mapeditor.editortabs;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import mapeditor.A4MapEditor;
import mapeditor.maps.TileLayer;

/**
 *
 * @author santi
 */
public class TileLayersTab extends EditorTab {

    JList tileLayerListcomponent = null;
    DefaultListModel tileLayerList = new DefaultListModel();
    
    public TileLayersTab(A4MapEditor editor) {
        super(editor);
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        
        tileLayerListcomponent = new JList(tileLayerList);
        tileLayerListcomponent.setPreferredSize(new Dimension(240,200));
/*
        tileLayerListcomponent.getSelectionModel().addListSelectionListener(
            new ListSelectionListener() {
                public void valueChanged(ListSelectionEvent e) {
                }
        });            
*/
        add(tileLayerListcomponent);

        JPanel buttonRow = new JPanel();
        buttonRow.setLayout(new BoxLayout(buttonRow, BoxLayout.X_AXIS));
        JButton add = new JButton("Add");
        add.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                int n = 1;
                do{
                    String name = "Tile Layer " + n;
                    if (m_editor.getMap().getTileLayer(name)==null) {
                        m_editor.getMap().addTileLayer(new TileLayer(name, m_editor.getMap().getWidth(), m_editor.getMap().getHeight()));
                        m_editor.refreshAllPanes();
                        break;
                    }
                    n++;
                }while(true);
            }
        });
        buttonRow.add(add);
        
        JButton remove = new JButton("Remove");
        remove.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                Object[] options = {"Ok",
                                    "Cancel"};
                int n = JOptionPane.showOptionDialog(remove,
                    "Delete the layer losing all of its content?",
                    "Confirmation",
                    JOptionPane.OK_CANCEL_OPTION,
                    JOptionPane.QUESTION_MESSAGE,
                    null,
                    options,
                    options[1]);
                if (n==0) {
                    TileLayer tl = (TileLayer)tileLayerListcomponent.getSelectedValue();
                    if (tl!=null) {
                        m_editor.getMap().removeTileLayer(tl);
                        m_editor.refreshAllPanes();
                    }
                }
            }
        });
        buttonRow.add(remove);

        JButton hideshow = new JButton("Hide/Show");
        hideshow.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                TileLayer tl = (TileLayer)tileLayerListcomponent.getSelectedValue();
                if (tl!=null) {
                    tl.setShow(!tl.getShow());
                    m_editor.refreshAllPanes();
                }
            }
        });
        buttonRow.add(hideshow);
        
        add(buttonRow);
        
        updateFromMap();
    }    
    
    
    public TileLayer getSelectedLayer() {
        return (TileLayer)tileLayerListcomponent.getSelectedValue();
    }
    
    
    public void updateFromMap() {
        TileLayer selected_tl = (TileLayer)tileLayerListcomponent.getSelectedValue();
        tileLayerList.clear();
        for(TileLayer tl:m_editor.getMap().getTileLayers()) {
            tileLayerList.addElement(tl);
            if (tl == selected_tl) {
                tileLayerListcomponent.setSelectedValue(tl, true);
            }
        }
    }
    
}
