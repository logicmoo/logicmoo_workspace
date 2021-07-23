/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mapeditor.editortabs;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import mapeditor.A4MapEditor;
import mapeditor.maps.TileSet;

/**
 *
 * @author santi
 */
public class TileSetTab extends EditorTab {

    JList tileSetListcomponent = null;
    DefaultListModel tileSetList = new DefaultListModel();
    TileSelector tileSelector = null;
    
    public TileSetTab(A4MapEditor editor) {
        super(editor);
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        
        // tile set list:
        {
            JPanel leftPanel = new JPanel();
            leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.Y_AXIS));
            
            tileSetListcomponent = new JList(tileSetList);
            tileSetListcomponent.setPreferredSize(new Dimension(240,200));
            tileSetListcomponent.getSelectionModel().addListSelectionListener(
                new ListSelectionListener() {
                    public void valueChanged(ListSelectionEvent e) {
                        TileSet ts = (TileSet)tileSetListcomponent.getSelectedValue();
                        tileSelector.setTileSet(ts);
                    }
            });            
            leftPanel.add(tileSetListcomponent);
            
            JPanel buttonRow = new JPanel();
            buttonRow.setLayout(new BoxLayout(buttonRow, BoxLayout.X_AXIS));
            JButton add = new JButton("Add");
            add.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent event) {
                    JFileChooser chooser = new JFileChooser();
                    File pathFile = new File(m_editor.getMap().getPath());
                    chooser.setCurrentDirectory(pathFile);
                    FileNameExtensionFilter filter = new FileNameExtensionFilter(
                        "Image files", "png", "jpg", "bmp", "gif");
                    chooser.setFileFilter(filter);
                    int returnVal = chooser.showOpenDialog(leftPanel);
                    if (returnVal == JFileChooser.APPROVE_OPTION) {
                        try {
                            TileSet ts = new TileSet(chooser.getSelectedFile().getAbsolutePath(), m_editor.getMap());
                            m_editor.getMap().addTileSet(ts);
                            updateFromMap();
                        }catch(Exception e) {
                            e.printStackTrace();
                            JOptionPane.showMessageDialog(buttonRow, "Could not load the file!\n" + e.getMessage());                            
                        }
                    }
                }
            });
            buttonRow.add(add);
            JButton remove = new JButton("Remove");
            remove.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent event) {
                    TileSet ts = (TileSet)tileSetListcomponent.getSelectedValue();
                    if (ts!=null) {
                        m_editor.getMap().removeTileSet(ts);
                        updateFromMap();
                    }
                }
            });
            buttonRow.add(remove);
            leftPanel.add(buttonRow);
            
            add(leftPanel);
        }
        
        // show tiles
        {
            tileSelector = new TileSelector();
            JScrollPane scroll = new JScrollPane(tileSelector, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            tileSelector.setParent(scroll);
            add(scroll);
        }
        
        updateFromMap();
    }    
    
    
    public int getSelectedTile()
    {
        int startTile = 1;
        for(TileSet ts:m_editor.getMap().getTileSets()) {
            
            if ((TileSet)tileSetListcomponent.getSelectedValue() == ts) {
                int tile = tileSelector.getSelectedTile();
                return startTile + tile;
            }
            
            startTile += ts.getNTiles();
        }        
        
        return 0;
    }
    
    
    public void updateFromMap() {
        TileSet selected_ts = (TileSet)tileSetListcomponent.getSelectedValue();
        int selected_tile = tileSelector.getSelectedTile();
        
        boolean already_set = false;
        tileSetList.clear();
        for(TileSet ts:m_editor.getMap().getTileSets()) {
            tileSetList.addElement(ts);
            if (ts == selected_ts) {
                tileSetListcomponent.setSelectedValue(ts, true);
                tileSelector.setSelectedTile(selected_tile);
                already_set = true;
            }
        }
        
        if (!already_set) {
            TileSet ts = (TileSet)tileSetListcomponent.getSelectedValue();
            tileSelector.setTileSet(ts);
        }
    }
    
}
