/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mapeditor.editortabs;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.HashMap;
import javax.swing.JOptionPane;
import mapeditor.A4MapEditor;
import mapeditor.maps.A4Map;
import mapeditor.maps.A4Object;
import mapeditor.maps.A4ObjectClass;
import mapeditor.maps.ObjectLayer;
import mapeditor.maps.TileLayer;
import mapeditor.maps.TileSet;
import org.jdom.Element;

/**
 *
 * @author santi
 */
public class MapVizPanel extends EditorTab {
    
    A4Map m_currentMap = null;
    HashMap<String,TileSet> m_additionalTilesets = new HashMap<>();
    
    public MapVizPanel(A4MapEditor editor) {
        super(editor);
        
        MapVizListener l = new MapVizListener(m_editor, this);
        
        addMouseListener(l);   
        addMouseMotionListener(l);   
        
        updateFromMap();
    }
    
    public void paint(Graphics g) {
        Graphics2D g2d = (Graphics2D)g;
        A4Map map = m_editor.getMap();
        
        // clear and draw map limits:
        g2d.setColor(Color.GRAY);
        g2d.fillRect(0,0,getWidth(), getHeight());
        g2d.setColor(Color.BLACK);
        g2d.fillRect(0,0,map.getWidth()*map.getTileWidth(), map.getHeight()*map.getTileHeight());
        
        // draw tile layers:
        for(TileLayer l:map.getTileLayers()) {
            if (l.getShow()) {
                int y = 0;
                for(int i = 0;i<l.getHeight();i++, y+=map.getTileHeight()) {
                    int x = 0;
                    for(int j = 0;j<l.getWidth();j++, x+=map.getTileWidth()) {
                        int gid = l.getTile(j, i);
                        drawTile(gid, x, y, g2d);
                    }
                }
            }
        }
        
        // draw the object layers:
        A4Object selectedObject = m_editor.getSelectedObject();
        for(ObjectLayer l:map.getObjectLayers()) {
            if (l.getShow()) {
                for(A4Object mo:l.getObjects()) {
                    Element animation = mo.getDefaultAnimation();
                    if (animation!=null) {
                        if (!drawAnimation(animation, mo.getX(), mo.getY(), g2d)) {
                            animation = null;
                        }
                    }
                    if (animation==null) {
                        g2d.setColor(Color.white);
                        g2d.drawRect(mo.getX(), mo.getY(), mo.getWidth(), mo.getHeight());
                    }
                    
                    if (mo == selectedObject) {
                        g2d.setColor(Color.cyan);
                        g2d.drawRect(mo.getX(), mo.getY(), mo.getWidth(), mo.getHeight());
                    }
                }
            }
        }
    }    
    
    public void drawTile(int gid, int x, int y, Graphics2D g2d)
    {
        if (gid==0) return;
        gid--;
        for(TileSet ts:m_editor.getMap().getTileSets()) {
            int n_tiles = ts.getNTiles();
            if (gid<n_tiles) {
                ts.drawTile(gid, x, y, g2d);
                return;
            }
            gid-=n_tiles;
        }
    }
    
    
    public boolean drawAnimation(Element animation, int x, int y, Graphics2D g2d)
    {
        try {
            String tileSetName = animation.getAttributeValue("file");
            TileSet ts = m_additionalTilesets.get(tileSetName);
            if (ts==null) {
                String filename = m_currentMap.getPath() + File.separator + tileSetName;
                ts = new TileSet(filename, m_currentMap);
                m_additionalTilesets.put(tileSetName, ts);
            }
            int width = Integer.parseInt(animation.getAttributeValue("dx"));
            int height = Integer.parseInt(animation.getAttributeValue("dy"));
            int starting_tile = Integer.parseInt(animation.getValue());
            for(int i = 0;i<height;i++) {
                for(int j = 0;j<width;j++) {
                   int tile = starting_tile + j + i*ts.getWidthInTiles();
                   ts.drawTile(tile, x + ts.getTileWidth()*j, y + ts.getTileHeight()*i, g2d);
                }
            }
        } catch(Exception e) {
            return false;
        }
        
        return true;
    }


    public void updateFromMap() {
        A4Map map = m_editor.getMap();
        setPreferredSize(new Dimension(map.getWidth()*map.getTileWidth(),
                                        map.getHeight()*map.getTileHeight()));
        m_currentMap = map;
        if (m_currentMap!=map) {
            m_additionalTilesets.clear();
        }
    }
    
    
    public void addTile(int x, int y) {
        TileLayer tl = m_editor.getSelectedTileLayer();
        int tile = m_editor.getSelectedTile();
        
        if (tl!=null && tile>0) {
            if (x>=0 && x<tl.getWidth() &&
                y>=0 && y<tl.getHeight()) {
                tl.setTile(x, y, tile);
                m_editor.refreshAllPanes();
            }
        }
    }


    public void removeTile(int x, int y) {
        TileLayer tl = m_editor.getSelectedTileLayer();
        
        if (tl!=null) {
            if (x>=0 && x<tl.getWidth() &&
                y>=0 && y<tl.getHeight()) {
                tl.setTile(x, y, 0);
                m_editor.refreshAllPanes();
            }
        }
    }


    public void addObject(int x, int y) {
        ObjectLayer ol = m_editor.getSelectedObjectLayer();
        if (ol!=null) {
            A4ObjectClass object_class = m_editor.getSelectedObjectType();
            Element e = new Element("object");
            e.setAttribute("class","item");
            if (object_class!=null) {
                e.setAttribute("class",object_class.getName());
            }
            e.setAttribute("x",""+x);
            e.setAttribute("y",""+y);
            e.setAttribute("width",""+m_currentMap.getTileWidth());
            e.setAttribute("height",""+m_currentMap.getTileHeight());

            A4Object mo = new A4Object(e);
            ol.addObject(mo);
            m_editor.refreshAllPanes();
        } else {
            JOptionPane.showMessageDialog(this, "Select an object layer first.");
        }
    }


    public void selectObject(int x, int y) {
        for(ObjectLayer ol:m_editor.getMap().getObjectLayers()) {
            for(A4Object mo:ol.getObjects()) {
                if (mo.getX()<=x && mo.getX()+mo.getWidth()>x &&
                    mo.getY()<=y && mo.getY()+mo.getHeight()>y) {
                    m_editor.selectObject(ol, mo);
                    return;
                }
            }
        }
        m_editor.selectObject(null, null);
    }
}
