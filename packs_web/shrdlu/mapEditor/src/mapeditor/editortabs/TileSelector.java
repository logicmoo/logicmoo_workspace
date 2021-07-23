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
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import mapeditor.maps.TileSet;

/**
 *
 * @author santi
 */
public class TileSelector extends JPanel {
    
    TileSet m_tileSet = null;
    JScrollPane m_parent = null;
    int m_selectedTile = -1;
    
    public TileSelector() {        
        setPreferredSize(new Dimension(640,400));
        addMouseListener(new MouseListener() {
            public void mouseClicked(MouseEvent e) {
            }

            public void mousePressed(MouseEvent e) {
                if (m_tileSet!=null) {
                    int x = e.getX();
                    int y = e.getY();
                    int tilex = x/m_tileSet.getTileWidth();
                    int tiley = y/m_tileSet.getTileHeight();
                    if (tilex>=0 && tilex<m_tileSet.getWidthInTiles() &&
                        tiley>=0 && tiley<m_tileSet.getHeightInTiles()) {
                        m_selectedTile = tilex + tiley*m_tileSet.getWidthInTiles();
                        repaint();
                    }
                }
            }

            public void mouseReleased(MouseEvent e) {
            }

            public void mouseEntered(MouseEvent e) {
            }

            public void mouseExited(MouseEvent e) {
            }
        });
    }
    
    public void setParent(JScrollPane parent) {
        m_parent = parent;
    }
    
    public void setTileSet(TileSet ts) {
        if (m_tileSet == ts) return;
        
        m_tileSet = ts;
        if (m_tileSet==null) {
            setPreferredSize(new Dimension(32,32));
        } else {
            setPreferredSize(new Dimension(m_tileSet.getWidthInTiles()*m_tileSet.getTileWidth(),
                                           m_tileSet.getHeightInTiles()*m_tileSet.getTileHeight()));
        }
        if (m_parent!=null) {
            m_parent.revalidate();
            m_parent.repaint();
        } else {
            revalidate();
            repaint();
        }
        m_selectedTile = -1;
    }
    
    
    public int getSelectedTile() {
        return m_selectedTile;
    }
    
    
    public void setSelectedTile(int tile) {
        m_selectedTile = tile;
    }
    
    
    public void paint(Graphics g) {
        Graphics2D g2d = (Graphics2D)g;
        g2d.setColor(Color.BLACK);
        g2d.fillRect(0,0,getWidth(), getHeight());

        if (m_tileSet!=null) {
            int tw = m_tileSet.getTileWidth();
            int th = m_tileSet.getTileHeight();
            for(int i = 0;i<m_tileSet.getNTiles();i++) {
                int x = (i%m_tileSet.getWidthInTiles())*tw;
                int y = (i/m_tileSet.getWidthInTiles())*th;
                m_tileSet.drawTile(i, x, y, g2d);
                if (m_selectedTile==i) {
                    g2d.setColor(Color.WHITE);
                    g2d.drawRect(x, y, tw, th);
                }
            }
        }
    }    
    
}
