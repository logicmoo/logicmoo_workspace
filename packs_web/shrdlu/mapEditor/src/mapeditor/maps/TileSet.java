/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mapeditor.maps;

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import javax.imageio.ImageIO;

/**
 *
 * @author santi
 */
public class TileSet {
    public String m_source_filename = null;
    A4Map m_map = null;
    BufferedImage m_image = null;
    
    public TileSet(String filename, A4Map map) throws Exception {
        m_source_filename = filename;
        m_map = map;
        
        // load the tiles:
        File f = new File(filename);
        m_image = ImageIO.read(f);
    }
    
    public String absolutePath() {
        return m_source_filename;
    }
    
    public String relativePath() {
        return new File(m_map.getPath()).toPath().toAbsolutePath().relativize(new File(m_source_filename).toPath().toAbsolutePath()).toString();
    }
    
    public String toString() {
        return relativePath();
    }
    
    public int getWidthInTiles() {
        return m_image.getWidth()/m_map.getTileWidth();
    }
    
    public int getHeightInTiles() {
        return m_image.getHeight()/m_map.getTileHeight();
    }

    public int getTileWidth() {
        return m_map.getTileWidth();
    }
    
    public int getTileHeight() {
        return m_map.getTileHeight();
    }

    public int getNTiles() {
        int h_tiles = m_image.getWidth()/m_map.getTileWidth();
        int v_tiles = m_image.getHeight()/m_map.getTileHeight();
        
        return v_tiles * h_tiles;
    }
    
    public BufferedImage getImage() {
        return m_image;
    }
    
    public void drawTile(int n, int x, int y, Graphics2D g2d) {
        int tw = m_map.getTileWidth();
        int th = m_map.getTileHeight();
        int h_tiles = m_image.getWidth()/tw;
        
        int sx = (n%h_tiles)*tw;
        int sy = (n/h_tiles)*th;
        
        g2d.drawImage(m_image, x, y, x+tw, y+th, 
                               sx, sy, sx+tw, sy+th, null);
    }
}
