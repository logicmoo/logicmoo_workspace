/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mapeditor.maps;

/**
 *
 * @author santi
 */
public class TileLayer {
    String m_name;
    boolean m_show;     // whether to show this in the editor, or hide it
    public int[][]m_tiles;
    
    public TileLayer(String name, int w, int h) {
        m_name = name;
        m_show = true;
        m_tiles = new int[w][h];
        for(int i = 0;i<w;i++) {
            for(int j = 0;j<h;j++) {
                m_tiles[i][j] = -1; 
            }
        }
    }
    
    public String toString() {
        return getName() + (m_show ? "":" (hidden)");
    }

    public String getName() {
        return m_name;
    }
    
    public boolean getShow() {
        return m_show;
    }
    
    public void setShow(boolean show) {
        m_show = show;
    }

    public int getWidth() {
        return m_tiles.length;
    }
    
    public int getHeight() {
        return m_tiles[0].length;
    }
    
    public int getTile(int x, int y) {
        return m_tiles[x][y];
    }
    
    public void setWidth(int w) {
        int current_w = getWidth();
        if (w==current_w) return;
        int current_h = getHeight();
        int [][]tiles = new int[w][current_h];
        for(int i = 0;i<current_h;i++) {
            for(int j = 0;j<current_w && j<w;j++) {
                tiles[j][i] = getTile(j,i);
            }
        }
        m_tiles = tiles;
    }
    
    public void setHeight(int h) {
        int current_h = getHeight();
        if (h==current_h) return;
        int current_w = getWidth();
        int [][]tiles = new int[current_w][h];
        for(int i = 0;i<h && i<current_h;i++) {
            for(int j = 0;j<current_w;j++) {
                tiles[j][i] = getTile(j,i);
            }
        }
        m_tiles = tiles;
    }

    public void setTile(int x, int y, int tile) {
        m_tiles[x][y] = tile;
    }
    
}
