/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mapeditor.editortabs;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import mapeditor.A4MapEditor;

/**
 *
 * @author santi
 */
public class MapVizListener implements MouseListener, MouseMotionListener
{
    A4MapEditor m_editor = null;    
    MapVizPanel m_panel = null;
    
    public MapVizListener(A4MapEditor editor, MapVizPanel panel) {
        m_editor = editor;
        m_panel = panel;
    }
    
    public void mouseClicked(MouseEvent e) {
    }

    public void mousePressed(MouseEvent e) {
        if (m_editor.selectedTool() == A4MapEditor.TOOL_TILES) {
            m_panel.addTile(e.getX()/m_editor.getMap().getTileWidth(), 
                    e.getY()/m_editor.getMap().getTileHeight());
        } else if (m_editor.selectedTool() == A4MapEditor.TOOL_REMOVE_TILES) {
            m_panel.removeTile(e.getX()/m_editor.getMap().getTileWidth(), 
                       e.getY()/m_editor.getMap().getTileHeight());
        } else if (m_editor.selectedTool() == A4MapEditor.TOOL_ADD_OBJECT) {
            m_panel.addObject(e.getX(),e.getY());
        } else if (m_editor.selectedTool() == A4MapEditor.TOOL_SELECT_OBJECTS) {
            m_panel.selectObject(e.getX(),e.getY());
        }
    }

    public void mouseReleased(MouseEvent e) {
    }

    public void mouseEntered(MouseEvent e) {
    }

    public void mouseExited(MouseEvent e) {
    }    

    public void mouseDragged(MouseEvent e) {
        if (m_editor.selectedTool() == A4MapEditor.TOOL_TILES) {
            m_panel.addTile(e.getX()/m_editor.getMap().getTileWidth(), 
                    e.getY()/m_editor.getMap().getTileHeight());
        } else if (m_editor.selectedTool() == A4MapEditor.TOOL_REMOVE_TILES) {
            m_panel.removeTile(e.getX()/m_editor.getMap().getTileWidth(), 
                       e.getY()/m_editor.getMap().getTileHeight());
        } else if (m_editor.selectedTool() == A4MapEditor.TOOL_ADD_OBJECT) {
        } else if (m_editor.selectedTool() == A4MapEditor.TOOL_SELECT_OBJECTS) {
        }        
    }

    public void mouseMoved(MouseEvent e) {

    }
}
