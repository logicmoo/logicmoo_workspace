/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mapeditor.editortabs;

import javax.swing.JPanel;
import mapeditor.A4MapEditor;

/**
 *
 * @author santi
 */
public abstract class EditorTab extends JPanel {
    A4MapEditor m_editor = null;

    public EditorTab(A4MapEditor editor) {
        m_editor = editor;
    }
    
    public abstract void updateFromMap();
}
