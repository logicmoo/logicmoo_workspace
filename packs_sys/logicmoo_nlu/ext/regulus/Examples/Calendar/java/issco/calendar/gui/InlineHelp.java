package issco.calendar.gui;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.BorderFactory;
import javax.swing.JList;
import javax.swing.JScrollPane;

public class InlineHelp extends JScrollPane {
    private static final long serialVersionUID = -2810946588731356757L;

    private final Calendar calendarRootComponent;

    private JList list = null;
    
    private String previousHelpText = "";

    public InlineHelp(final Calendar calendarRootComponent, String title) {
        super(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        this.calendarRootComponent = calendarRootComponent;

        setBorder(BorderFactory.createTitledBorder(title));
        list = new JList();
        list.addMouseListener(new MouseAdapter() {

            public void mouseClicked(MouseEvent e) {
                String helpText = (String)list.getSelectedValue();
                if ((helpText != null) && (e.getClickCount() == 2)) {
                    int index = list.locationToIndex(e.getPoint());

                }
            }
        });
        setViewportView(list);
    }

    public void setHelp(String[] help) {
        if (help == null) {
            String[] emptyList = {};
            list.setListData(emptyList);
        } else {
            list.setListData(help);
        }
    }
    
    public void setEnabled(boolean enabled){
        super.setEnabled(enabled);
        list.setEnabled(enabled);
    }
}