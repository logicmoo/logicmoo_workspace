/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mapeditor.maps;

import java.io.File;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.jdom.Element;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;
import util.XMLWriter;

/**
 *
 * @author santi
 */
public class A4Map {
    String m_path = null;
    
    int m_tile_width = 32; // in pixels
    int m_tile_height = 32;
    
    int m_map_width = 32; // in tiles
    int m_map_height = 20;
    
    String m_map_name = "a map";
    
    List<TileSet> m_tileSets = new ArrayList<>();
    
    List<TileLayer> m_tileLayers = new ArrayList<>();
    List<ObjectLayer> m_objectLayers = new ArrayList<>();
    
    List<Element> m_onStart = new ArrayList<>();
    List<Element> m_storyStateRules = new ArrayList<>();
    List<Element> m_eventRules = new ArrayList<>();
    
    
    public A4Map(String name, int w, int h, int tw, int th) {
        m_path = Paths.get(".").toAbsolutePath().normalize().toString();
        m_map_name = name;
        m_map_width = w;
        m_map_height = h;
        m_tile_width = tw;
        m_tile_height = th;
        
        m_tileLayers.add(new TileLayer("Tile Layer 1", m_map_width, m_map_height));
    }
    
    
    public A4Map(Element xml, String path) throws Exception {
        m_path = path;
        if (!xml.getName().equals("map")) throw new Exception("top tag is not `map'!");
        m_map_width = Integer.parseInt(xml.getAttributeValue("width"));
        m_map_height = Integer.parseInt(xml.getAttributeValue("height"));
        m_tile_width = Integer.parseInt(xml.getAttributeValue("tilewidth"));
        m_tile_height = Integer.parseInt(xml.getAttributeValue("tileheight"));
        m_map_name = null;
        Element properties_xml = xml.getChild("properties");
        for(Object o:properties_xml.getChildren()) {
            Element property_xml = (Element)o;
            if (property_xml.getAttributeValue("name").equals("name")) {
                m_map_name = property_xml.getAttributeValue("value");
            }
        }
        if (m_map_name==null) throw new Exception("Map specifies no name!");
        
        // tile sets:
        for(Object o:xml.getChildren("tileset")) {
            Element tileset_xml = (Element)o;
            for(Object o2:tileset_xml.getChildren("image")) {
                Element image_xml = (Element)o2;
                String filename = m_path + File.separator + image_xml.getAttributeValue("source");
                TileSet ts = new TileSet(filename, this);
                m_tileSets.add(ts);
            }
        }
        
        // tile layers:
        for(Object o:xml.getChildren("layer")) {
            Element layer_xml = (Element)o;
            TileLayer l = new TileLayer(layer_xml.getAttributeValue("name"),
                                        Integer.parseInt(layer_xml.getAttributeValue("width")),
                                        Integer.parseInt(layer_xml.getAttributeValue("height")));
            List tiles = layer_xml.getChild("data").getChildren("tile");
            Iterator it = tiles.iterator();
            for(int i = 0;i<l.getHeight();i++) {
                for(int j = 0;j<l.getWidth();j++) {
                    Element tile_xml = (Element)it.next();
                    l.setTile(j, i, Integer.parseInt(tile_xml.getAttributeValue("gid")));
                }
            }
            m_tileLayers.add(l);
        }
        
        // object layers:
        for(Object o:xml.getChildren("objectgroup")) {
            Element layer_xml = (Element)o;
            ObjectLayer l = new ObjectLayer(layer_xml.getAttributeValue("name"));
            for(Object o2:layer_xml.getChildren("object")) {
                Element object_xml = (Element)o2;
                A4Object mo = new A4Object(object_xml);
                l.addObject(mo);
            }
            m_objectLayers.add(l);
        }        
        
        // scripts:
        for(Object o:xml.getChildren("onStart")) {
            Element o_xml = (Element)o;
            m_onStart.add(o_xml);
        }        
        for(Object o:xml.getChildren("storyStateRule")) {
            Element o_xml = (Element)o;
            m_storyStateRules.add(o_xml);
        }        
        for(Object o:xml.getChildren("eventRule")) {
            Element o_xml = (Element)o;
            m_eventRules.add(o_xml);
        }        
    }
    
    public void saveToXML(XMLWriter w) {
        XMLOutputter outp = new XMLOutputter();
        outp.getFormat().setTextMode(Format.TextMode.TRIM);
        outp.getFormat().setIndent("  ");
        outp.getFormat().setLineSeparator("\n");

        w.tagWithAttributes("map", "version=\"A4\" "+
                                   "orientation=\"orthogonal\" "+
                                   "width=\""+m_map_width+"\" "+
                                   "height=\""+m_map_height+"\" "+
                                   "tilewidth=\""+m_tile_width+"\" "+
                                   "tileheight=\""+m_tile_height+"\"");
        w.tag("properties");
        w.closedTagWithAttributes("property", "name=\"name\" value=\""+m_map_name+"\"");
        w.tag("/properties");
        
        int firstTile = 1;
        for(TileSet ts:m_tileSets) {
            w.tagWithAttributes("tileset", "firstgid=\""+firstTile+"\" "+
                                       "name=\"graphics\" "+
                                       "tilewidth=\""+m_tile_width+"\" "+
                                       "tileheight=\""+m_tile_height+"\"");
            w.closedTagWithAttributes("image", "source=\""+ts.relativePath()+"\" "+
                                               "width=\""+ts.getImage().getWidth()+"\" "+
                                               "height=\""+ts.getImage().getHeight()+"\"");
            w.tag("/tileset");
            firstTile += ts.getWidthInTiles()*ts.getHeightInTiles();
        }
        
        for(TileLayer tl:m_tileLayers) {
            w.tagWithAttributes("layer","name=\""+tl.getName()+"\" width=\""+m_map_width+"\" height=\""+m_map_height+"\"");
            w.tag("data");
            for(int i = 0;i<tl.getHeight();i++) {
                for(int j = 0;j<tl.getWidth();j++) {
                    w.closedTagWithAttributes("tile", "gid=\""+tl.getTile(j, i)+"\"");
                }
            }
            w.tag("/data");
            w.tag("/layer");
        }
        
        for(ObjectLayer ol:m_objectLayers) {
            w.tagWithAttributes("objectgroup","name=\""+ol.getName()+"\" width=\""+m_map_width+"\" height=\""+m_map_height+"\"");
            for(A4Object mo:ol.getObjects()) {
                String s = outp.outputString(mo.getXML());        
                w.rawXMLRespectingTabs(s);
            }
            w.tag("/objectgroup");
        }
        
        for(Element e:m_onStart) {
            String s = outp.outputString(e);
            w.rawXMLRespectingTabs(s);
        }
        for(Element e:m_storyStateRules) {
            String s = outp.outputString(e);
            w.rawXMLRespectingTabs(s);
        }
        for(Element e:m_eventRules) {
            String s = outp.outputString(e);
            w.rawXMLRespectingTabs(s);
        }
        
        w.tag("/map");
    }
    
    public String getPath() {
        return m_path;
    }
    
    public void setPath(String path) {
        m_path = path;
    }
    
    public String getName() {
        return m_map_name;
    }
    
    public void setName(String name) {
        m_map_name = name;
    }
    
    public int getWidth() {
        return m_map_width;
    }

    public void setWidth(int w) {
        m_map_width = w;
        for(TileLayer l:m_tileLayers) l.setWidth(w);
    }
    
    public int getHeight() {
        return m_map_height;
    }
    
    public void setHeight(int h) {
        m_map_height = h;
        for(TileLayer l:m_tileLayers) l.setHeight(h);
    }

    public int getTileWidth() {
        return m_tile_width;
    }
    
    public void setTileWidth(int tw) {
        m_tile_width = tw;
    }

    public int getTileHeight() {
        return m_tile_height;
    }

    public void setTileHeight(int th) {
        m_tile_height = th;
    }

    public List<TileSet> getTileSets() {
        return m_tileSets;
    }
    
    public void addTileSet(TileSet ts) {
        m_tileSets.add(ts);
    }
    
    public void removeTileSet(TileSet ts) {
        m_tileSets.remove(ts);
    }
    
    public TileLayer getTileLayer(String name) {
        for(TileLayer tl:m_tileLayers) {
            if (tl.getName().equals(name)) return tl;
        }
        return null;
    }
    
    public List<TileLayer> getTileLayers() {
        return m_tileLayers;
    }
    
    public void addTileLayer(TileLayer tl) {
        m_tileLayers.add(tl);
    }
    
    public void removeTileLayer(TileLayer tl) {
        m_tileLayers.remove(tl);
    }


    public ObjectLayer getObjectLayer(String name) {
        for(ObjectLayer ol:m_objectLayers) {
            if (ol.getName().equals(name)) return ol;
        }
        return null;
    }
    
    public List<ObjectLayer> getObjectLayers() {
        return m_objectLayers;
    }
    
    public void addObjectLayer(ObjectLayer ol) {
        m_objectLayers.add(ol);
    }
    
    public void removeObjectLayer(ObjectLayer ol) {
        m_objectLayers.remove(ol);
    }
    
    public List<Element> getOnStart() {
        return m_onStart;
    }
    
    public void addOnStart(Element e) {
        m_onStart.add(e);
    }
    
    public void removeOnStart(Element e) {
        m_onStart.remove(e);
    }

    public List<Element> getStoryStateRules() {
        return m_storyStateRules;
    }
    
    public void addStoryStateRule(Element e) {
        m_storyStateRules.add(e);
    }

    public void removeStoryStateRule(Element e) {
        m_storyStateRules.remove(e);
    }

    public List<Element> getEventRules() {
        return m_eventRules;
    }
    
    public void addEventRule(Element e) {
        m_eventRules.add(e);
    }

    public void removeEventRule(Element e) {
        m_eventRules.remove(e);
    }

}
