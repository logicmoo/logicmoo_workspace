package net.sf.regulus.plugins.protege;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import net.sf.regulus.Utils;

import org.apache.commons.configuration.PropertiesConfiguration;

import edu.stanford.smi.protege.Application;
import edu.stanford.smi.protege.model.Cls;
import edu.stanford.smi.protege.model.DefaultSimpleInstance;
import edu.stanford.smi.protege.model.DefaultSlot;
import edu.stanford.smi.protege.model.Facet;
import edu.stanford.smi.protege.model.Project;
import edu.stanford.smi.protege.model.Slot;
import edu.stanford.smi.protege.model.ValueType;
import edu.stanford.smi.protege.plugin.ExportPlugin;
import edu.stanford.smi.protege.util.ComponentFactory;
import edu.stanford.smi.protege.util.Log;

public class ProtegeExporter implements ExportPlugin {
    
    private final String AGREEMENT_STRING = "agr";
    private final String ENTRY_TYPE_STRING = "entry_type";
    private final String SURFACE_FORM_STRING = "surface_form";
    private final String SURFACE_FORM_STRING_LATIN_TRANSLITERATION = "latin_transliteration";
    private final String SURFACE_FORM_STRING_GLOSS = "gloss";
    private final String SEMANTIC_STRING = "sem";

    public class Configuration extends PropertiesConfiguration {
        private boolean isDebuggingEnabled = true;
        
        private boolean isMultilingualDefinitionExportEnabled = false;
        
        private File featureValueFile = null;
        private PrintWriter globalDefinitionPrintWriter = null;
        private Hashtable<String, PrintWriter> langNameLexiconPrintWriterHash = null;
        private Hashtable<String, PrintWriter> langNameDefinitionPrintWriterHash = null;
        private String[] languages = null;
        private List ignoreFeatureList = null;

        public Configuration(File configFile) throws Exception {
            super(configFile);

            langNameLexiconPrintWriterHash = new Hashtable<String, PrintWriter>();
            langNameDefinitionPrintWriterHash = new Hashtable<String, PrintWriter>();

            isDebuggingEnabled = this.getBoolean("debug", false);

            ignoreFeatureList = this.getList("export.ignore.features", null);
            if(ignoreFeatureList == null){
                ignoreFeatureList = new LinkedList();
            }
            
            //
            // read in the language array
            //
            List<String> langList = this.getList("export.languages");
            if((langList != null) && (langList.size() > 0)){
                languages = new String[langList.size()];
                languages = langList.toArray(languages);
            }
            else{
                String warningMessage = "No languages specified in configuration file.\nPlease make sure ${export.languages} is set."; 
                LOGGER.warning(warningMessage);
                JOptionPane.showMessageDialog(null, warningMessage, "Error while reading configuration", JOptionPane.WARNING_MESSAGE);
            }
            LOGGER.config("Languages set to '" + languages + "'");
            
            //
            // read in ${export.dir} parameter - if not specified in the configuration
            // set it to the directory where the configuration resides
            //
            String configFileDir = configFile.getParent();
            if(configFileDir == null){
                configFileDir = "";
            }
            String exportDir = this.getString("export.dir");
            
            if(exportDir == null){
                this.setProperty("export.dir", configFileDir);
                exportDir = configFileDir;
            }
            LOGGER.config("Export directory set to '" + exportDir + "'");
            
            //
            // check if export of multilingual definition file is enabled
            //
            isMultilingualDefinitionExportEnabled = getBoolean("export.multilingual.definition", true);
            
            if(isMultilingualDefinitionExportEnabled){
                //
                // where to export multilingual feature definition file 
                //
                String featureValueFileName = getString("export.multilingual.definition.file", "");    
                if(Utils.isStringEmpty(featureValueFileName)) {
                    //
                    // construct feature value file name from export file name
                    //
                    String exportBaseFileName = configFile.getName();
                    String baseName = "defs.regulus";
                    int dotPos = exportBaseFileName.indexOf(".");
    
                    if(dotPos == -1){
                        baseName = exportBaseFileName + "-defs.regulus";
                    }
                    else{
                        baseName = exportBaseFileName.substring(0, dotPos);
                        baseName = baseName + "-defs.regulus";
                    }
                    featureValueFileName = configFile.getParent() + "/" + baseName;
                }
                
                LOGGER.config("Will export global feature-value-space to '" + featureValueFileName + "'");
                featureValueFile = new File(featureValueFileName);
                globalDefinitionPrintWriter = new PrintWriter(featureValueFile);
            }
            
            //
            // construct PrintWriters for all languages
            //
            String exportBaseDir = configFile.getParent();
            if(exportBaseDir == null){
                exportBaseDir = "";
            }
            
            for(int i=0 ; i<languages.length ; i++){
                // String fileName = exportBaseDir + "/" + languages[i].toLowerCase() + "-lex.regulus";
                this.setProperty("lang", languages[i]);
                String fileName = this.getString("export.languages.lexicals");
                File lexicalFile = new File(fileName);
                if(! lexicalFile.isAbsolute()){
                    fileName = exportDir + "/" + fileName;
                }
                LOGGER.config("Will export lexical entries for '" + languages[i] + "' to '" + fileName + "'");
                
                String encoding = this.getString(languages[i] + ".encoding", "");
                PrintWriter pw = null;
                if(Utils.isStringEmpty(encoding)) {
                    pw = new PrintWriter(new File(fileName));
                }
                else {
                    pw = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new PrintStream(fileName), encoding)));
                }
                langNameLexiconPrintWriterHash.put(languages[i].toLowerCase(), pw);
                
                String fileNameFeatureValue = this.getString("export.languages.definitions");
                File featureValueFile = new File(fileNameFeatureValue);
                if(! featureValueFile.isAbsolute()){
                    fileNameFeatureValue = exportDir + "/" + fileNameFeatureValue;
                }
                LOGGER.config("Will export definitions for '" + languages[i] + "' to '" + fileNameFeatureValue + "'");
                // String fileNameFeatureValue = exportBaseDir + "/" + languages[i].toLowerCase() + "-def.regulus";
                PrintWriter pwfv = new PrintWriter(new File(fileNameFeatureValue));
                langNameDefinitionPrintWriterHash.put(languages[i].toLowerCase(), pwfv);
            }
            // FileUtilities.createPrintWriter(File, boolean);
        }

        /**
         * @return Returns the languages.
         */
        public String[] getLanguages() {
            return languages;
        }

        public PrintWriter getGlobalDefinitionPrintWriter() {
            return globalDefinitionPrintWriter;
        }

        public void releaseResources() {
            if(globalDefinitionPrintWriter != null) {
                globalDefinitionPrintWriter.close();
            }
            
            Enumeration values = langNameLexiconPrintWriterHash.elements();
            
            while(values.hasMoreElements()){
                PrintWriter pw = (PrintWriter)values.nextElement();
                if(pw != null){
                    pw.close();
                }
            }
            
            values = langNameDefinitionPrintWriterHash.elements();
            while(values.hasMoreElements()){
                PrintWriter pw = (PrintWriter)values.nextElement();
                if(pw != null){
                    pw.close();
                }
            }
        }

        public PrintWriter getLexiconPrintWriterForLanguage(String languageName) {
            return (PrintWriter)langNameLexiconPrintWriterHash.get(languageName.toLowerCase());
        }

        /**
         * @return Returns the isDebuggingEnabled.
         */
        public boolean isDebuggingEnabled() {
            return isDebuggingEnabled;
        }

        /**
         * @param isDebuggingEnabled The isDebuggingEnabled to set.
         */
        public void setDebuggingEnabled(boolean isDebuggingEnabled) {
            this.isDebuggingEnabled = isDebuggingEnabled;
        }

        public PrintWriter getDefinitionPrintWriterForLanguage(String languageName) {
            return (PrintWriter)langNameDefinitionPrintWriterHash.get(languageName.toLowerCase());
        }

        public boolean isFeatureExportable(String feature){
            boolean result = true;
            
            if(ignoreFeatureList.contains(feature)){
                result = false;
            }
            
            return result;
        }

        /**
         * @return Returns the ignoreFeatureList.
         */
        public List getIgnoreFeatureList() {
            return ignoreFeatureList;
        }

        /**
         * @param ignoreFeatureList The ignoreFeatureList to set.
         */
        public void setIgnoreFeatureList(List ignoreFeatureList) {
            this.ignoreFeatureList = ignoreFeatureList;
        }
        
        public String getTransliterationMacro(){
            return this.getString("export.transliteration.macro", "transliteration");
        }

        /**
         * @return Returns the isMultilingualDefinitionExportEnabled.
         */
        public boolean isMultilingualDefinitionExportEnabled() {
            return isMultilingualDefinitionExportEnabled;
        }

        /**
         * @param isMultilingualDefinitionExportEnabled The isMultilingualDefinitionExportEnabled to set.
         */
        public void setMultilingualDefinitionExportEnabled(boolean isMultilingualDefinitionExportEnabled) {
            this.isMultilingualDefinitionExportEnabled = isMultilingualDefinitionExportEnabled;
        }

        public boolean writeLanguageSpecificFeatureValueDefs() {
            boolean result = true;
            result = this.getBoolean("export.language.definitions.include.language_specific_values", true);
            return result;
        }

        public boolean writeLanguageSpecificIgnoreFeatureDeds() {
            boolean result = true;
            result = this.getBoolean("export.language.definitions.include.ignore_feature", true);
            return result;
        }
        
    }

    private static final Logger LOGGER = Log.getLogger();
    private static final String EXTENSION = ".rex";
    private Configuration config = null;

    private File exporterConfigFile = null;

    public String getName() {
        LOGGER.entering(this.getClass().getName(), "getName");
        
        String result = "Regulus dictionary ...";

        LOGGER.exiting(this.getClass().getName(), "getName");
        return result;
    }

    public void handleExportRequest(Project project) {
        File file = promptForExportFile(project);
        if (file != null) {
            try {
                this.config = new Configuration(file);
                saveProject(project);
            } catch (Exception e) {
                StringWriter sw = new StringWriter ();
                PrintWriter pw = new PrintWriter(sw);
                
                pw.println("Failed to create exporter configuration:");
                e.printStackTrace(pw);
                pw.println("\nNo files written.");
                
                String errorMessage = sw.getBuffer().toString();
                
                JOptionPane.showMessageDialog(null, errorMessage, "Failed exporting target files", JOptionPane.ERROR_MESSAGE);
            }
        }
    }
    
    public void dispose() {
        // do nothing
    }
    
    public static void main(String[] args) {
        Application.main(args);
    }
    
    private File promptForExportFile(Project project) {
        exporterConfigFile = null;

        String name = project.getName();
        String proposedName = new File(name + EXTENSION).getPath();

        JFileChooser chooser = ComponentFactory.createFileChooser(proposedName, EXTENSION);

        if (chooser.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) {
            exporterConfigFile = chooser.getSelectedFile();
        }

        return exporterConfigFile;
     }
    
    
    
    /**
     * Writes out project's slots into the global multilingual definition file.
     * It contains slots (regulus features) for all languages.
     * 
     * This file contains REGULUS feature_value_space definitions,
     * and typically gets included by some regulus master file.
     * 
     * @param slotFile
     * @throws FileNotFoundException 
     */
    private void exportMultilingualDefinitions(Project project, PrintWriter writer) {
        LOGGER.entering(this.getClass().getName(), "exportMultilingualDefinitions(Project, PrintWriter)");

        Collection<DefaultSlot> allSlots = project.getKnowledgeBase().getSlots();

        LinkedList l = new LinkedList(allSlots);
        l.removeAll(config.getIgnoreFeatureList());

        Iterator<DefaultSlot> iter = allSlots.iterator();
        
        Collections.sort(l);

        writer.println("% **** Regulus Global Feature Value Space Definitions   ****");
        writer.println("% ****                  DO NOT EDIT                     ****");
        writer.println("% **** This file is autogenerated, and gets overwritten ****");
        writer.println("% **** the next time the project below is exported      ****");
        writer.println("% **** Source Project: " + project.getName() + " ****");
        writer.println("% **** Project File: " + project.getProjectURI() + " ****");
        writer.println("");
        
        dumpDefinitions(l, writer, "_value");

        writer.close();
        
        LOGGER.exiting(this.getClass().getName(), "exportMultilingualDefinitions");
    }
    
    private boolean isSlotExportable(final DefaultSlot slot){
        boolean result = false;
        
        result = ( (!slot.isSystem()) && config.isFeatureExportable(slot.getName()) );

        return result;
    }
    
    private void dumpDefinitions(Collection<DefaultSlot> collection, PrintWriter writer) {
        dumpDefinitions(collection, writer, "");
    }

    private void dumpDefinitions(Collection<DefaultSlot> collection, PrintWriter writer, final String suffix) {
        Iterator<DefaultSlot> i = collection.iterator(); 

        while(i.hasNext()){
            DefaultSlot slot = (DefaultSlot)i.next();

            if( isSlotExportable(slot) ) {
                int currentValueType = slot.getValueType().getIntValue();
                
                //
                // print out a warning and comment out definition if slot-name doesn't look prolog compatible
                //
                if(slot.getName().contains(" ") || slot.getName().contains("-")){
                    // TODO: collect warnings and present them after the export has been done
                    LOGGER.warning("Slot '" + slot.getName() + "' doesn't appear to be Prolog compatible.");
                    writer.print("% ");
                }
                if(currentValueType == ValueType.BOOLEAN.getIntValue()){
                    writer.println("feature_value_space(" + slot.getName() + suffix + ", [[true, false]]).");
                }
                else if( currentValueType == ValueType.SYMBOL.getIntValue()){
                    Collection languageSpecificValues = slot.getAllowedValues();
                    LinkedList ll = new LinkedList(languageSpecificValues);
                    Collections.sort(ll);
                    
                    String valueSpace = Utils.collectionToString(ll, ", ");

                    writer.println("feature_value_space(" + slot.getName() + suffix + ", [[" + valueSpace + "]]).");
                }
                else{
                    writer.println("feature_value_space(" + slot.getName() + suffix + ", [[" + slot.getAllowedValues() + "]]).");
                }
            }
        }
    }

    private void dumpLanguageSpecificDefinitions(Hashtable<Slot, Collection> languageSlotValueHash, PrintWriter writer, final String suffix) {
        LinkedList<Slot> l = new LinkedList(languageSlotValueHash.keySet());
        Collections.sort(l);
        Iterator<Slot> i = l.iterator(); 

        while(i.hasNext()){
            DefaultSlot slot = (DefaultSlot)i.next();

            if( isSlotExportable(slot) ) {
                int currentValueType = slot.getValueType().getIntValue();
                
                //
                // print out a warning and comment out definition if slot-name doesn't look prolog compatible
                //
                if(slot.getName().contains(" ") || slot.getName().contains("-")){
                    // TODO: collect warnings and present them after the export has been done
                    LOGGER.warning("Slot '" + slot.getName() + "' doesn't appear to be Prolog compatible.");
                    writer.print("% ");
                }
                if(currentValueType == ValueType.BOOLEAN.getIntValue()){
                    writer.println("feature_value_space(" + slot.getName() + suffix + ", [[true, false]]).");
                }
                else if( currentValueType == ValueType.SYMBOL.getIntValue()){
                    Collection languageSpecificValues = languageSlotValueHash.get(slot);
                    LinkedList ll = new LinkedList(languageSpecificValues);
                    Collections.sort(ll);
                    
                    String valueSpace = Utils.collectionToString(ll, ", ");

                    writer.println("feature_value_space(" + slot.getName() + suffix + ", [[" + valueSpace + "]]).");
                }
                else{
                    writer.println("feature_value_space(" + slot.getName() + suffix + ", [[" + slot.getAllowedValues() + "]]).");
                }
            }
        }
    }



    private void dumpLexicalEntries(PrintWriter writer, Collection<DefaultSimpleInstance> lexicalEntries){
        
        LinkedList<DefaultSimpleInstance> l = new LinkedList<DefaultSimpleInstance>(lexicalEntries);
        Collections.sort(l);
        
        for (Iterator iter = l.iterator(); iter.hasNext();) {

            String entryType = "";
            String surfaceForm = "";
            String surfaceFormLatinTransliteration = "";
            String surfaceFormGloss = "";
            StringBuffer lexicalFeatures = new StringBuffer();
            
            DefaultSimpleInstance lexicalEntry = (DefaultSimpleInstance)iter.next();

            Collection slots = lexicalEntry.getOwnSlots();
            
            String semanticVal = ""; 

                
            for (Iterator iterator = slots.iterator(); iterator.hasNext();) {
                DefaultSlot slot = (DefaultSlot) iterator.next();
                
                if (config.isDebuggingEnabled()) {
                    writer.println("% Slot: " + slot.getName() + "\tValue: " + lexicalEntry.getOwnSlotValue(slot));
                }
                
                if(! slot.isSystem()) {
                    if(slot.getName().equals(ENTRY_TYPE_STRING)) {
                        entryType = (String) lexicalEntry.getOwnSlotValue(slot);
                    }
                    else if(slot.getName().equals(SURFACE_FORM_STRING)) {
                        surfaceForm = (String) lexicalEntry.getOwnSlotValue(slot);
                    }
                    else if(slot.getName().equals(SURFACE_FORM_STRING_LATIN_TRANSLITERATION)){
                        surfaceFormLatinTransliteration = (String) lexicalEntry.getOwnSlotValue(slot);
                    }
                    else if(slot.getName().equals(SURFACE_FORM_STRING_GLOSS)){
                        surfaceFormGloss = (String) lexicalEntry.getOwnSlotValue(slot);
                    }
                    else if(slot.getName().equals(SEMANTIC_STRING)){
                        semanticVal = (String)lexicalEntry.getOwnSlotValue(slot);
                        if(semanticVal == null){
                            semanticVal = "Sem";
                        }
                    }
                    else if(slot.getName().equals(AGREEMENT_STRING)){
                        Collection slotValues = lexicalEntry.getOwnSlotValues(slot);
                        
                        if(! slotValues.isEmpty()){
                            String agreement = Utils.collectionToString(lexicalEntry.getOwnSlotValues(slot), ")\\/(");
                            agreement = "(" + agreement + ")";
                            agreement = agreement.replaceAll("_", "/\\\\");
                            agreement = agreement.replaceAll("\\\\3/\\\\sing", "\\\\(3/\\\\sing)");
                            lexicalFeatures.append(", agr=").append(agreement);
                        }
                    }
                    else{
                        if( slot.getAllowsMultipleValues() ){
                            Collection slotValues = lexicalEntry.getOwnSlotValues(slot);
                            if(! slotValues.isEmpty()) {
                                String slotValueString = Utils.collectionToString(slotValues, "\\/");
    
                                if(slotValues.size() == 1){
                                    lexicalFeatures.append(", ");
                                    lexicalFeatures.append(slot.getName()).append("=").append(slotValueString).append("");
                                }
                                else{
                                    //
                                    // TODO: make slot expose if it is conjuctive or  disjunctive
                                    // print out values in \/ or /\ format 
                                    //
                                    lexicalFeatures.append(", ");
                                    lexicalFeatures.append(slot.getName()).append("=").append(slotValueString).append("");
                                }
                            }
                        }
                        else{
                            lexicalFeatures.append(", ");
                            lexicalFeatures.append(slot.getName()).append("=").append(lexicalEntry.getOwnSlotValue(slot));
                        }
                    }
                }
            }
            
            String finalSurfaceForm = "";
            if(Utils.isStringEmpty(surfaceFormLatinTransliteration) && Utils.isStringEmpty(surfaceFormGloss)){
                finalSurfaceForm = surfaceForm;
            }
            else{
                if(surfaceFormGloss == null){
                    if(surfaceFormLatinTransliteration == null){
                        surfaceFormGloss = "";
                    }
                    else{
                        surfaceFormGloss = surfaceFormLatinTransliteration;
                    }
                }

                if(surfaceFormLatinTransliteration == null){
                    surfaceFormLatinTransliteration = lexicalEntry.getName();
                }
                
                finalSurfaceForm = "@" + config.getTransliterationMacro()
                + "('" + surfaceForm + "', '" + surfaceFormLatinTransliteration + "', '" + surfaceFormGloss + "')";
            }

            writer.println(entryType + ":[sem=[" + semanticVal + "]" + lexicalFeatures.toString() + "] --> " + finalSurfaceForm + ".");
        }
        
        if(!l.isEmpty()){
            writer.println("\n\n\n");
        }
    }

    private void exportLexicalEntries(Project project){
        LOGGER.entering(this.getClass().getName(), "exportLexicalEntries(Project)");

        LinkedList l = new LinkedList(project.getKnowledgeBase().getClses());
        Collections.sort(l);

        Iterator i = l.iterator();

        while (i.hasNext()) {
            Cls cls = (Cls) i.next();
            if (!cls.isIncluded()) {
                if(cls.getName().equals("Lexical Entry")){
                    //
                    // Iterate over all *direct* subclasses of "lexical entry"
                    //
                    LinkedList subClasses = new LinkedList(cls.getDirectSubclasses());
                    Collections.sort(subClasses);

                    for (Iterator iter = subClasses.iterator(); iter.hasNext();) {
                        Cls clzz = (Cls) iter.next();

                        Collection concreteLanguagesForCurrentCategory = clzz.getConcreteSubclasses();
                        Iterator j = concreteLanguagesForCurrentCategory.iterator();
                        while(j.hasNext()){
                            Cls concreteLangCategory = (Cls)j.next();
                            Collection<DefaultSimpleInstance> lexicalEntries = concreteLangCategory.getInstances();

                            String languageName = concreteLangCategory.getName();
                            languageName = languageName.substring(0, languageName.indexOf(' '));

                            //
                            // 'writer' has been opened when the '.rex' configuration file was loaded,
                            // and the corresponding language was listed in the export list 
                            //
                            PrintWriter writer = config.getLexiconPrintWriterForLanguage(languageName);
                            if( (writer != null) && (!lexicalEntries.isEmpty()) ){
                                writer.println("%");
                                writer.println("% " + concreteLangCategory.getName());
                                writer.println("%");
                                dumpLexicalEntries(writer, lexicalEntries);
                            }
                        }
                    }
                }
            }
        }

        LOGGER.exiting(this.getClass().getName(), "exportLexicalEntries");
    }
    
    
    private void exportLanguageSpecificDefinitions(Project project) {
        LOGGER.entering(this.getClass().getName(), "exportLanguageSpecificDefinitions(Project)");
        
        LinkedList l = new LinkedList(project.getKnowledgeBase().getClses());
        Collections.sort(l);
        
        //
        // Create structure for storing slotnames and collection of allowed slotvalues for each language
        //
        String[] languages = config.getLanguages();
        Hashtable<String, Hashtable<Slot,Collection> > languageSlotHashtable = new Hashtable<String, Hashtable<Slot,Collection> > ();
        
        for (int k = 0; k < languages.length; k++) {
            languageSlotHashtable.put(languages[k], new Hashtable<Slot,Collection>());
        }

        Iterator i = l.iterator();

        while (i.hasNext()) {
            Cls cls = (Cls) i.next();
            if (!cls.isIncluded()) {
                if(cls.getName().equals("Lexical Entry")){
                    //
                    // Iterate over all abstract subclasses of "lexical entry"
                    // and collect language specific slot values  
                    //
                    LinkedList<Cls> subClasses = new LinkedList(cls.getDirectSubclasses());
                    Collections.sort(subClasses);
                    
                    for (Iterator<Cls> iter = subClasses.iterator(); iter.hasNext();) {
                        Cls clzz = iter.next();
                        
                        if(clzz.isAbstract()) {
                            Collection<Cls> concreteLanguagesForCurrentCategory = clzz.getDirectSubclasses();
                            Iterator<Cls> j = concreteLanguagesForCurrentCategory.iterator();
                            while(j.hasNext()){
                                Cls languageClass = j.next();

                                String languageName = languageClass.getName();
                                languageName = languageName.substring(0, languageName.indexOf(' '));
                                LOGGER.finer("languageName = '" + languageName + "' (class '" + languageClass.getName() + "')");
                                
                                Hashtable<Slot,Collection> languageSlotValueHash = languageSlotHashtable.get(languageName);
                                if(languageSlotValueHash != null){
                                    Collection<Slot> c = languageClass.getTemplateSlots();
                                    
                                    Iterator<Slot> iterator = c.iterator();
                                    while (iterator.hasNext()) {
                                        Slot slot = iterator.next();
                                        Collection slotValues = languageClass.getTemplateSlotAllowedValues(slot);
                                        
                                        Collection storedSlotValues = languageSlotValueHash.get(slot);
                                        if(storedSlotValues != null){
                                            storedSlotValues.addAll(slotValues);
                                        }
                                        else{
                                            HashSet newValues = new HashSet();
                                            newValues.addAll(slotValues);
                                            languageSlotValueHash.put(slot, newValues);
                                        }
                                    }
                                }
                                else{
                                    LOGGER.finest("languageSlotValueHash is null ...");
                                }
                            }
                        }
                        else{
                            LOGGER.warning("Class '" + clzz.getName() + "' is not abstract and is therefore being ignored.");
                        }
                    }
                }
            }
        }
        
        // create collection of all user-defined slots -- useful for finding out which slots are ignored
        HashSet<DefaultSlot> allUserDefinedSlots = new HashSet<DefaultSlot>();
        Collection<DefaultSlot> allSlots = project.getKnowledgeBase().getSlots();
        Iterator<DefaultSlot> iter = allSlots.iterator();
        
        while(iter.hasNext()){
            DefaultSlot ds = iter.next();
            if(isSlotExportable(ds)){
                allUserDefinedSlots.add(ds);
            }
        }

        
        //
        // all slots have been collected into corresponding language-specific structures
        // time to write them out
        //
        Enumeration<String> languageEnum = languageSlotHashtable.keys();
        
        while(languageEnum.hasMoreElements()){
            String languageName = languageEnum.nextElement();
            Hashtable<Slot,Collection> languageSlotValueHash = languageSlotHashtable.get(languageName);

            //
            // Write out language-specific definition file
            //
            PrintWriter pw = config.getDefinitionPrintWriterForLanguage(languageName);

            pw.println("% **** Regulus Feature Value Space Definitions for '" + languageName + "' ****");
            pw.println("% ****                  DO NOT EDIT                     ****");
            pw.println("% **** This file is autogenerated, and gets overwritten ****");
            pw.println("% **** the next time the project below is exported      ****");
            pw.println("% **** Source Project: " + project.getName() + " ****");
            pw.println("% **** Project File: " + project.getProjectURI() + " ****");
            pw.println("");

            //
            // write out language-specific definitions
            //
            if( config.writeLanguageSpecificFeatureValueDefs() ) {
                pw.println("%\n% Language-Specific Feature Definitions\n%\n");
                dumpLanguageSpecificDefinitions(languageSlotValueHash, pw, "_value");
            }

            //
            // write out ignored feature list
            //
            if( config.writeLanguageSpecificIgnoreFeatureDeds() ) {
                HashSet<DefaultSlot> ignoredFeatures = new HashSet<DefaultSlot> (allUserDefinedSlots);
                ignoredFeatures.removeAll(languageSlotValueHash.keySet());
                
                LinkedList<DefaultSlot> lltmp = new LinkedList<DefaultSlot>(ignoredFeatures);
                Collections.sort(lltmp);
    
                Iterator<DefaultSlot> ifi = lltmp.iterator();
                
                pw.println("\n\n\n%\n% Ignored Features\n%\n");
    
                while(ifi.hasNext()){
                    DefaultSlot ignoredFeature = ifi.next();
                    pw.println("ignore_feature(" + ignoredFeature.getName() + ").");
                }
            }

            pw.close();

//            Iterator<Slot> slotIterator = languageSlots.iterator();
//            while(slotIterator.hasNext()){
//                Slot slot = slotIterator.next();
//            }
        }
        
        LOGGER.exiting(this.getClass().getName(), "exportLanguageSpecificDefinitions");
    }

    
    private void saveProject(Project project) {
        //
        // Create Feature-Value Space File
        //
        if(config.isMultilingualDefinitionExportEnabled) {
            exportMultilingualDefinitions(project, config.getGlobalDefinitionPrintWriter());
        }
        
        //
        // Create Language-Specific Definition Files
        //
        exportLanguageSpecificDefinitions(project);

        //
        // Dump Language-Specific Lexical Entries
        //
        exportLexicalEntries(project);
        
        //
        // Flush Output Streams
        //
        config.releaseResources();
    }
}
