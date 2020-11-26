
package lib;

/**
 *
 * @author Baioni, Grandi, Tallevi Diotallevi
 */

import java.io.IOException;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.xml.sax.SAXException;


public class XMLValidator {
  
  /**
   * Validatore di un file XML tramite il file schema xml/game.xsd
   * @param fileNameToValidate
   * @throws SAXException
   */
  public void Validate(String fileNameToValidate) throws SAXException {
    
      String schemaLang = "http://www.w3.org/2001/XMLSchema";
      SchemaFactory factory = SchemaFactory.newInstance(schemaLang);

      Schema schema = factory.newSchema(new StreamSource("xml/game.xsd"));
      Validator validator = schema.newValidator();
        try {
            validator.validate(new StreamSource(fileNameToValidate));
        } catch (IOException ex) {
            ex.printStackTrace();
        }
  }
}