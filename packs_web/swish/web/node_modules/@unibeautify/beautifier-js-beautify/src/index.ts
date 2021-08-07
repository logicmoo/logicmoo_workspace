import { js_beautify, html_beautify, css_beautify } from "js-beautify";
import { Beautifier, Language, BeautifierBeautifyData, DependencyType, NodeDependency } from "unibeautify";
import * as readPkgUp from "read-pkg-up";
import options from "./options";
const { pkg } = readPkgUp.sync({ cwd: __dirname });

interface JSBeautify {
  js_beautify(js_source_text: string, options?: JsBeautifyOptions): string;
  css_beautify(js_source_text: string, options?: CSSBeautifyOptions): string;
  html_beautify(js_source_text: string, options?: HTMLBeautifyOptions): string;
}

export const beautifier: Beautifier = {
  name: "JS-Beautify",
  package: pkg,
  dependencies: [
    {
      type: DependencyType.Node,
      name: "JS Beautify",
      package: "js-beautify",
    }
  ],
  options: {
    // HTML
    HTML: options.HTML,
    XML: options.HTML,
    Handlebars: options.HTML,
    Mustache: options.HTML,
    Liquid: options.HTML,
    // JavaScript
    JavaScript: options.JavaScript,
    EJS: options.JavaScript,
    JSX: options.JavaScript,
    JSON: options.JSON,
    JSON5: options.JSON,
    // CSS
    CSS: options.CSS
  },
  beautify({ text, options, language, dependencies }: BeautifierBeautifyData) {
    return new Promise((resolve, reject) => {
      const jsbeautify: JSBeautify = dependencies.get<NodeDependency>("JS Beautify").package;
      try {
        switch (language.name) {
          case "JSON":
          case "JSON5":
          case "JavaScript":
            return resolve(jsbeautify.js_beautify(text, options));
          case "JSX":
            options.e4x = true;
            options.es4 = true;
            return resolve(jsbeautify.js_beautify(text, options));
          case "Handlebars":
          case "Mustache":
            options.indent_handlebars = true;
            return resolve(jsbeautify.html_beautify(text, options));
          case "EJS":
          case "Liquid":
          case "HTML":
          case "XML":
            return resolve(jsbeautify.html_beautify(text, options));
          case "CSS":
            return resolve(jsbeautify.css_beautify(text, options));
          default:
            throw (
              new Error("Unknown language for JS Beautify: " + language)
            );
        }
      } catch (error) {
        return reject(error);
      }
    });
  }
};

export default beautifier;
