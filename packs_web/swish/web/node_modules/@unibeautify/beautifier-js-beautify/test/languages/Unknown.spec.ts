import { newUnibeautify, Beautifier, Language } from "unibeautify";
import beautifier from "../../src";
test(`should fail as an unknown language to JS Beautify`, () => {
  const unibeautify = newUnibeautify();
  const testLanguage: Language = {
    atomGrammars: [],
    extensions: ["test"],
    name: "TestLanguage",
    namespace: "test",
    since: "0.0.1",
    sublimeSyntaxes: [],
    vscodeLanguages: []
  };
  unibeautify.loadLanguage(testLanguage);
  const testBeautifier = {
    ...beautifier,
    options: {
      [testLanguage.name]: true,
    }
  };
  unibeautify.loadBeautifier(testBeautifier);
  expect(unibeautify
    .beautify({
      languageName: testLanguage.name,
      options: {},
      text: "",
    })
  ).rejects.toThrowError(`Unknown language for JS Beautify: ${testLanguage}`);
});