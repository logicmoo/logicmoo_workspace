import { newUnibeautify, Beautifier } from "unibeautify";
import beautifier from "../../src";
test(`should successfully beautify CSS text`, () => {
  const unibeautify = newUnibeautify();
  unibeautify.loadBeautifier(beautifier);
  const text = `.className1{font-size:12pt;}.className2{font-weight:bold;}`;
  const beautifierResult = `.className1 {\n\tfont-size: 12pt;\n}\n\n.className2 {\n\tfont-weight: bold;\n}`;
  return unibeautify
    .beautify({
      languageName: "CSS",
      options: {
        CSS: {
          indent_style: "tab",
          indent_size: 1,
        },
      },
      text,
    })
    .then(results => {
      expect(results).toBe(beautifierResult);
    });
});