import { newUnibeautify, Beautifier } from "unibeautify";
import beautifier from "../../src";
test("should successfully beautify HTML text", () => {
  const unibeautify = newUnibeautify();
  unibeautify.loadBeautifier(beautifier);
  const text = `<div>\n<span>Hello</span>\n<span>World</span>\n</div>`;
  const beautifierResult = `<div>\n  <span>Hello</span>\n  <span>World</span>\n</div>`;
  return unibeautify
    .beautify({
      languageName: "HTML",
      options: {
        HTML: {
          indent_char: " ",
          indent_size: 2
        }
      },
      text
    })
    .then(results => {
      expect(results).toBe(beautifierResult);
    });
});