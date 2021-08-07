import { newUnibeautify, Beautifier } from "unibeautify";
import beautifier from "../../src";
test("should successfully beautify JavaScript text", () => {
  const unibeautify = newUnibeautify();
  unibeautify.loadBeautifier(beautifier);
  const text = `function test(n){return n+1;}`;
  const beautifierResult = `function test(n) {
  return n + 1;
}`;
  return unibeautify
    .beautify({
      languageName: "JavaScript",
      options: {
        JavaScript: {
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