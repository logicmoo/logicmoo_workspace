import { newUnibeautify, Beautifier, Unibeautify } from "unibeautify";
import beautifier from "../../src";
testWithWrapLineLength(12);
testWithWrapLineLength(20);
testWithWrapLineLength(80);
testWithWrapLineLength(120);

function testWithWrapLineLength(wrapLineLength: number) {
  test(`should successfully beautify JavaScript text with wrap_line_length=${wrapLineLength}`, () => {
    const unibeautify = newUnibeautify();
    unibeautify.loadBeautifier(beautifier);

    const shortString = "c";
    const veryLongString = "c".repeat(Math.ceil(wrapLineLength / 2));

    const shortText = `["${shortString}", "${shortString}"];`;
    const shortBeautifierResult = shortText;

    if (shortText.length > wrapLineLength) {
      throw new Error(`Test text will always wrap. Please use a printWidth value greater than ${shortText.length}.`);
    }
    const longText = `["${veryLongString}", "${veryLongString}"];`;
    const longBeautifierResult = `["${veryLongString}",\n  "${veryLongString}"\n];`;

    const indentSize = 2;
    return Promise.all([
      beautifyWithPrintWidth(unibeautify, shortText, wrapLineLength).then(results => {
        expect(results).toBe(shortBeautifierResult);
      }),
      beautifyWithPrintWidth(unibeautify, longText, wrapLineLength).then(results => {
        expect(results).toBe(longBeautifierResult);
      })
    ]);
  });
}
function beautifyWithPrintWidth(unibeautify: Unibeautify, text: string, printWidth: number) {
  const indentSize = 2;
  return unibeautify.beautify({
    languageName: "JavaScript",
    options: {
      JavaScript: {
        indent_char: " ",
        indent_size: indentSize,
        end_with_comma: false,
        wrap_line_length: printWidth
      }
    },
    text
  });
}
