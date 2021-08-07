import { newUnibeautify, Beautifier } from "unibeautify";
import beautifier from "../../src";
test("should successfully beautify JSON text", () => {
  const unibeautify = newUnibeautify();
  unibeautify.loadBeautifier(beautifier);
  const text = `{"one":1,"two":[],"three":[3,"three"]}`;
  const beautifierResult = `{\n  "one": 1,\n  "two": [],\n  "three": [3, "three"]\n}`;
  return unibeautify
    .beautify({
      languageName: "JSON",
      options: {
        JSON: {
          indent_style: "space",
          indent_size: 2,
        },
      },
      text,
    })
    .then(results => {
      expect(results).toBe(beautifierResult);
    });
});
