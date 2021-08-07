import { newUnibeautify, Beautifier } from "unibeautify";
import beautifier from "../../src";
test(`should successfully beautify Handlebars text`, () => {
  const unibeautify = newUnibeautify();
  unibeautify.loadBeautifier(beautifier);
  const text = `<script id="hello" type= "text/x-handlebars-template" ><div><h1>{{title}}</h1>\n{{body}}\n</div></script>`;
  const beautifierResult = `<script id="hello" type="text/x-handlebars-template">\n\t<div>\n\t\t<h1>{{title}}</h1>\n\t\t\
{{body}}\n\t</div>\n</script>`;
  return unibeautify
    .beautify({
      languageName: "Handlebars",
      options: {
        Handlebars: {
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
