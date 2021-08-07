import { newUnibeautify, Beautifier } from "unibeautify";
import beautifier from "../../src";
test("should successfully beautify JSX text", () => {
  const unibeautify = newUnibeautify();
  unibeautify.loadBeautifier(beautifier);
  const text = `export default class TestCase extends React.Component {
    render() {
      return ( <div className={this.props.className} someAttr>
        <div>Smth</div>
      </div> );
    }
  }`;
  const beautifierResult = `export default class TestCase extends React.Component {
  render() {
    return (<div className={this.props.className} someAttr>
        <div>Smth</div>
      </div>);
  }
}`;
  return unibeautify
    .beautify({
      languageName: "JSX",
      options: {
        JSX: {
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
test("should successfully beautify JSX text with wrap_line_length", () => {
  const unibeautify = newUnibeautify();
  unibeautify.loadBeautifier(beautifier);
  const text = `export default class TestCase extends React.Component {
    render() {
      return ( <div className={this.props.className} someAttr>
        <div>Smth</div>
      </div> );
    }
  }`;
  const beautifierResult = `export default class TestCase extends React
  .Component {
    render() {
      return (
        <div className={this.props.className} someAttr>
        <div>Smth</div>
      </div>
      );
    }
  }`;
  return unibeautify
    .beautify({
      languageName: "JSX",
      options: {
        JSX: {
          indent_char: " ",
          indent_size: 2,
          wrap_line_length: 40
        }
      },
      text
    })
    .then(results => {
      expect(results).toBe(beautifierResult);
    });
});
