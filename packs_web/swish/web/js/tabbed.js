/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2018, VU University Amsterdam
			      CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/**
 * @fileOverview
 * This file deals with tabbed panes.  It implements dynamic tabs on top
 * if Bootstrap.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define(["jquery", "form", "config", "preferences", "modal",
    "laconic", "search", "chatbell", "sourcelist"
  ],
  function($, form, config, preferences, modal) {

    var the_iframe_tab;
    var modes = [];
    /**
     * Suggests a mode based on the file extension present in the given path
     * @param {string} path The path to the file
     * @returns {object} Returns an object containing information about the
     *  suggested mode.
     */
    function getModeForPathAlt(path) {
      var mode = aceModesByNameOrExt.text;
      var fileName = path.split(/[\/\\]/).pop();
      for (var i = 0; i < modes.length; i++) {
        if (modes[i].supportsFile(fileName)) {
          mode = modes[i];
          break;
        }
      }
      return mode;
    }

    var Mode = function(name, caption, extensions) {
      this.name = name;
      this.caption = caption;
      this.mode = "ace/mode/" + name;
      this.extensions = extensions;
      var re;
      if (/\^/.test(extensions)) {
        re = extensions.replace(/\|(\^)?/g, function(a, b) {
          return "$|" + (b ? "^" : "^.*\\.");
        }) + "$";
      } else {
        re = "^.*\\.(" + extensions + ")$";
      }

      this.extRe = new RegExp(re, "gi");
    };

    Mode.prototype.supportsFile = function(filename) {
      return filename.match(this.extRe);
    };

    // todo firstlinematch
    var supportedAceModesInit = {
      ABAP: ["abap"],
      ABC: ["abc"],
      ActionScript: ["as"],
      ADA: ["ada|adb"],
      Apache_Conf: ["^htaccess|^htgroups|^htpasswd|^conf|htaccess|htgroups|htpasswd"],
      AsciiDoc: ["asciidoc|adoc"],
      Assembly_x86: ["asm|a"],
      AutoHotKey: ["ahk"],
      BatchFile: ["bat|cmd"],
      Bro: ["bro"],
      C_Cpp: ["cpp|c|cc|cxx|h|hh|hpp|ino"],
      C9Search: ["c9search_results"],
      Cirru: ["cirru|cr"],
      Clojure: ["clj|cljs"],
      Cobol: ["CBL|COB"],
      coffee: ["coffee|cf|cson|^Cakefile"],
      ColdFusion: ["cfm"],
      CSharp: ["cs"],
      CSS: ["css"],
      Curly: ["curly"],
      D: ["d|di"],
      Dart: ["dart"],
      Diff: ["diff|patch"],
      Dockerfile: ["^Dockerfile"],
      Dot: ["dot"],
      Drools: ["drl"],
      Dummy: ["dummy"],
      DummySyntax: ["dummy"],
      Eiffel: ["e|ge"],
      EJS: ["ejs"],
      Elixir: ["ex|exs"],
      Elm: ["elm"],
      Erlang: ["erl|hrl"],
      Forth: ["frt|fs|ldr|fth|4th"],
      Fortran: ["f|f90"],
      FTL: ["ftl"],
      Gcode: ["gcode"],
      Gherkin: ["feature"],
      Gitignore: ["^.gitignore"],
      Glsl: ["glsl|frag|vert"],
      Gobstones: ["gbs"],
      golang: ["go"],
      GraphQLSchema: ["gql"],
      Groovy: ["groovy"],
      HAML: ["haml"],
      Handlebars: ["hbs|handlebars|tpl|mustache"],
      haskell: ["hs|haskell|curry"],
      Haskell_Cabal: ["cabal"],
      haXe: ["hx"],
      Hjson: ["hjson"],
      // Add themissingmode"Django"toext-modelist
      Django: ["html"],
      HTML: ["html|htm|xhtml"],
      HTML_Elixir: ["eex|html.eex"],
      HTML_Ruby: ["erb|rhtml|html.erb"],
      INI: ["ini|conf|cfg|prefs"],
      Io: ["io"],
      Jack: ["jack"],
      Jade: ["jade|pug"],
      Java: ["java"],
      JavaScript: ["js|jsm|jsx"],
      JSON: ["json"],
      JSONiq: ["jq"],
      JSP: ["jsp"],
      JSX: ["jsx"],
      Julia: ["jl"],
      Kotlin: ["kt|kts"],
      LaTeX: ["tex|latex|ltx|bib"],
      LESS: ["less"],
      Liquid: ["liquid"],
      Lisp: ["lisp|pddl|l|subl|cl|clif|lsp|kif|cycl"],
      // dmiles:  + pddl + clif + lsp + kif + cycl
      LiveScript: ["ls"],
      LogiQL: ["logic|lql"],
      LSL: ["lsl"],
      Lua: ["lua"],
      LuaPage: ["lp"],
      Lucene: ["lucene"],
      Makefile: ["^Makefile|^GNUmakefile|^makefile|^OCamlMakefile|make"],
      Markdown: ["md|markdown"],
      Mask: ["mask"],
      MATLAB: ["matlab"],
      Maze: ["mz"],
      MEL: ["mel"],
      MUSHCode: ["mc|mush"],
      MySQL: ["mysql"],
      Nix: ["nix"],
      NSIS: ["nsi|nsh"],
      ObjectiveC: ["m|mm"],
      OCaml: ["ml|mli"],
      Pascal: ["pas|p"],
      Perl: ["perl|pm"],
      // dmiles pl -> perl
      pgSQL: ["pgsql"],
      PHP: ["php|phtml|shtml|php3|php4|php5|phps|phpt|aw|ctp|module"],
      Pig: ["pig"],
      Powershell: ["ps1"],
      Praat: ["praat|praatscript|psc|proc"],

      Razor: ["cshtml|asp"],
      Logtalk: ["lgt"],

      Prolog: ["plg|pro|pfc|plt|prolog|pl|chr|lgt|P|cpl|csp|dlv|asp|ec|lps|lpsw|lpst"],

      // dmiles: + pl + chr
      Properties: ["properties"],
      Protobuf: ["proto"],
      Python: ["py"],
      R: ["r"],
      RDoc: ["Rd"],
      RHTML: ["Rhtml"],
      RST: ["rst"],
      Ruby: ["rb|ru|gemspec|rake|^Guardfile|^Rakefile|^Gemfile"],
      Rust: ["rs"],
      SASS: ["sass"],
      SCAD: ["scad"],
      Scala: ["scala"],
      Scheme: ["scm|sm|rkt|oak|scheme"],
      SCSS: ["scss"],
      SH: ["sh|bash|^.bashrc"],
      SJS: ["sjs"],
      Smarty: ["smarty|tpl"],
      snippets: ["snippets"],
      Soy_Template: ["soy"],
      Space: ["space"],
      SQL: ["sql"],
      SQLServer: ["sqlserver"],
      Stylus: ["styl|stylus"],
      SVG: ["svg"],
      Swift: ["swift"],
      Tcl: ["tcl"],
      Tex: ["tex"],
      Text: ["txt"],
      Textile: ["textile"],
      Toml: ["toml"],
      TSX: ["tsx"],
      Twig: ["twig|swig"],
      Typescript: ["ts|typescript|str"],
      Vala: ["vala"],
      VBScript: ["vbs|vb"],
      Velocity: ["vm"],
      Verilog: ["v|vh|sv|svh"],
      VHDL: ["vhd|vhdl"],
      Wollok: ["wlk|wpgm|wtest"],
      XML: ["xml|rdf|rss|wsdl|xslt|atom|mathml|mml|xul|xbl|xaml|owl"],
      // dmiles: + owl
      XQuery: ["xq"],
      YAML: ["yaml|yml"]
    };

    var nameOverrides = {
      ObjectiveC: "Objective-C",
      CSharp: "C#",
      golang: "Go",
      C_Cpp: "C and C++",
      coffee: "CoffeeScript",
      HTML_Ruby: "HTML (Ruby)",
      HTML_Elixir: "HTML (Elixir)",
      FTL: "FreeMarker"
    };

    var aceModesByNameOrExt = {};

    for (var name in supportedAceModesInit) {
      var data = supportedAceModesInit[name];
      var dataExt = data[0].split("|");
      var displayName = (nameOverrides[name] || name).replace(/_/g, " ");
      var filename = name.toLowerCase();
      var mode = new Mode(filename, displayName, data[0]);
      aceModesByNameOrExt[filename] = mode;
      for (var ext in dataExt) {
        aceModesByNameOrExt[ext] = mode;
      }
      modes.push(mode);
    }

    var recurse = 0;

    // array containing all the editors we will create
    var aceEditors = [];

    var tabbed = {
      tabTypes: {},
      type: function(from) {

        recurse++;
        if (recurse > 10) {
          debugger;
          recurse = 0;
          return null;
        }

        if (from == "prolog") from = "pl";
        if (from == "program") from = "pl";

        var ret = null;
        var ext = from.split('.').pop();

        for (var k in tabbed.tabTypes) {
          if (tabbed.tabTypes.hasOwnProperty(k) &&
            tabbed.tabTypes[k].dataType == ext) {
            ret = tabbed.tabTypes[k];
            if (ret != undefined) {
              recurse--;
              return ret;
            }
            // return tabbed.tabTypes[k];
          }
        }
        // debugger;
        ret = tabbed.tabTypes[ext];
        if (ret != undefined) {
          recurse--;
          return ret;
        }

        var mode = getModeForPathAlt(from).mode

        var extUpC = ext.slice(0, 1).toUpperCase() + ext.slice(1, ext.length);

        if (!mode.endsWith("/prolog")) {
          //   if (true) return undefined;
          //   if (true) return tabbed.tabTypes.program;
        }

        tabbed.tabTypes[ext] = ret = {
          dataType: ext,
          typeName: ext,
          label: (extUpC + " Program"),
          contentType: "text/x-prolog",
          order: false,
          create: function(dom, options) {
            $(dom).addClass("prolog-editor")
              .prologEditor($.extend({
                placeholder: "Your " + extUpC + " program rules and facts go here ...",
                codeType: ext,
                save: true
              }, options))
              .prologEditor('makeCurrent', options);
          }

        };
        recurse--;
        return ret;
      }
    };

    tabbed.tabTypes.permalink = {
      dataType: "lnk",
      typeName: "program",
      label: "Program",
      create: function(dom, options) {
        $(dom).addClass("prolog-editor printable")
          .prologEditor($.extend({
            save: true
          }, options))
          .prologEditor('makeCurrent');
      }
    };


    function clone(obj) {
      var copy;

      // Handle the 3 simple types, and null or undefined
      if (null == obj || "object" != typeof obj) return obj;

      // Handle Date
      if (obj instanceof Date) {
        copy = new Date();
        copy.setTime(obj.getTime());
        return copy;
      }

      // Handle Array
      if (obj instanceof Array) {
        copy = [];
        for (var i = 0, len = obj.length; i < len; i++) {
          copy[i] = clone(obj[i]);
        }
        return copy;
      }

      // Handle Object
      if (obj instanceof Object) {
        copy = {};
        for (var attr in obj) {
          if (obj.hasOwnProperty(attr)) copy[attr] = clone(obj[attr]);
        }
        return copy;
      }

      throw new Error("Unable to copy obj! Its type isn't supported.");
    }

    (function($) {
      var pluginName = 'tabbed';
      var tabid = 0;

      /** @lends $.fn.tabbed */
      var methods = {
        /**
         * Turn the current element into a Bootstrap tabbed pane. All
         * children of the current element are changed into tabs.  The
         * child can control the mapping using:
         *
         *   - `data-label = "Label"`
         *   - `data-close = "disabled"`
         */
        _init: function(options) {
          options = options || {};

          return this.each(function() {
            var elem = $(this);
            var data = {}; /* private data */

            data.newTab = options.newTab;
            data.tabTypes = options.tabTypes || tabbed.tabTypes;
            elem.data(pluginName, data); /* store with element */

            elem.addClass("tabbed unloadable");
            elem.tabbed('makeTabbed');
            elem.on("trace-location", function(ev, prompt) {
              elem.tabbed('showTracePort', prompt);
            });
            elem.on("data-is-clean", function(ev, clean) {
              var tab = $(ev.target).closest(".tab-pane");
              var a = elem.tabbed('navTab', tab.attr('id'));

              if (a) {
                if (clean)
                  a.removeClass("data-dirty");
                else
                  a.addClass("data-dirty");
              }
            });
            elem.on("unload", function(ev) {
              if (ev.target == elem[0] &&
                elem.closest(".swish").swish('preserve_state')) {
                var state = elem[pluginName]('getState');
                localStorage.setItem("tabs", JSON.stringify(state));
              }
            });
            elem.on("restore", function(ev) {
              var state;

              if (ev.target == elem[0]) {
                try {
                  var str = localStorage.getItem("tabs");
                  if (str)
                    state = JSON.parse(str);
                } catch (err) {}

                if (state && typeof(state) == "object") {
                  elem[pluginName]('setState', state);
                }
              }
            });
            elem.on("preference", function(ev, pref) {
              if (pref.name == "preserve-state" &&
                pref.value == false) {
                localStorage.removeItem("tabs");
              }
            });
          });
        },

        /**
         * Turn the pane into a tabbed pane
         */
        makeTabbed: function() {
          var children = this.children();
          var ul = $.el.ul({
            class: "nav nav-tabs",
            role: "tablist"
          });
          var contents = $.el.div({
            class: "tab-content"
          });

          this.prepend(contents);
          this.prepend(ul);

          $(ul).on("click", "span.xclose", function(ev) {
            var id = $(ev.target).parent().attr("data-id");
            $(ev.target).parents(".tabbed").first().tabbed('removeTab', id);
            ev.preventDefault();
          });
          $(ul).on("click", "a", function(ev) {
            $(ev.target).closest("a").tab('show');
            ev.preventDefault();
          });

          /* Turn children into tabs */
          for (var i = 0; i < children.length; i++) {
            var child = $(children[i]);
            var id = genId();
            var label = child.attr("data-label") || "Unknown";
            var close = child.attr("data-close") != "disabled";
            var active = (i == children.length - 1); /* activate last */

            var li = this.tabbed('tabLabel', id, label, close);
            if (active)
              $(li).addClass("active");
            $(ul).append(li);
            $(contents).append(wrapInTab($(children[i]), id, active));
          }

          /* Create and handle "+" button */
          var create = $.el.a({
              class: "tab-new compact",
              title: "Open a new tab"
            },
            glyphicon("plus"));
          $(ul).append($.el.li({
            class: "tab-new",
            role: "presentation"
          }, create));
          $(create).on("click", function(ev) {
            var tabbed = $(ev.target).parents(".tabbed").first();

            tabbed.tabbed('newTab');
            ev.preventDefault();
            return false;
          });

          /* Handle tab-switching */
          $(ul).on("shown.bs.tab", "a", function(ev) {
            var newContentID = $(ev.target).data("id");
            $("#" + newContentID + " .swish-event-receiver").trigger("activate-tab");
            var s = $("#" + newContentID + " .storage")
            if(s.storage) {
			   $("#" + newContentID + " .storage").storage("activate");
            }
          });

          if (this.tabbed('navContent').children().length == 0) {
            this.tabbed('newTab');
          }
        },

        /**
         * Add an empty new tab from the "+" button.  This calls
         * options.newTab() to return a DOM element for the new
         * tab.
         * @param {HTMLElement} [content] Content for the new tab
         * If omitted, it calls `options.newTab` or uses the method
         * `tabSelect`.
         * @return {jQuery} object representing the created tab
         */
        newTab: function(dom, active) {
          var data = this.data(pluginName);

          if (dom == undefined) {
            if (data.newTab) {
              dom = data.newTab();
            } else {
              var sl;
              dom = this.tabbed('tabSelect');
              $(dom).append(this.tabbed('profileForm'),
                $.el.hr(),
                //this.tabbed('searchForm'),
                sl = $.el.div({
                  class: "sourcelist"
                }));
              // BEGIN FILE-EXPLORER
              var ss = $('<div><iframe id="the_iframe" src="/ef/elfinder.swish.html" width="100%" height="700px" /></div>');
              $(ss).css('height', $(window).height() * 0.80);
              // $(ss).css('height', "20%");
              $(ss).css('width', "100%");
              $(dom).append(ss);
              // END FILE-EXPLORER

              $(sl).sourcelist();
            }
          }

          if (active == undefined)
            active = true;

          return this.tabbed('addTab', dom, {
            active: active,
            close: true
          });
        },

        getState: function() {
          var state = this[pluginName]('get_ordered_storage').storage('getState');

          state.pathname = window.location.pathname;
          state.time = new Date().getTime();

          return state;
        },

        setState: function(state) {
          var elem = this;
          var fromURL = this.find(".storage").length > 0;

          for (var i = 0; i < state.tabs.length; i++) {
            var data = state.tabs[i];
            this[pluginName]('restoreTab', data, fromURL);
          }
        },

        restoreTab: function(data, fromURL) {
          var elem = this;
          var tab;

          data.query = null; /* null keeps query */
          data.noHistory = true; /* do not update window path */

          var existing = this.find(".storage").storage('match', data);
          if (existing) {
            existing.data('storage').url = data.url;
            tab = existing.closest(".tab-pane");
            elem.tabbed('move_right', tab);
          } else {
            tab = undefined;
          }

          function restoreData(into, from) {
            if (from.data) {
              into.find(".storage").storage('setValue', {
                data: from.data,
                role: 'source'
              });
            }
            if (from.chatroom) {
              into.find(".storage").storage('chat', from.chatroom);
            }
          }

          if (existing) {
            restoreData(tab, data);
          } else if (existing) {
            /* nothing to do? */
          } else {
            /* TBD: Centralise */
            var select = this.find("div.tabbed-select");
            var newtab;
            var restoring = '<div class="restore-tab">Restoring ' +
              (data.file || data.url) + " ..." +
              '</div>';

            if (select.length > 0) {
              newtab = select.first().closest(".tab-pane");
              newtab.html(restoring);
            } else {
              var active = (!fromURL && Boolean(data.active));
              newtab = elem.tabbed('newTab', $(restoring), active);
            }

            if (data.st_type == "gitty") {
              var url = config.http.locations.web_storage + data.file;
              $.ajax({
                url: url,
                type: "GET",
                data: {
                  format: "json"
                },
                success: function(reply) {
                  reply.url = url;
                  reply.st_type = "gitty";
                  reply.noHistory = true;
                  if (!elem.tabbed('setSource', newtab, reply)) {
                    console.log("Failed to restore", data.file);
                    elem.tabbed('removeTab', tab.attr("id"));
                  }
                  restoreData(newtab, data);
                  if (!fromURL && newtab.hasClass("active"))
                    newtab.find(".storage").storage("activate");
                },
                error: function(jqXHR) {
                  modal.ajaxError(jqXHR);
                }
              });
            } else if (data.url) {
              $.ajax({
                url: data.url,
                type: "GET",
                data: {
                  format: "json"
                },
                success: function(source) {
                  var msg;

                  if (typeof(source) == "string") {
                    msg = {
                      data: source
                    };
                    msg.st_type = "external";
                  } else if (typeof(source) == "object" &&
                    typeof(source.data) == "string") {
                    msg = source;
                    msg.st_type = "filesys";
                  } else {
                    alert("Invalid data");
                    return;
                  }
                  msg.noHistory = true;
                  msg.url = data.url;
                  if (!elem.tabbed('setSource', newtab, msg)) {
                    console.log("Failed to restore", data.url);
                    elem.tabbed('removeTab', newtab.attr("id"));
                  }
                  restoreData(newtab, data);
                  if (!fromURL && newtab.hasClass("active"))
                    newtab.find(".storage").storage("activate");
                },
                error: function(jqXHR) {
                  modal.ajaxError(jqXHR);
                }
              });
            } else {
              console.log("Cannot restore ", data);
            }
          }
        },


        /**
         * Add a new tab from the provided source.  If there is a _select_
         * (new) tab, open the data in this tab.
         */
        tabFromSource: function(src) {
          var elem = this;
          var select = this.find("div.tabbed-select");

          if (typeof(src) == "string")
            src = {
              data: src
            };

          function inNewTab() {
            var tab = elem.tabbed('newTab', $("<span></span>"));
            if (!elem.tabbed('setSource', tab, src)) {
              elem.tabbed('removeTab', tab.attr("id"));
            }
          }

          if (select.length > 0) {
            var tab = select.first().closest(".tab-pane");
            this.tabbed('show', tab.attr("id"));
            this.tabbed('setSource', tab, src);
          } else if (src.newTab || preferences.getVal("new-tab")) {
            inNewTab();
          } else {
            var tab;

            this.find(".storage").each(function(i, st) {
              if ($(st).storage('setSource', src)) {
                tab = $(st).closest(".tab-pane");
                return false;
              }
            });

            if (tab)
              this.tabbed('show', tab.attr("id"));
            else
              inNewTab();
          }

          return this;
        },

        /**
         * Transform the new tab into a tab that can hold the requested
         * source.
         * @return {Boolean} `true` if a suitable type was found
         */
        setSource: function(tab, src) {
          if (typeof(src) == "object" &&
            ((src.meta && src.meta.name) || src.url)) {
            var name = (src.meta && src.meta.name) ? src.meta.name : src.url;
            var tabType = tabbed.type(name);
            var options = {};

            if (src.noHistory)
              options.noHistory = true;
            var unknownType = (tabType == undefined || tabType.typeName == tabType.dataType);

            var mode = getModeForPathAlt(name);

            if (mode == aceModesByNameOrExt.prolog || (!unknownType && tabType != undefined)) {
              var content = $.el.div();
              tab.html("");
              tab.tabbed('title', tabType.label, tabType.dataType);
              tab.append(content);
              tabType.create(content, options);
              $(content).storage('setSource', src);
              tab.tabbed('title', name, tabType.label);
              return true;
            }

            var hasAceEditor = (mode != undefined && mode != aceModesByNameOrExt.text);

            if (preferences.getVal("monaco-editor") || tabType == undefined) {
              this.tabbed('newMonacoEditor', tab, name, src);
              tab.tabbed('title', name, tabType.label);
              //debugger;
              return true;
            }

            if (preferences.getVal("ace-editor") || hasAceEditor) {
              this.tabbed('newAceEditor', tab, name, src);
              tab.tabbed('title', name, tabType.label);
              //debugger;
              return true;
            }

            var content = $.el.div();
            tab.html("");
            tab.tabbed('title', tabType.label, tabType.dataType);
            tab.append(content);
            tabType.create(content, options);
            $(content).storage('setSource', src); {
              tab.tabbed('title', name, tabType.label);
              return true;
            }
          }

          return false;
        },

        newAceEditor: function(tab, name, src) {

          var tabUniqueId = new Date().getTime() + "" + Math.floor(Math.random() * 10000);
          var ssid = 'editor_' + tabUniqueId;
          var extra =
            `<form action=''><div id='file_controls' style='white-space: nowrap; color: #4245f4; font-weight: bold; font-size:90%;'>
            <strong> <label><input type='radio' name='runnermode' value='ignore'>Ignore</label>
            <label><input type='radio' name='runnermode' value='pengine' checked>Send</label>
            <label><input type='radio' name='runnermode' value='save_only'>Save</label>
            <label><input type='radio' name='runnermode' value='save_consult'>Reconsult</label>
            <label>(<input type='checkbox' name='even_inactive' value='true'>even if tab inactive)</label>
            &nbsp;&nbsp;Show:
            <label><input type='checkbox' name='show_line_numbers' value='true' CHECKED>line-numbers</label>
            <label><input type='checkbox' name='show_formating' value='true'>formating</label></strong></div><div id='ace_controls' style='white-space: nowrap; font-size:90%; display: none;' >
            <br>

            <label>Mode<select id='mode' size='1'></select></label>

            <label>Split<select id='split' size='1'>
            <option value='none'>None</option>
            <option value='below'>Below</option>
            <option value='beside'>Beside</option>
            </select></label>
            <label>Theme<select id='theme' size='1'></select></label>
            <label>Text
            <select id='fontsize' size='1'>
            <option value='10px'>10px</option>
            <option value='11px'>11px</option>
            <option value='12px' selected='selected'>12px</option>
            <option value='13px'>13px</option>
            <option value='14px'>14px</option>
            <option value='16px'>16px</option>
            <option value='18px'>18px</option>
            <option value='20px'>20px</option>
            <option value='24px'>24px</option>
            </select></label>
            <label for='folding'>Folding</label>

            <select id='folding' size='1'>
            <option value='manual'>manual</option>
            <option value='markbegin' selected='selected'>mark begin</option>
            <option value='markbeginend'>mark begin and end</option>
            </select>
            <label for='keybinding'>Keys</label>

            <select id='keybinding' size='1'>
            <option value='ace'>Ace</option>
            <option value='vim'>Vim</option>
            <option value='emacs'>Emacs</option>
            <option value='custom'>Custom</option>
            </select>
</div></form>`;

          tab.html('<div class="myeditor" id="' + ssid + '_other">' + extra + '</div>');
          var tital = name.split('/').pop();
          var newEditorElement = $('<div class="ureditor" id="' + ssid + '">' + src.data + '</div>');
          tab.append(newEditorElement);

          var mode = getModeForPathAlt(name).mode
          if (mode == null) {
            // mode = modelist.getModeForPath(name).mode
            if (mode == null) {
              mode = "ace/mode/lisp";
            }
          }

          // initialize the editor in the tab
          // var ED = ace;
          // debugger;
          var editor
          try {
            editor = ace.edit(ssid);
          } catch (err) {
            try {
              editor = ace.edit(newEditorElement);
            } catch (err) {
              editor = ace.edit("" + ssid + '_other');
            }
          }


          editor.setTheme("ace/theme/chrome");
          editor.renderer.setOption('showLineNumbers', true);
          editor.renderer.setShowGutter(true);
          editor.getSession().setUseWrapMode(false);

          //editorList.push({ id: tabUniqueId, instance: editor });

          if (mode != null) {
            tital = mode.split('/').pop() + ":" + tital;
            editor.getSession().setMode(mode);
            // $("<a href='"+src.url+"'>"+tital+"</a>")
          }
          tab.tabbed('title', tital, mode);
          // set the size of the editor
          newEditorElement.width('100%');
          newEditorElement.height('100%');

          // resize the editor
          editor.resize();
        },

        destroyEditor: function(prompt) {

          console.log('close a tab and destroy the ace editor instance');

          console.log($(this).parent());

          var tabUniqueId = $(this).parent().attr('data-tab-id');

          console.log(tabUniqueId);

          var resultArray = $.grep(editors, function(n, i) {
            return n.id === tabUniqueId;
          }, true);

          var editor = resultArray[0].instance;

          // destroy the editor instance
          editor.destroy();

          // remove the panel and panel nav dom
          $('#tabs').find('#panel_nav_' + tabUniqueId).remove();
          $('#tabs').find('#panel_' + tabUniqueId).remove();

        },

        newMonacoEditor: function(tab, name, src) {

          var tabUniqueId = new Date().getTime() + "" + Math.floor(Math.random() * 10000);
          var ssid = 'editor_' + tabUniqueId;

          // tab.html('<script src="/node_modules/monaco-editor/min/vs/loader.js"></script>');
          var tital = name.split('/').pop();
          var mode = getModeForPathAlt(name).mode
          if (mode != null) {
            tital = mode.split('/').pop() + ":" + tital;
          }


          var editor = null;
          tab.html('<div class="myeditor" id="' + ssid + '_other">' + "" + '</div>');
          tab.tabbed('title', tital, mode);
          var newEditorElement = $('<div id="' + ssid + '"></div>');
          // var newEditorElement = $('<div id="' + ssid + '" class="monaco-ele swish-event-receiver storage unloadable"></div>');

          tab.append(newEditorElement);

          // set the size of the editor
          newEditorElement.width('100%');
          newEditorElement.height('100%');

          // initialize the editor in the tab
          require.config({
            paths: {
              'vs': '/node_modules/monaco-editor/min/vs'
            }
          });

          var elem = this;

          require(['vs/editor/editor.main'], function() {

            monaco.editor.defineTheme('myTheme', {
              base: 'vs',
              inherit: true,
              rules: [{
                background: 'EDF9FA'
              }],
              colors: {
                'editor.foreground': '#000000',
                'editor.background': '#EDF9FA',
                'editorCursor.foreground': '#8B0000',
                'editor.lineHighlightBackground': '#0000FF20',
                'editorLineNumber.foreground': '#008800',
                'editor.selectionBackground': '#88000030',
                'editor.inactiveSelectionBackground': '#88000015'
              },
              rules: [{
                  token: 'comment',
                  foreground: 'ffa500',
                  fontStyle: 'italic underline'
                }, {
                  token: 'comment.js',
                  foreground: '008800',
                  fontStyle: 'bold'
                }, {
                  token: 'comment.css',
                  foreground: '0000ff'
                }, // will inherit fontStyle from `comment` above
                {
                  token: 'custom-info',
                  foreground: '808080'
                }, {
                  token: 'custom-error',
                  foreground: 'ff0000',
                  fontStyle: 'bold'
                }, {
                  token: 'custom-notice',
                  foreground: 'FFA500'
                }, {
                  token: 'custom-date',
                  foreground: '008800'
                }
              ]
            });

            // Register a completion item provider for the new language
            monaco.languages.registerCompletionItemProvider('mySpecialLanguage', {
              provideCompletionItems: () => {
                var suggestions = [{
                  label: 'simpleText',
                  kind: monaco.languages.CompletionItemKind.Text,
                  insertText: 'simpleText'
                }, {
                  label: 'testing',
                  kind: monaco.languages.CompletionItemKind.Keyword,
                  insertText: 'testing(${1:condition})',
                  insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet
                }, {
                  label: 'ifelse',
                  kind: monaco.languages.CompletionItemKind.Snippet,
                  insertText: [
                    'if (${1:condition}) {',
                    '\t$0',
                    '} else {',
                    '\t',
                    '}'
                  ].join('\n'),
                  insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                  documentation: 'If-Else Statement'
                }];
                return {
                  suggestions: suggestions
                };
              }
            });

            monaco.editor.setTheme('myTheme');
            var constr = {
              value: src.data,
              automaticLayout: true, // the important part
              language: "mySpecialLanguage",
              fontFamily: "Arial",
              fontSize: 20,
              lineNumbers: "off",
              roundedSelection: false,
              scrollBeyondLastLine: false,
              readOnly: false,
              glyphMargin: false,
              scrollbar: {
                // Subtle shadows to the left & top. Defaults to true.
                useShadows: false,

                // Render vertical arrows. Defaults to false.
                verticalHasArrows: true,
                // Render horizontal arrows. Defaults to false.
                horizontalHasArrows: true,

                // Render vertical scrollbar.
                // Accepted values: 'auto', 'visible', 'hidden'.
                // Defaults to 'auto'
                vertical: 'visible',
                // Render horizontal scrollbar.
                // Accepted values: 'auto', 'visible', 'hidden'.
                // Defaults to 'auto'
                horizontal: 'visible',

                verticalScrollbarSize: 17,
                horizontalScrollbarSize: 17,
                arrowSize: 30
              }
            };
            try {
              editor = monaco.editor.create(document.getElementById(ssid), constr)
            } catch (err) {
              elem.tabbed('newAceEditor', tab, name, src);
              return true;
            }
            // editor.setWidth('100%');


            // Explanation:
            // Press F1 (Alt-F1 in Edge) => the action will appear and run if it is enabled
            // Press Ctrl-F10 => the action will run if it is enabled
            // Press Chord Ctrl-K, Ctrl-M => the action will run if it is enabled

            editor.addAction({
              // An unique identifier of the contributed action.
              id: 'my-unique-id',

              // A label of the action that will be presented to the user.
              label: 'My Label!!!',

              // An optional array of keybindings for the action.
              keybindings: [
                monaco.KeyMod.CtrlCmd | monaco.KeyCode.F10,
                // chord
                monaco.KeyMod.chord(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KEY_K, monaco.KeyMod.CtrlCmd | monaco.KeyCode.KEY_M)
              ],

              // A precondition for this action.
              precondition: null,

              // A rule to evaluate on top of the precondition in order to dispatch the keybindings.
              keybindingContext: null,

              contextMenuGroupId: 'navigation',

              contextMenuOrder: 1.5,

              // Method that will be executed when the action is triggered.
              // @param editor The editor instance is passed in as a convinience
              run: function(ed) {
                alert("i'm running => " + ed.getPosition());
                return null;
              }
            });

            if (mode != null) {
              tital = mode.split('/').pop() + ":" + tital;
              //  editor.getSession().setMode(mode);
              // $("<a href='"+src.url+"'>"+tital+"</a>")
            }
            tab.tabbed('title', tital, mode);
            // set the size of the editor
            newEditorElement.width('100%');
            newEditorElement.height('100%');

            // resize the editor
            newEditorElement.resize;

            return newEditorElement;
          });
        },


        /**
         * Show a tracer port. This implies finding the proper editor,
         * making sure it is visible and ask it to show to port or, if
         * no editor is displaying this source, create a new one.
         * @param {Object} prompt
         * @param {Object} [prompt.source]
         * @param {Object} [prompt.source.file] is the file associated
         * with the debug event.  Currently, we accept
         *
         *   - `pengine://<pengine>/src` refers to the editor that provided
         *     the source for pengine <pengine>
         *	 - `swish://<file>.pl` refers to an included file from the
         *	   store.
         */
        showTracePort: function(prompt) {
          if (prompt && prompt.source && prompt.source.file) {
            var file = prompt.source.file;
            var pengineID, store;
            var editors;

            function isPengineSrc() {
              var id;

              if (file.startsWith("pengine://"))
                return file.split("/")[2];
            }

            function isStoreSrc() {
              var prefix = "swish://";
              if (file.startsWith(prefix))
                return file.slice(prefix.length);
            }

            if ((pengineID = isPengineSrc())) {
              editors = this.find(".prolog-editor")
                .filter(function(i, e) {
                  return $(e).prologEditor('pengine', {
                    has: pengineID
                  });
                });
            } else if ((store = isStoreSrc())) {
              editors = this.find(".storage")
                .storage('match', {
                  file: store
                });

              if (!editors) {
                this.closest(".swish")
                  .swish('playFile', {
                    file: store,
                    newTab: true,
                    noHistory: true,
                    prompt: prompt
                  });
                return this;
              }
            }

            if (editors)
              editors.prologEditor('showTracePort', prompt);
          }

          return this;
        },


        /**
         * Add a new tab using content
         * @param {Object} content is the DOM node to use as content for the
         * tab.
         * @param {Object} options
         * @param {Boolean} [options.active] if `true`, make the new tab
         * active
         * @param {Boolean} [options.close] if `true`, allow closing the new
         * tab.
         * @return {jQuery} the created tab element
         */
        addTab: function(content, options) {
          var ul = this.tabbed('navTabs');
          var id = genId();
          var tab = wrapInTab(content, id, options.active);
          
          this.tabbed('navContent').append(tab);

          var li = this.tabbed('tabLabel', id, "New tab", close, "select");

          var create = ul.find("a.tab-new");
          if (create.length == 1)
            $(li).insertBefore(create.first().parent());
          else
            ul.append(li);

          // debugger;

          if (options.active)
            $(li).find("a").first().tab('show');

          return tab;
        },

        /**
         * Remove tab with given Id. If the tab is the active tab, make the
         * previous tab active, or if there is no previous, the next. If the
         * tabbed environment becomes empty, add a virgin tab.
         *
         * @param {String} id is the id of the tab to destroy
         */
        removeTab: function(id) {
          var li = this.tabbed('navTabs').find("a[data-id='" + id + "']").parent();
          var tab = $("#" + id);
          var new_active;

          if (tab.find(".storage").storage('unload', "closetab") == false)
            return;

          if (tab.is(":visible"))
            new_active = li.prev() || li.next();
          li.remove();
          /* HACK: close embedded runners */
          tab.find(".prolog-runner").prologRunner('close');
          tab.find(".storage").storage('close');
          tab.remove();
          if (new_active && new_active.length > 0) {
            new_active.find("a").first().tab('show');
          } else if (this.tabbed('navContent').children().length == 0) {
            this.tabbed('newTab');
          }

          $(".storage").storage('chat_status', true);
        },

        /**
         * Show indicated tab.
         * @param {String} id is the id of the tab to show.
         */
        show: function(id) {
          var a = this.tabbed('navTab', id);
          if (a) {
            a.tab('show');
          }

          $(".storage").storage('chat_status', true);
        },

        /**
         * Move the argument tab or tab id to the right of all
         * tabs.
         */
        move_right: function(tab) {
          var id;
          var ul = this.find(">ul");

          if (typeof(tab) == "string")
            id = tab;
          else
            id = tab.attr('id');

          ul.find("a[data-id=" + id + "]")
            .closest("li")
            .insertBefore(ul.children().last());
        },

        /**
         * Create a label (`li`) for a new tab.
         * @param {String} id is the identifier of the new tab
         * @param {String} label is the textual label of the new tab
         * @param {Boolean} close determines whether or nor a close button
         * is added to the tab.
         * @param {String} [type="pl"] indicates the type of the tab. This
         * is used for associating an icon with the tab.
         */
        tabLabel: function(id, label, close, type) {
          var close_button;
          var chat;
          if (close) {
            close_button = glyphicon("remove", "xclose");
            $(close_button).attr("title", "Close tab");
          }
          type = type || "pl";

          var a1 = $.el.a({
              class: "compact",
              href: "#" + id,
              "data-id": id
            },
            $.el.span({
              class: "tab-icon type-icon " + type
            }),
            $.el.span({
              class: "tab-dirty",
              title: "Tab is modified. " +
                "See File/Save and Edit/View changes"
            }),
            chat = $.el.a({
              class: 'tab-chat'
            }),
            $.el.span({
              class: "tab-title"
            }, label),
            close_button);
          var li = $.el.li({
            role: "presentation"
          }, a1);

          $(chat).chatbell()
            .on("click", function(ev) {
              var id = $(ev.target).closest("a.compact").data("id");
              $("#" + id).find(".storage").storage('chat');
              return false;
            });

          return li;
        },

        /**
         * Calling obj.tabbed('anchor') finds the <a> element
         * representing the tab label from the node obj that appears
         * somewhere on the tab
         */
        anchor: function() {
          var tab = this.closest(".tab-pane");

          if (tab.length == 0) {
            return undefined; /* e.g., fullscreen mode */
          }

          var tabbed = tab.closest(".tabbed");
          var id = tab.attr("id");
          var ul = tabbed.tabbed('navTabs');
          var a = ul.find("a[data-id=" + id + "]");

          return a;
        },

        /**
         * Find the storage objects in the tabbed environment in the
         * order of the tabs.  Note that the content divs maye be ordered
         * differently.
         */
        get_ordered_storage: function() {
          var elem = this;
          var result = [];

          this.find(">ul>li").each(function() {
            var id = $(this).find(">a").data('id');
            elem.find(">div.tab-content>div[id=" + id + "] .storage").each(function() {
              result.push(this);
            });
          });

          return $(result);
        },

        /**
         * This method is typically _not_ called on the tab, but on some
         * inner element of the tab.  It changes the title of the tab.
         * @param {String} title is the new title for the tab.
         * @param {String} [type="pl"] is the new type for the tab.
         */
        title: function(title, type) {
          var a = this.tabbed('anchor');
		  var slashed = title.split('/');
		  title = slashed[slashed.length-1];
          if (a) {
            a.find(".tab-title").text(title);
            if (type) {
              var icon = a.find(".tab-icon");
              icon.removeClass();
              icon.addClass("tab-icon type-icon " + type);
            }
          }

          return this;
        },

        /**
         * Set the chat message feedback for this tab
         * @param {Object} [chats]
         * @param {Number} [chats.count] number of available chat messages
         * on the document.
         */
        chats: function(chats) {
          var a = this.tabbed('anchor');

          if (a) {
            a.find(".chat-bell").chatbell('update', chats);
          }

          return this;
        },

        /**
         * Increment the chat count and possibly associate the bell
         * with the document identifier.
         * @param {String} [docid] is the document identifier to associate
         * with.
         */
        'chats++': function(docid) {
          var a = this.tabbed('anchor');

          if (a) {
            a.find(".chat-bell").chatbell('chats++', docid);
          }

          return this;
        },


        /**
         * Default empty tab content that allows the user to transform
         * the tab into the desired object.
         * @return {Object} containing content for the new tab
         */
        tabSelect: function() {
          var data = this.data(pluginName);
          var dom = $.el.div({
              class: "tabbed-select"
            },
            $.el.div({
                class: "tabbed-create"
              },
              $.el.label({
                  class: "tabbed-left"
                },
                "Create a "),
              g = $.el.div({
                class: "btn-group",
                role: "group"
              }),
              $.el.label({
                class: "tabbed-right"
              }, "here")));
          var types = [];

          for (var k in data.tabTypes) {
            if (data.tabTypes.hasOwnProperty(k) &&
              data.tabTypes[k].order)
              types.push(k);
          }
          types.sort(function(a, b) {
            return data.tabTypes[a].order - data.tabTypes[b].order;
          });

          for (var i = 0; i < types.length; i++) {
            var type = data.tabTypes[types[i]];

            $(g).append($.el.button({
                type: "button",
                class: "btn btn-primary",
                "data-type": type.typeName,
                "data-ext": type.dataType
              },
              type.label));
          }

          $(g).on("click", ".btn", function(ev) {
            var type = $(ev.target).data('type');
            var tab = $(ev.target).closest(".tab-pane");
            var content = $.el.div();
            var options = $.extend({}, tabbed.tabTypes[type]);
            var profile = tab.find("label.active > input[name=profile]").val();

            if (profile) {
              options.profile = profile;
              options.value = tab.tabbed('profileValue', profile,
                tabbed.tabTypes[type].dataType);
              if (options.value != undefined)
                preferences.setVal("default-profile", profile);
            }

            tab.html("");
            tab.tabbed('title', options.label, options.dataType);
            tab.append(content);
            tabbed.tabTypes[type].create(content, options);
          });
          $(g).addClass("swish-event-receiver");
          $(g).on("download save fileInfo print", function(ev) {
            var tab = $(ev.target).closest(".tab-pane");
            if (tab.is(":visible")) {
              var typelabel = {
                "download": "you wish to download",
                "save": "you wish to save",
                "print": "you wish to print",
                "fileInfo": "for which you want details"
              };

              modal.alert("Please activate the tab " + typelabel[ev.type]);
              ev.stopPropagation();
            }
          });
          $(g).on("profile-selected", function(ev, profile) {
            $(ev.target).find("button").each(function() {
              $(this).prop('disabled',
                profile.type.indexOf($(this).data('ext')) < 0);
            });
          });

          return dom;
        },

        /**
         * Find sources
         */
        searchForm: function() {
          var sform = $.el.form({
              class: "search-sources"
            },
            $.el.label({
              class: "control-label"
            }, 'Open source file containing'),
            $.el.div({
                class: "input-group"
              },
              $.el.input({
                type: "text",
                class: "form-control search",
                placeholder: "Search sources",
                'data-search-in': "sources store_content",
              }),
              $.el.div({
                  class: "input-group-btn"
                },
                $.el.button({
                    class: "btn btn-default",
                    type: "submit"
                  },
                  $.el.i({
                    class: "glyphicon glyphicon-search"
                  })))),
            $.el.div({
                class: "input-group"
              },
              form.fields.radio("smatch",
                [{
                  label: "Start of line",
                  value: "sol"
                }, {
                  label: "Start of word",
                  value: "sow",
                  active: true
                }, {
                  label: "Anywhere",
                  value: "anywhere"
                }])));
          $(sform).find("input.search").search();

          return sform;
        },

        sourceList: function() {


        },

        profileForm: function() {
          if (config.swish.profiles && config.swish.profiles.length > 0) {
            var def;

            for (var i = 0; i < config.swish.profiles.length; i++) {
              delete config.swish.profiles[i].active;
            }

            if ((def = preferences.getVal("default-profile"))) {
              for (var i = 0; i < config.swish.profiles.length; i++) {
                if (config.swish.profiles[i].value == def)
                  config.swish.profiles[i].active = true
              }
            } else {
              config.swish.profiles[0].active = true;
            }

            var pform =
              $.el.div({
                  class: "tabbed-profile"
                },
                $.el.label({
                  class: "tabbed-left"
                }, "based on"),
                $.el.div({
                    class: "input-group select-profile"
                  },
                  form.fields.radio("profile", config.swish.profiles)),
                $.el.label({
                  class: "tabbed-right"
                }, "profile"));

            $(pform).on("click", function(ev) {
              var select = $(ev.target).find("input").val();
              var profile = profileObject(select);
              $(ev.target).closest(".tab-pane")
                .find(".tabbed-create .btn-group")
                .trigger("profile-selected", profile);
            });

            return pform;
          }
        },

        profileValue: function(name, ext) {
          var url = config.http.locations.swish + "profile/" + name + "." + ext;
          return $.ajax({
            url: url,
            type: "GET",
            data: {
              format: "raw"
            },
            async: false,
            error: function(jqXHR) {
              modal.ajaxError(jqXHR);
            }
          }).responseText;
        },

        /**
         * Get the UL list that represents the nav tabs
         */
        navTabs: function() {
          return this.find("ul.nav-tabs").first();
        },

        navTab: function(id) {
          var a = this.find("ul.nav-tabs").first().find("a[data-id='" + id + "']");
          if (a.length > 0)
            return a;
        },

        navContent: function() {
          return this.find("div.tab-content").first();
        }
      }; // methods

      /**
       * Wrap a content element in a Bootstrap tab content.
       * @param {Object} dom is the object that must be wrapped
       * @param {String} id is the identifier to give to the new content
       * @param {Boolean} active sets the tab to active if `true`
       * @return {jQuery} `div` object of class `tab-pane` and the
       * passed `id`.
       */
      function wrapInTab(dom, id, active) {
        $(dom).wrap('<div role="tabpanel" class="tab-pane" id="' + id + '"></div>');
        var wrapped = $(dom).parent();

        if (active)
          wrapped.addClass("active");

        return wrapped;
      }

      function glyphicon(glyph, className) {
        var span = $.el.span({
          class: "glyphicon glyphicon-" + glyph
        });

        if (className)
          $(span).addClass(className);

        return span;
      }

      function genId() {
        return "tabbed-tab-" + tabid++;
      }

      function profileObject(name) {
        if (config.swish.profiles) {
          for (var i = 0; i < config.swish.profiles.length; i++) {
            if (config.swish.profiles[i].value == name)
              return config.swish.profiles[i];
          }
        }
      }

      /**
       * <Class description>
       *
       * @class tabbed
       * @tutorial jquery-doc
       * @memberOf $.fn
       * @param {String|Object} [method] Either a method name or the jQuery
       * plugin initialization object.
       * @param [...] Zero or more arguments passed to the jQuery `method`
       */

      $.fn.tabbed = function(method) {
        if (methods[method]) {
          return methods[method]
            .apply(this, Array.prototype.slice.call(arguments, 1));
        } else if (typeof method === 'object' || !method) {
          return methods._init.apply(this, arguments);
        } else {
          $.error('Method ' + method + ' does not exist on jQuery.' + pluginName);
        }
      };
    }(jQuery));

    return tabbed;
  });
