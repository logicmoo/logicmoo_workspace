% Rules for removing TeX commands in order to produce plain ASCII output

% Ordered list of defaults, strongest first.

orthography_rewrite("\\_\\-", "_").
orthography_rewrite("$\\langle$", "<").
orthography_rewrite("$\\rangle$", ">").
orthography_rewrite("\\begin", "").
orthography_rewrite("\\end", "").
orthography_rewrite("\\item", "- ").
orthography_rewrite("{verbatim}", "").
orthography_rewrite("{itemize}", "").
orthography_rewrite("{figure}", "").
orthography_rewrite("\\tt", "").
orthography_rewrite("\\ref", "").
orthography_rewrite("\\caption", "").
orthography_rewrite("\\label", "").
orthography_rewrite("{", "").
orthography_rewrite("}", "").
orthography_rewrite("\\_", "_").
