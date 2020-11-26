<a href="" id="top"></a>
<span dir="auto">KE-Text</span>
===============================

From Public Domain Knowledge Bank

Jump to: [navigation](#column-one), [search](#searchInput)

<span id="KE_Text" class="mw-headline">KE Text</span>
=====================================================

The *KE Text* facilities allow authors to compose a set of KB changes in text and add them to a <a href="/wiki/index.php?title=Cyc_KB&amp;action=edit&amp;redlink=1" class="new" title="Cyc KB (page does not exist)">Cyc KB</a> in a single batch operation.

E-Mail Comments to: info@cyc.com.

CopyrightÂ© 1996 â€“ 2015 Cycorp. All rights reserved.

Contents
--------

-   [<span class="tocnumber">1</span> <span class="toctext">KE Text</span>](#KE_Text)
    -   [<span class="tocnumber">1.1</span> <span class="toctext">Introduction</span>](#Introduction)
    -   [<span class="tocnumber">1.2</span> <span class="toctext">KE Text Syntax</span>](#KE_Text_Syntax)
        -   [<span class="tocnumber">1.2.1</span> <span class="toctext">Notation</span>](#Notation)
            -   [<span class="tocnumber">1.2.1.1</span> <span class="toctext">Variables</span>](#Variables)
            -   [<span class="tocnumber">1.2.1.2</span> <span class="toctext">Constants</span>](#Constants)
            -   [<span class="tocnumber">1.2.1.3</span> <span class="toctext">Strings</span>](#Strings)
            -   [<span class="tocnumber">1.2.1.4</span> <span class="toctext">Keywords</span>](#Keywords)
            -   [<span class="tocnumber">1.2.1.5</span> <span class="toctext">Symbols</span>](#Symbols)
        -   [<span class="tocnumber">1.2.2</span> <span class="toctext">Expressions</span>](#Expressions)
        -   [<span class="tocnumber">1.2.3</span> <span class="toctext">Directives</span>](#Directives)
            -   [<span class="tocnumber">1.2.3.1</span> <span class="toctext">Reserved Words</span>](#Reserved_Words)
                -   [<span class="tocnumber">1.2.3.1.1</span> <span class="toctext">General</span>](#General)
                -   [<span class="tocnumber">1.2.3.1.2</span> <span class="toctext">Enter and Delete</span>](#Enter_and_Delete)
                -   [<span class="tocnumber">1.2.3.1.3</span> <span class="toctext">Rename</span>](#Rename)
                -   [<span class="tocnumber">1.2.3.1.4</span> <span class="toctext">Constant</span>](#Constant)
                -   [<span class="tocnumber">1.2.3.1.5</span> <span class="toctext">In MT</span>](#In_MT)
            -   [<span class="tocnumber">1.2.3.2</span> <span class="toctext">Default MT</span>](#Default_MT)
            -   [<span class="tocnumber">1.2.3.3</span> <span class="toctext">Truth Value</span>](#Truth_Value)
            -   [<span class="tocnumber">1.2.3.4</span> <span class="toctext">Direction</span>](#Direction)
            -   [<span class="tocnumber">1.2.3.5</span> <span class="toctext">Formula</span>](#Formula)
            -   [<span class="tocnumber">1.2.3.6</span> <span class="toctext">Enter</span>](#Enter)
            -   [<span class="tocnumber">1.2.3.7</span> <span class="toctext">Delete</span>](#Delete)
            -   [<span class="tocnumber">1.2.3.8</span> <span class="toctext">Rename</span>](#Rename_2)
            -   [<span class="tocnumber">1.2.3.9</span> <span class="toctext">Kill</span>](#Kill)
            -   [<span class="tocnumber">1.2.3.10</span> <span class="toctext">Include</span>](#Include)
        -   [<span class="tocnumber">1.2.4</span> <span class="toctext">Predicate Directives</span>](#Predicate_Directives)
        -   [<span class="tocnumber">1.2.5</span> <span class="toctext">Special Handling for TheAssertionSentence</span>](#Special_Handling_for_TheAssertionSentence)
        -   [<span class="tocnumber">1.2.6</span> <span class="toctext">Comments in KE Text</span>](#Comments_in_KE_Text)
        -   [<span class="tocnumber">1.2.7</span> <span class="toctext">Order of Expressions</span>](#Order_of_Expressions)
        -   [<span class="tocnumber">1.2.8</span> <span class="toctext">Another Example</span>](#Another_Example)
        -   [<span class="tocnumber">1.2.9</span> <span class="toctext">Processing KE Text in the Cyc Web Interface</span>](#Processing_KE_Text_in_the_Cyc_Web_Interface)
        -   [<span class="tocnumber">1.2.10</span> <span class="toctext">Loading a KE Test File from a Console Interactor</span>](#Loading_a_KE_Test_File_from_a_Console_Interactor)
-   [<span class="tocnumber">2</span> <span class="toctext">Appendix A: KE Text History</span>](#Appendix_A:_KE_Text_History)
    -   [<span class="tocnumber">2.1</span> <span class="toctext">Appendix B: KE Text Syntax</span>](#Appendix_B:_KE_Text_Syntax)

<span id="Introduction" class="mw-headline">Introduction</span>
---------------------------------------------------------------

KE Text (Knowledge E Text, where the E stands for Editing, which is historically correct, or Entry, which is intuitively correct) is an ASCII text format for specifying changes to a <a href="/wiki/index.php?title=Cyc_KB&amp;action=edit&amp;redlink=1" class="new" title="Cyc KB (page does not exist)">Cyc KB</a>. The text is parsed into KB operations (<a href="/wiki/index.php?title=Cyc-Assert&amp;action=edit&amp;redlink=1" class="new" title="Cyc-Assert (page does not exist)">asserts</a>, <a href="/wiki/index.php?title=Cyc-Unassert&amp;action=edit&amp;redlink=1" class="new" title="Cyc-Unassert (page does not exist)">unasserts</a>, <a href="/wiki/index.php?title=Cyc-Rename&amp;action=edit&amp;redlink=1" class="new" title="Cyc-Rename (page does not exist)">renames</a>, etc.) that are then evaluated using the <a href="/wiki/index.php?title=KE_API&amp;action=edit&amp;redlink=1" class="new" title="KE API (page does not exist)">KE API</a> (<a href="/wiki/index.php?title=Ke-assert&amp;action=edit&amp;redlink=1" class="new" title="Ke-assert (page does not exist)">ke-assert</a>, <a href="/wiki/index.php?title=Ke-unassert&amp;action=edit&amp;redlink=1" class="new" title="Ke-unassert (page does not exist)">ke-unassert</a>, <a href="/wiki/index.php?title=Ke-rename&amp;action=edit&amp;redlink=1" class="new" title="Ke-rename (page does not exist)">ke-rename</a>, etc.) KE Text is processed through the browser through either the <a href="/wiki/index.php?title=Load_KE_File&amp;action=edit&amp;redlink=1" class="new" title="Load KE File (page does not exist)">Load KE File</a> page, which loads a KE Text file or the <a href="/wiki/index.php?title=Compose_KE_Text&amp;action=edit&amp;redlink=1" class="new" title="Compose KE Text (page does not exist)">Compose KE Text</a> page, which allows typing in KE Text.

The KE Text facilities allow authors to compose a set of KB changes in text and add them to a Cyc KB in a single batch operation. Choosing to use KE Text is strictly a matter of convenience. Operations entered using KE Text do not differ in the KB from those entered via other browser tools. Most users find it convenient in situations where many similar changes need to be made or when they are adding a related set of new constants and assertions.

<span id="KE_Text_Syntax" class="mw-headline">KE Text Syntax</span>
-------------------------------------------------------------------

KE Text syntax is just a syntactic/notational variation of <a href="/wiki/index.php?title=CycL&amp;action=edit&amp;redlink=1" class="new" title="CycL (page does not exist)">CycL</a>. Many people find it easier and faster to write some types of CycL expressions using KE Text syntax than using â€œcanonicalâ€ <a href="/wiki/index.php?title=FOPC-in-Lisp&amp;action=edit&amp;redlink=1" class="new" title="FOPC-in-Lisp (page does not exist)">FOPC-in-Lisp</a> CycL syntax, which partly explains why KE Text syntax continues to be supported by knowledge entry tools.

-   Notation
-   Expressions
-   Directives
-   Comments in KE Text
-   Order of Expressions
-   Another Example
-   Processing KE Text in the Cyc Web Interface
-   Loading a KE Test File from a Console Interactor

### <span id="Notation" class="mw-headline">Notation</span>

#### <span id="Variables" class="mw-headline">Variables</span>

Variables occurring anywhere in a KE text (e.g., inside rule statements) must begin with a question mark (?). This convention signals the knowledge entry facilities to treat the object as a variable, rather than as a constant.

#### <span id="Constants" class="mw-headline">Constants</span>

It is not necessary to include the initial â€œ\#$â€ in references to known constants (i.e., constants which Cyc already knows to exist), though it is not disallowed. For example, one could use either [\#$Siegel](#.24Siegel) or <a href="/wiki/index.php?title=Siegel&amp;action=edit&amp;redlink=1" class="new" title="Siegel (page does not exist)">Siegel</a>; [\#$KeithsHouse](#.24KeithsHouse) or <a href="/wiki/index.php?title=KeithsHouse&amp;action=edit&amp;redlink=1" class="new" title="KeithsHouse (page does not exist)">KeithsHouse</a>. Accepted practice is to write KE text without the â€œ\#$â€ prefix.

#### <span id="Strings" class="mw-headline">Strings</span>

Strings referred to in KE text (such as entries on the [\#$comment](#.24comment) predicate for a constant) must be delimited by double quotes (e.g., â€œThis is a string.â€), as in <a href="/wiki/index.php?title=COMPUTER_LANGUAGE_COMMON_LISP&amp;action=edit&amp;redlink=1" class="new" title="COMPUTER LANGUAGE COMMON LISP (page does not exist)">Common Lisp</a> and <a href="/wiki/index.php?title=COMPUTER_LANGUAGE_C&amp;action=edit&amp;redlink=1" class="new" title="COMPUTER LANGUAGE C (page does not exist)">C</a>.

To use double quotes inside comments, prefix them with a backslash. (e.g. â€œThis comment \\â€contains\\â€ double quotes.â€)

KE File preserves tab and return/newline/linefeed characters that occur inside strings, but will remove any non-printing characters.

#### <span id="Keywords" class="mw-headline">Keywords</span>

Keywords occurring in formulas must be prefixed with a colon.

(e.g. â€œf: (<a href="/wiki/index.php?title=GenKeyword&amp;action=edit&amp;redlink=1" class="new" title="GenKeyword (page does not exist)">genKeyword</a> <a href="/wiki/index.php?title=PublicConstant&amp;action=edit&amp;redlink=1" class="new" title="PublicConstant (page does not exist)">PublicConstant</a>Â :PUBLIC-CONSTANT).â€)

#### <span id="Symbols" class="mw-headline">Symbols</span>

Symbols occuring in formulas must be prefixed with a single quote.

(e.g. â€œf: (<a href="/wiki/index.php?title=AfterAdding&amp;action=edit&amp;redlink=1" class="new" title="AfterAdding (page does not exist)">afterAdding</a> <a href="/wiki/index.php?title=Genls&amp;action=edit&amp;redlink=1" class="new" title="Genls (page does not exist)">genls</a> â€˜<a href="/wiki/index.php?title=GENLS-AFTER-ADDING&amp;action=edit&amp;redlink=1" class="new" title="GENLS-AFTER-ADDING (page does not exist)">GENLS-AFTER-ADDING</a>).â€)

### <span id="Expressions" class="mw-headline">Expressions</span>

A complete, meaningful syntactic unit of Cyc KE text is an â€œexpressionâ€.

Expressions in KE Text syntax are somewhat analogous to sentences in a natural language, or more closely, to expressions in a programming language such as <a href="/wiki/index.php?title=COMPUTER_LANGUAGE_COMMON_LISP&amp;action=edit&amp;redlink=1" class="new" title="COMPUTER LANGUAGE COMMON LISP (page does not exist)">Lisp</a> or <a href="/wiki/index.php?title=COMPUTER_LANGUAGE_JAVA&amp;action=edit&amp;redlink=1" class="new" title="COMPUTER LANGUAGE JAVA (page does not exist)">Java</a>. In <a href="/wiki/index.php?title=COMPUTER_LANGUAGE_JAVA&amp;action=edit&amp;redlink=1" class="new" title="COMPUTER LANGUAGE JAVA (page does not exist)">Java</a>, the end of an expression is indicated by a semi-colon (;). In <a href="/wiki/index.php?title=COMPUTER_LANGUAGE_COMMON_LISP&amp;action=edit&amp;redlink=1" class="new" title="COMPUTER LANGUAGE COMMON LISP (page does not exist)">Lisp</a>, the end of an expression is indicated by a right parenthesis that balances a corresponding left parenthesis. In KE Text syntax, each expression must end with a period (.), and the period must be outside a comment or a string. The general form of an expression in KE Text syntax is as follows:

     <directive>: <data-object-or-object-sequence>. 

### <span id="Directives" class="mw-headline">Directives</span>

There are two types of objects which may fill the position in a KE text expression: reserved words and predicates.

-   Reserved Words
-   Predicate Directives
-   Special Handling for <a href="/wiki/index.php?title=TheAssertionSentence&amp;action=edit&amp;redlink=1" class="new" title="TheAssertionSentence (page does not exist)">TheAssertionSentence</a>

#### <span id="Reserved_Words" class="mw-headline">Reserved Words</span>

The first type comprises reserved words (analogous to reserved words in a programming language), which are as follows:

-   Constant
-   In Mt
-   Default Mt
-   Truth Value (or TV)
-   Direction (or D)
-   Formula
-   Enter
-   Delete
-   Rename
-   Kill
-   Include

##### <span id="General" class="mw-headline">General</span>

The syntax for all reserved words except Enter, Delete, and Rename is the same. Each reserved word is followed by a colon delimiter, exactly one data object, and a period. That is, the form of a reserved word expression in KE Text syntax is:

       <reserved-word>: <data-object>.

Note that reserved word directive names are not case-sensitive. For example, â€œconstantâ€ works just as well as â€œConstantâ€.

##### <span id="Enter_and_Delete" class="mw-headline">Enter and Delete</span>

Enter and Delete have the following syntax:

      <reserved-word>. 

##### <span id="Rename" class="mw-headline">Rename</span>

Rename has the following syntax:

       <reserved-word> <old-constant-name> .

##### <span id="Constant" class="mw-headline">Constant</span>

If the reserved word is â€œConstantâ€, the data object following the colon delimiter must be the name of a Cyc constant (e.g., Pittman, or MarksHouse, or some other Cyc constant). For example:

        Constant: Pittman.
        Constant: Muffet.

If the data object following the colon delimiter is not already known (by Cyc) to be a Cyc constant, users will be asked whether they want to create a new constant with that name.

When an expression beginning with a Constant directive is evaluated, it causes the default entry constant (the â€œcurrentâ€ constant) to be set to the named constant, the default truth value to be set toÂ :unknown, the default direction to also be set toÂ :unknown, and the default entry <a href="/wiki/index.php?title=Microtheory&amp;action=edit&amp;redlink=1" class="new" title="Microtheory (page does not exist)">microtheory</a> to be set to the <a href="/wiki/index.php?title=BaseKB&amp;action=edit&amp;redlink=1" class="new" title="BaseKB (page does not exist)">BaseKB</a>. The only exception to this is if the <a href="/wiki/index.php?title=Microtheory&amp;action=edit&amp;redlink=1" class="new" title="Microtheory (page does not exist)">microtheory</a> has previously been set via the Default Mt directive, in which case the use of the Constant directive leaves the microtheory unchanged. All of the settings made by the *Constant* directive persist until they are changed by some other (implicit or explicit) directive.

##### <span id="In_MT" class="mw-headline">In MT</span>

If the reserved word is â€œIn Mtâ€, the data object following the colon delimiter must be a known (i.e., already existing) microtheory.

Example:

    In Mt: HumanActivitiesMt.

When an expression beginning with an In Mt directive is evaluated, it causes the default entry microtheory to be set to the named microtheory. This setting persists until the next occurrence of an *In Mt* directive, a *Default Mt* directive, or a *Constant* directive.

#### <span id="Default_MT" class="mw-headline">Default MT</span>

If the reserved word is â€œDefault Mtâ€, the data object following the colon delimiter must be a known (i.e., already existing) microtheory.

For example:

    Default Mt: HumanActivitiesMt.

When an expression beginning with a Default Mt directive is evaluated, is causes the entry/delete microtheory to be set to the named microtheory. This setting persists until the next occurrence of a Default Mt or In Mt directive, or the end of the file/text being processed. Once a directive has been overriden by another directive, the scope of the original directive is no longer valid, even if the second directive is itself overriden later. Note that this directive, unlike the In Mt directive, prevents each occurrence of a Constant directive from resetting the default microtheory to BaseKB. This directive makes it easier to process all (or most) of the expressions in a file/text segment in the same microtheory.

#### <span id="Truth_Value" class="mw-headline">Truth Value</span>

If the reserved word is â€œTruth Valueâ€ (or â€œTVâ€), the data object following the colon delimiter must be one of the keywordsÂ :default,Â :monotonic, orÂ :unknown. (Itâ€™s also OK to omit the colon).

(Check the glossary for a quick description of the difference between default true and monotonically true.)

Examples:

    Truth Value:Â :monotonic.
     TV: monotonic.

It should only very rarely be necessary for a user to use a Truth Value directive. KE Text assigns truth values to entry expressions automatically. If an entry expression begins with a predicate which is an instance of <a href="/wiki/index.php?title=DefaultMonotonicPredicate&amp;action=edit&amp;redlink=1" class="new" title="DefaultMonotonicPredicate (page does not exist)">DefaultMonotonicPredicate</a> (including <a href="/wiki/index.php?title=Isa&amp;action=edit&amp;redlink=1" class="new" title="Isa (page does not exist)">isa</a> and <a href="/wiki/index.php?title=Genls&amp;action=edit&amp;redlink=1" class="new" title="Genls (page does not exist)">genls</a>), the expression is automatically assigned a truth value ofÂ :monotonic. All other entry expressions are automatically assigned a truth value ofÂ :default. Note that the strength of â€œimpliesâ€ isÂ :default, by default.The only reason to use a Truth Value directive is if you want to override these built-in defaults.

When an expression beginning with a Truth Value directive is evaluated, it causes the entry truth value for the following expression to be set to the indicated truth value. (Note that an expression might comprise several assertions. This will be explained more fully below). After the expression to be entered is evaluated, the setting immediately reverts toÂ :unknown, until the next entry expression is encountered (and the truth value is automatically set toÂ :default orÂ :monotonic), or until another Truth Value directive is read.

When would you want to use this directive? Suppose you wanted to enter a rule with a truth value ofÂ :monotonic. Since the default setting for rules isÂ :default, in your KE text you would want to precede the rule with this expression:

    Truth Value:Â :monotonic.

#### <span id="Direction" class="mw-headline">Direction</span>

If the reserved word is â€œDirectionâ€ (or â€œDâ€), the data object following the colon delimiter must be one of the keywordsÂ :forward (to indicate forward propagation),Â :backward (to indicate backward propagation), orÂ :unknown. (Itâ€™s also OK to omit the colon).

Examples:

     Direction:Â :forward.
     d: backward.

KE Text assigns directions to entry expressions automatically. Expressions beginning with a simple predicate (<a href="/wiki/index.php?title=Ground_atomic_formula&amp;action=edit&amp;redlink=1" class="new" title="Ground atomic formula (page does not exist)">ground atomic formulas</a>) are assigned a direction ofÂ :forward. All other entry expressions (most notably, all rules are assigned a direction ofÂ :backward.

The only reason to use a *Direction* directive is if you want to override these built-in defaults.

When an expression beginning with a *Direction* directive is evaluated, it causes the direction for the 'following' entry expression to be set to the indicated direction. After the expression to be entered is read, the setting immediately reverts toÂ :unknown, until the next entry expression is encountered or another Direction directive is read.

The most common use for this directive is to enter rules with a direction ofÂ :forward.

#### <span id="Formula" class="mw-headline">Formula</span>

If the reserved word is â€œ*F*â€ (for â€œ*formula*â€), the data object following the colon delimiter must be a well-formed CycL Formula.

The constants referred to in the CycL formula must already be known to Cyc (i.e., must already exist, perhaps as a result of being created at some previous point in the KE text).

(KE Text will also accept the directive â€œ*EL*â€ to specify a <a href="/wiki/index.php?title=CycL_Formula&amp;action=edit&amp;redlink=1" class="new" title="CycL Formula (page does not exist)">CycL Formula</a>. â€œ*EL*â€ stands for â€œ*Epistemological Level*â€, as distinct from â€œ*HL*â€, which stands for â€œ*Heuristic Level*â€.)

Examples:

    F: (implies

    (isaÂ ?cat Tiger)

    (hasVisibleSurfacePatternTypeÂ ?cat StripedPattern)).

    F:  (holdsIn (Year 1995) (owns Goolsbey KeithsHouse)).

    F: (likesAsFriend SimoneSiegel KathyBurns).

#### <span id="Enter" class="mw-headline">Enter</span>

If the reserved word is â€œ**Enter**â€, it must be followed by a period. All of the expressions following this reserved word until an occurrence of the reserved word â€œDeleteâ€ (or the end of the file/text) will be processed assuming that the resulting assertions should be entered into the Cyc KB.

For example:

    Enter.
     FÂ : (holdsIn (Year 1995) (owns Goolsbey KeithsHouse)).

The default processing mode for the KE Text facilities (KE File, Compose) is entry mode, so you donâ€™t have to use this reserved word unless you want to start entering assertions after a region of text in which the processing mode was set to delete (see below).

#### <span id="Delete" class="mw-headline">Delete</span>

If the reserved word is â€œDeleteâ€, it must be followed by a period. All of the expressions following this reserved word until an occurrence of the reserved word â€œEnterâ€ (or the end of the file/text) will be processed assuming that the resulting assertions should be removed from the Cyc KB.

For example:

    Delete.

    F: (holdsIn (Year 1996) (owns Goolsbey KeithsOldCar)).

    Enter.

    F: (holdsIn (Year 1996) (owns Goolsbey KeithsNewCar)).

Since the default processing mode for the KE Text facilities (KE File, Compose) is entry mode, you must use use the â€œDeleteâ€ reserved word if you want to remove assertions from the Cyc KB by processing KE Text. If the assertion to be removed is a local assertion, the method (FI function) used is KE-UNASSERT. If the assertion to be removed is a remote assertion, the method used is KE-BLAST. Exercise caution in using this directive.

#### <span id="Rename_2" class="mw-headline">Rename</span>

If the reserved word is â€œRenameâ€, the colon delimiter must be followed by a constant name, a string indicating the new name, and a period.

For example:

    Rename: NicksFirstKid â€œSimoneSiegelâ€.

The Rename directive provides a convenient way to do a batch of constant renames in a file/text.

A renamed constant does not become the â€œcurrentâ€ constant.

#### <span id="Kill" class="mw-headline">Kill</span>

If the reserved word is â€œKillâ€, the colon delimiter must be followed by the name of the constant to be killed.

For example:


    Kill: highestVolcanoInRegion.

#### <span id="Include" class="mw-headline">Include</span>

If the reserved word is â€œIncludeâ€, the colon delimiter must be followed by the name of the file to be included in quotes.

For example:

    Include: â€œanother-file.keâ€.

The above line will look for the file named another-file.ke in the same directory as the KE Text file being loaded. If the KE Text is being typed into a compose window, you can use the absolute path to the file you want to include. For example:

    Include: â€œ/home/user5/ke/another-file.keâ€.

### <span id="Predicate_Directives" class="mw-headline">Predicate Directives</span>

The second type of directive comprises Cyc predicates occurring within the scope of a (previously occurring) Constant directive. The Constant directive sets the â€œcurrentâ€ constant, which then is understood to be the first argument to assertions generated from the following predicate directive expressions.

(Note that predicate directive names, unlike reserved word directive names, are case-sensitive. After all, a predicate directive name is just the name of a CycL predicate, and CycL constant names are case-sensitive.)

Each predicate directive is followed by a colon delimiter, one or more data objects, and a period. That is, the form of a predicate expression in KE Text syntax is

&lt;predicate&gt;: &lt;data-object-1&gt; \[&lt;data-object-2&gt;â€¦&lt;data-object-n&gt;\].

The data objects following the colon delimiter comprise the additional argument(s) to the predicate in the predicate directive.

Example:

Constant: Goolsbey.

isa: HumanCyclist ElectricalEngineer.

feelsTowardsObject: (SimoneSiegel Affection Positive)

(BillJ Curiosity Positive).

comment: â€œKeith Goolsbey is a member of the Cycorp technical board.â€.

In this example, the Constant directive sets the â€œcurrentâ€ constant to be Goolsbey. Goolsbey is then assumed to be the first argument to assertions formed from the three following predicate directive expressions (the expressions which begin with â€œisaâ€, â€œfeelsTowardsObjectâ€, and â€œcommentâ€).

If the predicate directive is the name of a binary predicate (such as isa and comment), each of the data objects following the colon delimiter is assumed to be part of an assertion in which the predicate directive is the predicate, the default constant is the first argument, and the data object is the second argument.

If the predicate directive is the name of an n-ary predicate where n is greater than 2 (such as \#$feelsTowardsObject), each of the data objects following the colon delimiter must be a list. The elements in the list are assumed to be part of an assertion in which the predicate directive is the predicate, the default constant is the first argument, and the elements (in listed order) are the remaining arguments. So, when evaluated and processed, the KE text fragment in the example above would result in the addition of the following six assertions to the KB:

(\#$isa \#$Goolsbey \#$HumanCyclist)

(\#$isa \#$Goolsbey \#$ElectricalEngineer)

(\#$feelsTowardsObject \#$Goolsbey \#$SimoneSiegel \#$Affection \#$Positive)

(\#$feelsTowardsObject \#$Goolsbey \#$BillJ \#$Curiosity \#$Positive)

(\#$comment \#$Goolsbey â€œKeith Goolsbey is a member of the Cycorp technical board.â€)

Note that because any number of data objects may follow a colon delimiter preceded by a predicate directive, one KE text expression may result in several assertions being added to the knowledge base. Any reserved word directive immediately preceding such a compound KE text expression (i.e., an expression yielding more than one assertion) will apply to all of the assertions resulting from the expression.

Also, note that since a â€œcanonicalâ€ CycL Formula can be entered in KE text by using the F directive, the assertions resulting from the expressions in the example above are exactly the same as the assertions resulting from the expressions in the example immediately below.

Example:


    F: (isa Goolsbey HumanCyclist).

    F: (isa Goolsbey ElectricalEngineer).

    F: (feelsTowardsObject Goolsbey SimoneSiegel Affection Positive).

    F: (feelsTowardsObject Goolsbey BillJ Curiosity Positive).

    F: (comment Goolsbey â€œKeith Goolsbey is a member of the Cycorp technical board.â€).

If Cyc had the unary predicate â€œdogâ€, indicating membership in the class of all dogs (or the quality of â€œdognessâ€), assertions using this predicate could be entered with an expression such as this:

    dog: Brandy .

    F: (dog Brandy) .

### <span id="Special_Handling_for_TheAssertionSentence" class="mw-headline">Special Handling for <a href="/wiki/index.php?title=TheAssertionSentence&amp;action=edit&amp;redlink=1" class="new" title="TheAssertionSentence (page does not exist)">TheAssertionSentence</a></span>

The constant <a href="/wiki/index.php?title=TheAssertionSentence&amp;action=edit&amp;redlink=1" class="new" title="TheAssertionSentence (page does not exist)">TheAssertionSentence</a> has special support for referring to the previously asserted sentence, not including previous sentences that mentioned [\#$TheAssertionSentence](#.24TheAssertionSentence) itself. This is useful for making meta-assertions without having to copy the [\#$ist](#.24ist) assertions formula multiple times.

For example:

    ;; an example rule:

    In Mt: BaseKB.

    f: (implies

          (and

                 (termOfUnitÂ ?NART (?FUNC .Â ?ARGS))

                 (argIsaÂ ?FUNCÂ ?NÂ ?COL)
          
                 (argNÂ ?ARGNÂ ?NÂ ?NART)

                 (isaÂ ?ARGN Collection))

      (isaÂ ?ARGNÂ ?COL)).

     Â ;; assert a ruleTrivialForJustificationParaphrase GAF about the rule

     In Mt: BaseKB.

     f: (ruleTrivialForJustificationParaphrase TheAssertionSentence).

    Â ;; assert a salientAssertions GAF about the rule (not about the

    Â ;;  ruleTrivialForJustificationParaphrase GAF

     In Mt: BaseKB.

     f: (salientAssertions argIsa TheAssertionSentence).

Note that this support breaks the ability to make assertions about <a href="/wiki/index.php?title=TheAssertionSentence&amp;action=edit&amp;redlink=1" class="new" title="TheAssertionSentence (page does not exist)">TheAssertionSentence</a> itself using <a href="/wiki/index.php?title=KE_Text&amp;action=edit&amp;redlink=1" class="new" title="KE Text (page does not exist)">KE Text</a>.

### <span id="Comments_in_KE_Text" class="mw-headline">Comments in KE Text</span>

Comments (text to be read by a human, but not interpreted or entered by a program) are allowed in KE text. The comment indicator is the semi-colon (;), as in Common Lisp. Lines beginning with a semi-colon will be ignored. More precisely, any sequence of characters following a semi-colon (and including the semi-colon) up until the next occurrence of a return (line-break, line-feed) character will be ignored, except when the semi-colon occurs inside a string (a character sequence delimited by double quotes) which is not itself inside a comment.

### <span id="Order_of_Expressions" class="mw-headline">Order of Expressions</span>

Expressions in KE text are evaluated and processed in the order of their occurrence in the text. In general, itâ€™s a good idea to write KE text expressions about a constant only after the point where a Constant directive for that constant occurs (unless, of course, the constant is already known to Cyc).

### <span id="Another_Example" class="mw-headline">Another Example</span>

    Constant: Siegel.

    isa: HumanCyclist CulturalAnthropologist.

    In Mt: NaiveBiologicalDescentMt.

    children: SimoneSiegel.

    In Mt: LanguageAndWritingSystemMt.

    Direction:Â :forward.

    F: (implies

           (languageSpokenÂ ?person EasternPahariLanguage)

           (likesAsFriend SiegelÂ ?person)).

Evaluation of the expressions above would result in the four assertions being added to the Cyc KB.

In the BaseKB, we would have:

    (#$isa #$Siegel #$HumanCyclist)

    (#$isa #$Siegel #$CulturalAnthropologist)

In the NaiveBiologicalDescentMt, we would have:

    (#$children #$Siegel #$SimoneSiegel)

And in the LanguageAndWritingSystemMt, with directionÂ :forward (forward propagation), we would have:

    (#$implies

           (#$languageSpokenÂ ?person #$EasternPahariLanguage)

           (#$likesAsFriend #$SiegelÂ ?person))

### <span id="Processing_KE_Text_in_the_Cyc_Web_Interface" class="mw-headline">Processing KE Text in the Cyc Web Interface</span>

The Cyc Web Interface provides two facilities for processing KE Text: the Compose page and KE-File. Both may be accessed from the â€œKE-Fileâ€ section of the Cyc Navigator page. The Compose page can also be accessed from the Tools menu.

On the Compose page, you compose KE Text expressions in a large input pane. Clicking the â€œEvalâ€ button submits the completed expressions for processing.

On the KE-File page, you enter the pathname of the KE Text format file that you want to process. Clicking the â€œLoadâ€ button loads the file. The file must be in the filesystem of the Cyc Server Machine.

Either way, the KE Text Parser will process your KE Text and present its results to you before making any changes to the KB.

If it finds syntactic errors in your expressions, it will ask you to correct them before proceeding.

If it finds that you have referred to constants which do not yet exist, it will ask you whether you want to create them.

When the KE Text expressions parse without error or question, it will display the proposed changes as FI operations and ask for confirmation.

If the changes are confirmed, the operations are queued for processing on the Cyc Server Machine.

### <span id="Loading_a_KE_Test_File_from_a_Console_Interactor" class="mw-headline">Loading a KE Test File from a Console Interactor</span>

To process a KE Text file from a console interactor pane, call the function LOAD-KE-TEXT-FILE.

(In a Lisp implementation of Cyc, make sure you have set the package to â€œcycâ€ by evaluating (in-package â€œcycâ€) before calling LOAD-KE-TEXT-FILE).

    LOAD-KE-TEXT-FILE takes four arguments:

-   The first is the userâ€™s Cyc constant name (a character string delimited by double quotes).
-   The second is the pathname of the file to be entered (another character string).
-   The third indicates where the request should be queued, eitherÂ :agenda,Â :aux, or NIL.
-   The fourth specifies whether there will be â€œno user interactionâ€. T means no user interaction; NIL indicates there will be user interaction. This argument defaults to NIL.

Unless the fourth argument is T, the user will be asked questions as the file is processed. The user is given the opportunity to preview and confirm the batch of assertions before they are queued for processing.

<span id="Appendix_A:_KE_Text_History" class="mw-headline">Appendix A: KE Text History</span>
=============================================================================================

KE Text syntax is just a syntactic/notational variation of <a href="/wiki/index.php?title=CycL&amp;action=edit&amp;redlink=1" class="new" title="CycL (page does not exist)">CycL</a>. To some extent, it is a holdover from when Cyc was a frame-based system and CycL was a <a href="/wiki/index.php?title=Frame-based&amp;action=edit&amp;redlink=1" class="new" title="Frame-based (page does not exist)">frame-based</a> language. Many people find it easier and faster to write some types of CycL expressions using KE Text syntax than using â€œcanonicalâ€ <a href="/wiki/index.php?title=FOPC&amp;action=edit&amp;redlink=1" class="new" title="FOPC (page does not exist)">FOPC</a>-in-Lisp CycL syntax, which partly explains why KE Text syntax continues to be supported by knowledge entry tools.

    There are some aliases for directives which are retained for backward compatibility with files written for older versions of KE Text:

    Unit = Constant
     Access Level or AL = Direction
     0 =Â :forward
     4 =Â :backward
     EL = Formula

In the past you could not include references to a constant in a file/text if the constant appears in a kill expression earlier in the file/text. This would cause problems when the resulting kill expressions are processed by the Cyc Agenda, since the following expressions would reference what is now a dead (non-existent) constant.

<span id="Appendix_B:_KE_Text_Syntax" class="mw-headline">Appendix B: KE Text Syntax</span>
-------------------------------------------------------------------------------------------

    {ke-file}        Â ::= â€œâ€

    | {full-expression}

     | {full-expression} {ke-file}

     {full-expression}Â ::= {expression} â€œ.â€

    {expression}     Â ::= {directive}

     | {predicate-assertion}


     {directive}Â ::= {assert-mode-directive}

     | {constant-directive}

     | {in-mt-directive}

     | {default-mt-directive}

     | {assertion-direction-directive}

     | {assertion-truth-directive}

     | {include-ke-file-directive}

     | {kill-directive}

     | {rename-directive}

     | {formula-directive}



     {assert-mode-directive}Â ::= {enter-directive} | {delete-directive}

     {enter-directive}Â ::= {enter-keyword}

     {enter-keyword}Â ::= â€œenterâ€



    {delete-directive}Â ::= {delete-keyword}

     {delete-keyword}Â ::= â€œdeleteâ€



    {in-mt-directive}Â ::= {in-mt-keyword} â€œ:â€ {microtheory}

     {in-mt-keyword}Â ::= â€œin mtâ€

    {microtheory}Â ::= a string representing an instance of #$Microtheory



     {default-mt-directive}Â ::= {default-mt-keyword} â€œ:â€ {microtheory}

     {default-mt-keyword}Â ::= â€œdefault mtâ€

    {microtheory}Â ::= a string representing an instance of #$Microtheory



     {assertion-direction-directive}Â ::= {assertion-direction-keyword} â€œ:â€

    {assertion-direction-indicator}

     {assertion-direction-keyword}Â ::= â€œdâ€ | â€œalâ€ | â€œaccess levelâ€ | â€œdirectionâ€

    {assertion-direction-indicator}Â ::= â€œ:â€ {assertion-direction-indicator-keyword}

     | {assertion-direction-indicator-keyword}

     {assertion-direction-indicator-keyword}Â ::= â€œ0â€ | â€œforwardâ€

    | â€œ4â€ | â€œbackwardâ€

    | â€œcodeâ€ | â€œunknownâ€



    {assertion-truth-directive}Â ::= {assertion-truth-keyword} â€œ:â€

    {assertion-truth-indicator}

     {assertion-truth-keyword}Â ::= â€œtruth valueâ€ | â€œtvâ€

    {assertion-truth-indicator}Â ::= â€œ:â€ {assertion-truth-indicator-keyword}

     | {assertion-truth-indicator-keyword}

     {assertion-truth-indicator-keyword}Â ::= â€œ:defaultâ€ | â€œ:monotonicâ€ | â€œ:unknownâ€



    {constant-directive}Â ::= {constant-keyword} â€œ:â€ {constant-name}

     {constant-keyword}Â ::= â€œconstantâ€ | â€œunitâ€

    {constant-name}Â ::= a string representing the name of the constant



     {kill-directive}Â ::= {kill-keyword} â€œ:â€ {existing-constant}

     {kill-keyword}Â ::= â€œkillâ€

    {existing-constant}Â ::= a string representing an existing constant



     {rename-directive}Â ::= {rename-keyword} â€œ:â€ {existing-constant}

     {new-constant-name-string}

     {rename-keyword}Â ::= â€œrenameâ€

    {existing-constant}Â ::= a string representing an existing constant

     {new-constant-name-string}Â ::= a quoted string of the new constant name



     {include-ke-file-directive}Â ::= {include-ke-file-keyword} â€œ:â€ {filepath}

     {include-ke-file-keyword}Â ::= â€œincludeâ€

    {filepath}Â ::= path to the file



     {formula-directive}Â ::= {formula-keyword} â€œ:â€ {formula}

     {formula-keyword}Â ::= â€œfâ€ | â€œelâ€ | â€œformulaâ€

    {formula}Â ::= a CycL formula



     {predicate-assertion}Â ::= {pred-assertion-arity-2}

     | {pred-assertion-arity-3}

     | {pred-assertion-arity-4}

     | {pred-assertion-arity-5}

     {pred-assertion-arity-2}Â ::= {binary-pred} â€œ:â€ {atoms}

     {atoms}Â ::= {atom} | {atom} {atoms}

     {atom}Â ::= fort | string | number |Â ?TODO?

     {pred-assertion-arity-3}Â ::= {ternary-pred} â€œ:â€ {atom-pairs}

     {atom-pairs}Â ::= {atom-pair} | {atom-pair} {atom-pairs}

     {atom-pair}Â ::= â€œ(â€ {atom} {atom} â€œ)â€

    {pred-assertion-arity-4}Â ::= {quaternary-pred} â€œ:â€ {atom-triplets}

     {atom-triplets}Â ::= {atom-triplet} | {atom-triplet} {atom-triplets}

     {atom-triplet}Â ::= â€œ(â€ {atom} {atom} {atom} â€œ)â€

    {pred-assertion-arity-5}Â ::= {quintary-pred} â€œ:â€ {atom-quartets}

     {atom-quartets}Â ::= {atom-quartet} | {atom-quartet} {atom-quartets}

     {atom-quartet}Â ::= â€œ(â€ {atom} {atom} {atom} {atom} â€œ)â€

Appendix G ++++++++++

Common Cyc Abbreviations

Abbrev.

Definition

AIS
AbstractInformationStructure
API Application Programmersâ€™ Interface Arg Logical â€˜argumentâ€™ position of some relation in the CYC KB. BLO \#$BiologicalLivingObject CNF Conjunctive Normal Form CW Conceptual Work CycL The Cyc description language; extended predicate logic; formerly called CL (Constraint Language), then EL (Epistemological Language); see \#$CycL (includes HL) DARPA Defense Advanced Research Projects Agency Defn Definition DNF Disjunctive Normal Form EL Epistemological Level (user-level representations) FI Functional Interface Fn Function FOPC First-Order Predicate Calculus fp Forward Propagate GAF Ground Atomic Formula genl Generalization (i.e. a more general term); \#$genls is star-closure GT General Transitivity HL Heuristic Level (an internal machine represention) HPKB The DARPA High-Performance Knowledge Bases project IBO \#$InformationBearingObject IBQS \#$IntervalBasedQuantitySlot IBT \#$InformationBearingThing IDE Integrated Development Environment IKB Integrated Knowledge Base â€” the version of the Cyc Knowledge Base released for the High-Performance Knowledge Base project KB \#$KnowledgeBase, specifically, the Cyc Knowledge Base KE Knowledge Entry (into the Cyc KB) KFD Knowledge Formation through Dialog LHS Left-Hand Side (of a rule); the antecedent MELD Moderately Expressive Logical Description language; a superset of CycL Mt \#$Microtheory MWW Multi-Word-Word NART Non-Atomic Reified Term NAT Non-Atomic Term; a function together with its argument(s) NAUT Non-Atomic Un-reified Term; a functional term that is semantically valid but Cyc has currently not thought about (like John Smithâ€™s great-great grandmother) NL Natural Language NLP Natural Language Processing; also Natural Language Parsing NLU Natural Language Understanding OE Ontological Engineer or Engineering (no longer used) PIT \#$PropositionalInformationThing pred \#$Predicate PSC \#$ProblemSolvingCntxt Reln Relation RHS Right-Hand Side (of a rule); the consequent RKF Rapid Knowledge Formation â€” the DARPA follow-on research project to HPKB SDBI Semantic Database Integration SKSI Semantic Knowledge Source Integration SME Subject Matter Expert spec Specialization (i.e. a more specialized term); (inverse of \#$genls); specs is star-closure STIB Short Time Interval Before \[see \#$STIB\] STIF Short Time Interval Following \[see \#$STIF\] SubL A dialect of Lisp designed to implement the Cyc application TMS Truth-Maintenance System WALES Web-Assisted Lexical Entry System WFF Well-formed formula; pronounced â€˜woof.â€™ Used as an adjective to denote correctness (i.e. â€˜This CycL sentence is not wff.â€™)

Appendix L ++++++++++

OpenCyc License

The OpenCyc Knowledge Base

The OpenCyc Knowledge Base consists of code, written in the declarative language CycL, that represents or supports the representation of facts and rules pertaining to consensus reality. OpenCyc is licensed using the Apache License, Version 2, whose text can be found here. The OpenCyc CycL code base is the â€œWorkâ€ referred to in the Apache license. The terms of this license equally apply to renamings and other logically equivalent reformulations of the Knowledge Base (or portions thereof) in any natural or formal language.

The OpenCyc Java API and Other Non-CycL Open Source Code

Some other programs from those listed above are provided as open source. All of the programs of this type that currently ship with OpenCyc use the Apache License, but it is possible that future released programs may use a different license. All license and copyright information for these programs are included in their distributions.

The OpenCyc Knowledge Server

The OpenCyc Knowledge Server consists of a binary derivative version of the CycÂ® Inference Engine and Knowledge Base Index, a binary derivative version of the CycÂ® Knowledge Base Browser, and a suite of tools for rapidly extracting knowledge from a domain expert.

Cycorp is providing a free-of-charge license to the OpenCyc Knowledge Server that grants the Licensee the irrevocable right to download, to use, and to distribute the OpenCyc Knowledge Server in binary form for commercial or non-commercial use. By obtaining information, software, and/or documentation of the OpenCyc Knowledge Server in whole or in part (collectively, â€œMaterialâ€), you agree to be legally bound by the following terms.

The OpenCyc OWL Ontologies

These files contain an OWL representation of information contained in the OpenCyc Knowledge Base. The content of these OWL files are licensed under the Creative Commons Attribution 3.0 license whose text can be found at <a href="http://creativecommons.org/licenses/by/3.0/legalcode" class="uri" class="external free">http://creativecommons.org/licenses/by/3.0/legalcode</a>. The content of these OWL files, including the OpenCyc content they represent, constitutes the â€œWorkâ€ referred to in the Creative Commons license. The terms of this license equally apply to, without limitation, renamings and other logically equivalent reformulations of the content of these OWL files (or portions thereof) in any natural or formal language, as well as to derivations of this content or inclusion of it in other ontologies.

Appendix SubL +++++++++++++

<a href="http://www.cyc.com/documentation/subl-reference/" class="uri" class="external free">http://www.cyc.com/documentation/subl-reference/</a>

I have been studying OpenCyc and I was disappointed when the SubL guide I found didn't have any mention of the KB creation and access commands.

I have also been using AIMLpad which is a Alice.org chatbot program. It had a Cyc interface. By inspecting the documentation that came with AMLpad.

{NOTE: I have been informed that the FI-\* commands are being depreciated, that CYC-\* commands should be used instead. I have not tested this and will update the page once I can provide concrete feedback)

SubL commands: (FI-CREATE '"Cone4" )

Creates a new constant in Cyc. (FI-ASSERT '(\#$isa \#$Cone4 \#$Cone) '\#$BaseKB ':Default 'nil)

Makes assertion into Cyc knowledgebase. The second argument is the name of the microtheory to assert the fact. The third argument is the chaining direction. (FI-UNASSERT '(\#$isa \#$TomBelpasso \#$Dog) '\#$BaseKB )

Retracts previously asserted facts. (FI-RENAME '\#$Cone4 "Cone6")

Changes the name of the constant to a new sting. (fi-ask '(\#$isa '?ISIT \#$Cone) \#$EverythingPSC)

Does a KB query.

These commands are queued, I think there are are versions of the commands that are immediately applied.

â€¢Introduction â€¢KE Text Syntax â€¢Appendix A: KE Text History â€¢Appendix B: KE Text Syntax

Appendix T ++++++++++ [KE-Text/Terms](/wiki/index.php/KE-Text/Terms "KE-Text/Terms")

Retrieved from "<http://www.pdkb.org/wiki/index.php?title=KE-Text&oldid=39>"

Navigation menu
---------------

### Views

-   

    [Page](/wiki/index.php/KE-Text "View the content page [c]")
-   

    [Discussion](/wiki/index.php?title=Talk:KE-Text&action=edit&redlink=1 "Discussion about the content page [t]")
-   

    [View source](/wiki/index.php?title=KE-Text&action=edit "This page is protected.
    You can view its source [e]")
-   

    [History](/wiki/index.php?title=KE-Text&action=history "Past revisions of this page [h]")

### Personal tools

-   

    [Log in](/wiki/index.php?title=Special:UserLogin&returnto=KE-Text "You are encouraged to log in; however, it is not mandatory [o]")

[](/wiki/index.php/Main_Page "Visit the main page")

### Navigation

-   

    [Main page](/wiki/index.php/Main_Page "Visit the main page [z]")
-   

    [Recent changes](/wiki/index.php/Special:RecentChanges "A list of recent changes in the wiki [r]")
-   

    [Random page](/wiki/index.php/Special:Random "Load a random page [x]")
-   

    [Help](https://www.mediawiki.org/wiki/Special:MyLanguage/Help:Contents "The place to find out")

### Search

Â 

### Tools

-   

    [What links here](/wiki/index.php/Special:WhatLinksHere/KE-Text "A list of all wiki pages that link here [j]")
-   

    [Related changes](/wiki/index.php/Special:RecentChangesLinked/KE-Text "Recent changes in pages linked from this page [k]")
-   

    [Special pages](/wiki/index.php/Special:SpecialPages "A list of all special pages [q]")
-   

    [Printable version](/wiki/index.php?title=KE-Text&printable=yes "Printable version of this page [p]")
-   

    [Permanent link](/wiki/index.php?title=KE-Text&oldid=39 "Permanent link to this revision of the page")
-   

    [Page information](/wiki/index.php?title=KE-Text&action=info)

[<img src="/wiki/skins/common/images/cc-by-sa.png" alt="Creative Commons Attribution Share Alike" width="88" height="31" />](http://creativecommons.org/licenses/by-sa/3.0/)

[<img src="/wiki/skins/common/images/poweredby_mediawiki_88x31.png" alt="Powered by MediaWiki" width="88" height="31" />](//www.mediawiki.org/)

-   

    This page was last modified on 24 September 2016, at 10:46.
-   

    This page has been accessed 41 times.
-   

    Content is available under <a href="http://creativecommons.org/licenses/by-sa/3.0/" class="external">Creative Commons Attribution Share Alike</a> unless otherwise noted.
-   

    [Privacy policy](/wiki/index.php/Public_Domain_Knowledge_Base:Privacy_policy "Public Domain Knowledge Base:Privacy policy")
-   

    [About Public Domain Knowledge Bank](/wiki/index.php/Public_Domain_Knowledge_Base:About "Public Domain Knowledge Base:About")
-   

    [Disclaimers](/wiki/index.php/Public_Domain_Knowledge_Base:General_disclaimer "Public Domain Knowledge Base:General disclaimer")


