# masterpg

## Overview

The `masterpg` utility composes files by replacing placeholder tags with actual content defined elsewhere, possibly in other files. Initially it was meant for static HTML files, as an alternative to a missing
`<include />` tag in HTML.

This crate provides both
- a library function,
- and a command-line utility.

Both the library function and the command-line utility were originally designed to be used web-server-side or during the publishing phase of web pages, albeit they
can compose any kind of UTF-8 text files.


## Library function

`pub fn compose` in `src/lib.rs` provides this utility's composition functionality as a public function you can call from within your own Rust applications.

It has at least two modes of input and output, as it takes an instance of an implementation of the [`string_io_and_mock::TextIOHandler`](https://crates.io/crates/string_io_and_mock) trait for its `io_handler` parameter  :
- files
- strings stored in memory.

In order to have `masterpg` work with files, you can pass an instance of `string_io_and_mock::FileTextHandler`.

In order to have it work with strings, you can pass an instance of `string_io_and_mock::MockTextHandler`.

*(`MockTextHandler` is called that way because it can be used to mock the behaviour of `FileTextHandler` in unit tests, even if it is usable and tested as a component in its own right for storage and retrieval of strings in memory.)*

For other needs, you can always develop your own implementation of `string_io_and_mock::TextIOHandler`.

In order to use the library function, execute the below command in your project's main directory:

> `cargo add masterpg`


## Command-line utility

The command-line utility is defined by `fn main` in src/main.rs and calls the library function.

In order to use it, you'll have to [install Rust](https://www.rust-lang.org/tools/install), open a terminal window, and execute

> `cargo install masterpg`

which will both download and compile `masterpg` for your architecture.

For more details about the file system path where the resulting executable can be found, see [Cargo's reference](https://doc.rust-lang.org/cargo/commands/cargo-install.html#description).


## Input file tags
`masterpg` functions by processing eight kinds of tags in input files :

> `<+output file_path/>` :
>> indicates to which file the result of the composition operation has to be written.

> `<+master file_path/>` :
>> indicates that a file having the given path is to be used as master page. If the file path doesn't contain a full path, it is considered to be relative to  the current active directory.<br />
>> An input file can contain more than one `<+master ...>` tags.

> `<+placeholder tag_name/>` :
>> indicates that this placeholder has to be replaced by the content of an `<+actual>` tag having the same tag name.

> `<+actual tag_name>`...`</+actual>` :
>> the actual content to replace placeholders with having the same tag name.

> `<+comment>`...`</comment>` :
>> will be removed from the output.

> `<+timestamp/>` :
>> produces a timestamp in the form of `ts=16548548678647` in the output text. The digits are the number of milliseconds elapsed since the start of the Unix epoch.

> `<+calc tag_name [operator] [operand1] [operand2] [operand3] ... />` :
>> performs a calculation : it applies the operator +, -, *, / or others on one or more operands.

> `<+laconic tag_name {laconic expression or script} />` :
>> performs a calculation by interpreting a [Laconic](https://crates.io/crates/laconic) script.

Note that these tags are case-insensitive.

## Calculations in `<+calc ... />` tags

The operands of a `<+calc  ... />` tag can be either
- literal numerical values, or else
- tag names that refer to other `<+calc tag_name .../>` tags or `<+actual tag_name>` tags having that tag name.

This means that `<+actual>` tags and `<+calc>` tags can't have the same tag name.

No units may be included in the operands : 10 is allowed, but not 10px.

Units, however, may follow immediately after `<+calc - .../>` or `<+placeholder .../>` tags. E.g.:

>`<+actual divHeight>30</+actual>`<br/>
>`<+actual goldenCut>1.618034</+actual>`<br/>
>`<+calc largeDivHeight * divHeight 2/>`<br/>
>`<+calc largeDivWidth / largeDivHeight goldenCut />`<br/>
>`...`<br/>
>`<+placeholder largeDivWidth/>px`<br/>
>`...`<br/>
>`<+placeholder largeDivWidth/>px`<br/>

A `<+calc tag_name .../>` tag, when evaluated, will be replaced by an `<+/actual tag_name>result</+actual>` tag, except when the tag name (the first word after `<+calc`) is `-`, in which case the result will be directly included in the output file :

>`<+actual price>200</+actual>`<br />
>`<+actual commission>20</+actual>`<br />
>`<+actual taxRate>0.20</+actual>`<br />
>`<+calc amount + price commission/> ---> <+actual amount>220</+actual>`<br />
>`<+calc tax * amount taxRate/> ---> <+actual>44</+actual>`<br />
>`<+calc - + amount tax/> $ ---> 264 $`

`<+calc>` tags handle the below operators :

- `+` : addition
- `-` : subtraction
- `*` : multiplication
- `x` : multiplication also
- `/` : division
- `:` : division also
- `÷` : division also
- `%` : remainder
- `min` : get smallest of given operands
- `max` : get greatest of given operands
- `abs` : absolute value
- `^` : power
- `**` : power also
- `pow` : power also
- `exp` : power also
- `sign` : the sign of the first operand.
- `round` : rounding around 0
- `trunc` : removing fractal value, towards 0,
- `floor` : removing fractal value, towards -∞
- `ceiling` : removing fractal value, towards +∞

> These operators are case-insensitive.

> `<+calc>`, `<+actual>` and `<+placeholder>` tags may reside in the same file, in a master file or in a client file referring to a master file. As `masterpg` operates by joining all these files' contents to one text before resolving the said tags, you are free to put these tags wherever they serve your purpose.

## Calculations in `<+laconic ... />` tags

Version 2.0.0 of the `masterpg` crate saw the addition of `<+laconic ...>` tags.

These tags offer both way more functionality and way more flexibility in calculations than the `<+calc ...>` tags.

A `<+laconic tagname {script}/>` tag contains a tagname or a `-` marker, just like the `<+calc>` tag, but next it contains a Laconic expression or script enclosed in matching multiples of curly braces. E.g.:

>`<+laconic - {* 200 c#gold}/>`<br/>
> will put the product of 200 and the golden ratio constant in the composed file.<br/>

>`<+laconic - {{? v#isHtml [s <style>.small {font-size: .5em;}</style>] #}}/>`<br/>
> will insert an HTML style tag into the composed file if variable #isHtml has a truthy value, without any need to escape the single curly braces, as the entire Laconic expression is enclosed in double curly braces.

Laconic scripts are unaware about other tags in source files, but there's no need for that:

instead of using references to other tags by tag name, Laconic scripts can process variables assigned in other Laconic tags in the set of source files used to compose a result file.

In Laconic scripts, one way of assigning a variable is an expression like the below one:

> `$#varName value`

E.g.:

> `<+laconic dummyTagName {$#width 400}/>`<br/>
> assigns 400 to a variable having name 'width'.<br/>
> (Mind that this way, variable names can't have spaces.)

> A subsequent Laconic tag can reuse the value of that variable by referencing it using Laconic's `v` operator:<br/>
> `<+laconic - {/ v#width c#gold}/>`<br/>
> will put the width divided by the golden ratio constant in the output file of the composition.

*Note: one <+laconic .../> tag can contain multiple assignments, and even calculations, on a single line or even multiple ones, e.g.:*

>`<+laconic generalAssignments {`<br/>
>`    $#rows 10`<br/>
>`    $#columns 4`<br/>
>`    $#cells * v#rows v#columns`<br/>
>`}/>` 

Laconic offers much more than simple calculations on numeric variables! See [crates.io](https://crates.io/crates/laconic) and its documentation link for the full documentation of the Laconic language.

## How input files are processed

masterpg works by :

- handling all the input files given on its command line;

- executing the below steps for every input file given on the the command line :

    - finding the first `<+output>` tag and storing the output file path in a variable;

    - then reading an input file in a string and replacing all `<+master />`-tags in this string with the content of the entire file indicated, and even doing this recursively, so even master page files can contain their own `<+master />`-tags;

    - then, in the resulting string, replacing
        - all `<+calc>` tags with `<+actual>` tags holding the calculated value, or with the literal value;
        - all `<+laconic>` tags with `<+actual>` tags holding the calculated value, or with the literal value;
        - all `<+placeholder>` tags with the content of the corresponding `<+actual>` tags;
        - all `<timestamp/>` tags with a timestamp;

    - then, removing all the `<+...>` tags (should only be +comment, +output and +actual);

    - finally, writing the result to the file path found in the first `<+output>` tag, overwriting existing files.


As a convention, which is not enforced by `masterpg`, you could use the below file name extensions :

- *.mpm for master pages;
- *.mpc for master page consumers (or clients), having an `<+master .../>` tag;
- *.mpx for files that both serve as a master page and consume one themselves.

Note that only the name of the final consumer file has to be passed as the first argument to the command

> `masterpg [consumer file]`

or, if more than one output file has to be generated :

> `masterpg [consumer file] [consumer file] [consumer file] ...`

## Example: HTML

### Given consts.mpm :

`<+actual siteName>Test Pages</+actual>`<br />
`<+actual author>Lenny Baxter</+actual>`

### and given general.mpx :

`<+master consts.mpm/>`<br />
`<!doctype html>`<br />
`<html>`<br />
`   <head>`<br />
`       <title><+placeholder pageTitle/></title>`<br />
`   </head>`<br />
`   <body>`<br />
`       <h1><+placeholder pageTitle/></h1>`<br />
`       <i>by <+placeholder author/></i>`<br />
`       <div id="main"><+placeholder pageContent/></div>`<br />
`   </body>`<br />
`</html>`

### and given testpg.mpc :

`<+master general.mpx/>`<br />
`<+output index.htm/>`<br />
<br />
`<+actual pageContent>`<br />
`<p>Welcome to my <+placeholder siteName/> site !</p>`<br />
`<p>I'm <+placeholder author/> and I'm just testing the masterpg module.</p>`<br />
`<p>Bye for now !</p>`<br />
`</+actual>`<br />
<br />
`<+actual pageTitle>Welcome</+actual>`

### then the command

`masterpg testpg.mpc`

### creates the file index.html :

`<!doctype html>`<br />
`<html>`<br />
`   <head>`<br />
`       <title>Welcome</title>`<br />
`   </head>`<br />
`   <body>`<br />
`       <h1>Welcome</h1>`<br />
`       <i>by Lenny Baxter</i>`<br />
`       <div id="main">`<br />
`           <p>Welcome to my Test Pages site !</p>`<br />
`           <p>I'm Lenny Baxter and I'm just testing the masterpg module.</p>`<br />
`           <p>Bye for now !</p>`<br />
`       </div>`<br />
`   </body>`<br />
`</html>`

## Example: PostScript

### Given undecim.ps.mpm :

`<+output undecim.ps/>`<br/>

`%!PS`<br/>
`newpath`<br/>
` `<br/>
`<+laconic - {`<br/>
`	$#centerX 300`<br/>
`	$#centerY 400`<br/>
`	$#radius 150`<br/>
`	$#points 11`<br/>
`	$#skip i / v#points 2`<br/>
`	$#arch * v#skip / *2p v#points`<br/>
`	#`<br/>
`}/>`<br/>
` `<br/>
`<+laconic - {v#centerX}/> <+laconic - {+ v#centerY v#radius}/> moveto`<br/>
` `<br/>
`<+laconic - {`<br/>
`	o#fmt 3`<br/>
`	$#linesCode c#n`<br/>
` `<br/>
`	F`<br/>
`		1`<br/>
`		v#points`<br/>
`		1`<br/>
`		#i `<br/>
`		;(`<br/>
`			$#nextArch + % * v#arch v#i *2p /p2`<br/>
`			$#x + v#centerX * v#radius C v#nextArch`<br/>
`			$#y + v#centerY * v#radius S v#nextArch`<br/>
`			+(:#linesCode v#x [s ] v#y [s lineto] c#n)`<br/>
`		)`<br/>
` `<br/>
`	v#linesCode`<br/>
`}/>`<br/>
` `<br/>
`gsave`<br/>
`0 1 0.5 setrgbcolor`<br/>
`fill`<br/>
`grestore`<br/>
`0 0 1 setrgbcolor`<br/>
`4 setlinewidth`<br/>
`stroke`<br/>
`showpage`

> *Note: the # in the last line of the first <+laconic> tag causes an empty string to be output in the result file. Without this, the value of the last expression - the $#arch assignment - would be output in the result file.*

### then the command

`masterpg undecim.ps.mpm`

### creates the file undecim.ps:

`%!PS`<br/>
`newpath`<br/>
` `<br/>
` `<br/>
` `<br/>
`300.000000 550.000000 moveto`<br/>
` `<br/>
` `<br/>
`257.740 256.075 lineto`<br/>
`381.095 526.187 lineto`<br/>
`186.637 301.770 lineto`<br/>
`436.445 462.312 lineto`<br/>
`151.527 378.653 lineto`<br/>
`448.472 378.653 lineto`<br/>
`163.555 462.312 lineto`<br/>
`413.362 301.770 lineto`<br/>
`218.904 526.187 lineto`<br/>
`342.260 256.075 lineto`<br/>
`300.000 550.000 lineto`<br/>
` `<br/>
` `<br/>
`gsave`<br/>
`0 1 0.5 setrgbcolor`<br/>
`fill`<br/>
`grestore`<br/>
`0 0 1 setrgbcolor`<br/>
`4 setlinewidth`<br/>
`stroke`<br/>
`showpage`

