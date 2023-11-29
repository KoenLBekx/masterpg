# masterpg


## Overview

The `masterpg` utility composes files - usually static HTML files - as an alternative to a missing
`<include />` tag in HTML.

This crate provides both
- a library function,
- and a command-line utility.

Both the library function and the command-line utility were designed to be used web-server-side or during the publishing phase of web pages, albeit they
can compose any kind of text files.


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
`masterpg` functions by processing seven kinds of tags in input files :

> `<+output file_path/>` :
>> indicates to which file the result of the composition operation has to be written.

> `<+master file_path/>` :
>> indicates that a file having the given path is to be used as master page. If the file path doesn't contain a full path, it is considered to be relative to  the current active directory.<br />
An input file can contain more than one `<+master ...>` tags.

> `<+placeholder tag_name/>` :
>> indicates that this placeholder has to be replaced by the content of an `<+actual>` tag having the same tag name.

> `<+actual tag_name>`...`</+actual>` :
>> the actual content to replace placeholders with having the same tag name.

> `<+comment>`...`</comment>` :
>> will be removed from the output.

> `<+timestamp/>` :
>> produces a timestamp in the form of `ts=16548548678647` in the output text. The digits are
the number of milliseconds elapsed since the start of the Unix epoch.

> `<+calc tag_name [operator] [operand1] [operand2] [operand3] ... />` :
>> performs a calculation : it applies the operator +, -, *, / or others on one or more operands.

Note that these tags are case-insensitive.

## Calculations

The operands of a `<+calc  ... />` tag can be either
- literal numerical values, or else
- tag names that refer to other `<+calc tag_name .../>` tags or `<+actual tag_name>` tags
having that tag name.

This means that `<+actual>` tags and `<+calc>` tags can't have the same tag name.

No units may be included in the operands : 10 is allowed, but not 10px.

Units, however, may follow immediately after `<+calc - .../>` or `<+placeholder .../>` tags. E.g.:

>`<+actual divHeight>30</+actual>`<br/>
`<+actual goldenCut>1.618034</+actual>`<br/>
`<+calc largeDivHeight * divHeight 2/>`<br/>
`<+calc largeDivWidth / largeDivHeight goldenCut />`<br/>
`...`<br/>
`<+placeholder largeDivWidth/>px`<br/>
`...`<br/>
`<+placeholder largeDivWidth/>px`<br/>

A `<+calc tag_name .../>` tag, when evaluated, will be replaced by an `<+/actual tag_name>result</+actual>` tag, except when the tag name (the first word after `<+calc`) is `-`, in which case the result will be directly included in the output file :

>`<+actual price>200</+actual>`<br />
`<+actual commission>20</+actual>`<br />
`<+actual taxRate>0.20</+actual>`<br />
`<+calc amount + price commission/> ---> <+actual amount>220</+actual>`<br />
`<+calc tax * amount taxRate/> ---> <+actual>44</+actual>`<br />
`<+calc - + amount tax/> $ ---> 264 $`

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

> `<+calc>`, `<+actual>` and `<+placeholder>` tags may reside in the same file, in a master
file or in a client file referring to a master file. As `masterpg` operates by joining all
these files' contents to one text before resolving the said tags, you are free to put these
tags wherever they serve your purpose.

## How input files are processed

masterpg works by :

- handling all the input files given on its command line;

- executing the below steps for every input file given on the the command line :

    - finding the first `<+output>` tag and storing the output file path in a variable;

    - then reading an input file in a string and replacing all `<+master />`-tags in this string with the content of the entire file indicated, and even doing this recursively, so even master page files can contain their own `<+master />`-tags;

    - then, in the resulting string, replacing
        - all `<+calc>` tags with `<+actual>` tags holding the calculated value;
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

## Example

### Given consts.mpm :

`<+actual siteName>Test Pages</+actual>`<br />
`<+actual author>Lenny Baxter</+actual>`

### Given general.mpx :

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

### Given testpg.mpc :

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

### Then the command

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
