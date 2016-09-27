#lang scribble/manual

@(require (for-label konffaa))

@title[#:tag "konffaa"]{Konffaa}

@author["Tero Hasu"]

@deftech{Konffaa} is a lightweight configuration manager originally created for the @link["https://github.com/contextlogger/"]{ContextLogger2} project.
Konffaa aims to work with different build managers and scripts by generating build information in different languages.
It consists of a @tt{konffaa} command-line tool and a @racketmodname[konffaa] project configuration specification language.

@section[#:tag "konffaa-lang"]{Variant Specification Language}

@defmodulelang[konffaa]

The @racketmodname[konffaa] language exports all of @racketmodname[racket], as well as additional forms for defining variants and their attributes.

For defining a variant, you may use the @racket[define-variant], @racket[define-variant*], and @racket[define-variant**] forms. The one-star form is like the no-star form, except that it @racket[provide]s the defined variant. The two-star form both @racket[provide]s the variant under its original name, plus it also provides it as the ``main'' import of the module, so that the @tt{konffaa} command will find it.

@deftogether[
 (@defform[(define-variant id (base-expr ...) body ...+)]
  @defform[(define-variant* id (base-expr ...) body ...+)]
  @defform[(define-variant** id (base-expr ...) body ...+)])]{
Defines a product variant, binding it as @racket[id].
The variant inherits from other variants as specified by @racket[base-expr ...].
The @racket[body ...] of a definition should contain attributes, fields, and axioms specifying the variant and its constraints.
}

@deftogether[
 (@defform[(define-attribute id value-expr ...+)]
  @defform[(define-field id value-expr ...+)])]{
Defines a variant attribute or field, naming it as @racket[id].
Its value is given by the last of the @racket[value-expr]essions.
The difference between the two forms is that only the former defines fields whose values get output; consequently, those values should be serializable. Inheritance works for both kinds of fields.
}

@defform[(define-axiom id assert-expr ...+)]{
Defines an axiom, naming it as @racket[id].
The @racket[assert-expr]essions would typically contain one or more @racket[assert]ions.
Axioms can be inherited between variants in the same way as fields are.}

@defform[(assert test-expr maybe-msg)
  #:grammar ([maybe-msg (code:line) message])]{
An assertion to use within an axiom.
An optional @racket[message] may be provided, in addition to a @racket[test-expr]ession which should hold for the containing axiom to hold.}

See the source code of the modules @filepath{attribute.rkt}, @filepath{axiom.rkt}, and @filepath{surface.rkt} for more details about the @racketmodname[konffaa] DSL.

For examples of variant configuration files, see the @filepath{variants} directory of the source distribution.

@section[#:tag "konffaa-cli"]{Command-Line Interface}

The idea with configuring a project for a specific build variant with Konffaa is simple: you invoke @tt{konffaa} for a specific configuration @racketvarfont{variant} as its command-line argument:
@;
@commandline{konffaa @racketvarfont{variant}}
@;
The @racketvarfont{variant} may either name a variant specification file, or it may be a variant name, whose filename is then assumed to be @filepath{variants/@racketvarfont{variant}.var.rkt}.

When correctly invoked, the @tt{konffaa} program will then process the specified variant-describing input file by computing a full set of attributes for that variant, which---if the associated axioms hold---are then output essentially as sets of key-value pairs in different file formats. Those output files will then reflect the selected variant configuration until such time that @tt{konffaa} is invoked again to choose a different configuration.

The generated include files are only touched when they actually change as a result of the change in configuration, which allows Konffaa to work better with build managers (such as Make) whose dependency checking is based on timestamps.

The generated include files are intended to be used to configure other development tools (most notably build managers) in a variant-specific way.

Konffaa presently supports GNU Make and @tt{qmake} as foreign output languages, as well as C and Ruby, with the latter ones aimed at configuring programs and custom build scripts. Scripts can be particularly useful for driving template-based generation of input files for more obscure build tools that use non-standard file formats.

@section{Shell Completion}

Completion of variant names based on the @filepath{.var.rkt} suffixed files found in the @filepath{variants} directory is supported for the @tt{konffaa} command in the Bash shell. That support is provided by the @filepath{bash_completion} file in the source distribution.

@section[#:tag "konffaa-license"]{License}

Except where otherwise noted, the following license applies:

Copyright © 2009--2016 Helsinki Institute for Information
Technology HIIT, University of Bergen, and the authors.

Authors: Tero Hasu

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
