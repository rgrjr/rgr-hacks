;;; Autoloads for "published" files in this directory.  Updated automatically by
;;; the rgr-update-public-autoloads command.  -- rgr, 23-Mar-98.

;;;### (autoloads (bmerc-goto-cti bmerc-install-frame-properties) "bmerc-hacks" "bmerc-hacks.el" (14204 217))
;;; Generated autoloads from bmerc-hacks.el

(autoload (quote bmerc-install-frame-properties) "bmerc-hacks" nil nil nil)

(autoload (quote bmerc-goto-cti) "bmerc-hacks" "\
In a core file, goes to the specified core residue, given a CTI as
a prefix argument (prompts if no prefix specified)." t nil)

;;;***

;;;### (autoloads (rgr-html-delete-boilerplate rgr-html-batch-update-boilerplate rgr-html-update-boilerplate rgr-html-update-buffer-boilerplate) "rgr-html-boilerplate" "rgr-html-boilerplate.el" (14093 11073))
;;; Generated autoloads from rgr-html-boilerplate.el

(autoload (quote rgr-html-update-buffer-boilerplate) "rgr-html-boilerplate" "\
Redoes all the boilerplate in the buffer (or the currently restricted
portion of it)." t nil)

(autoload (quote rgr-html-update-boilerplate) "rgr-html-boilerplate" "\
Update this (or the next) <!--boilerplate--> construct." t nil)

(autoload (quote rgr-html-batch-update-boilerplate) "rgr-html-boilerplate" nil nil nil)

(autoload (quote rgr-html-delete-boilerplate) "rgr-html-boilerplate" nil t nil)

;;;***

;;; Generated autoloads from rgr-html-head.el

(autoload (quote rgr-html-increment-buffer-heading-nesting) "rgr-html-head" "\
Increment or decrement the nesting level of headings in the buffer,
e.g. changing <h1> to <h2>, <h2> to <h3>, etc.  By default, the depth is
increased by one, but this can be changed with a prefix argument, which
can be negative to decrease the nesting depth.  HTML tag nesting errors
are not detected." t nil)

(autoload (quote rgr-html-increment-region-heading-nesting) "rgr-html-head" "\
Increment or decrement the nesting level of headings in the buffer,
e.g. changing <h1> to <h2>, <h2> to <h3>, etc.  By default, the depth is
increased by one, but this can be changed with a prefix argument, which
can be negative to decrease the nesting depth.  HTML tag nesting errors
are not detected." t nil)

(autoload (quote rgr-html-number-headings) "rgr-html-head" "\
Scan the buffer for headings, putting section numbers on them." t nil)

(autoload (quote rgr-html-add-heading-anchors) "rgr-html-head" "\
Add anchors to all headings that do not already have them.
A numeric arg means only go down to that depth." t nil)

;;;***

;;; Generated autoloads from rgr-html-hacks.el

(autoload (quote rgr-html-forward-paragraph) "rgr-html-hacks" "\
Move forward to the end of the HTML paragraph, where `paragraph' is
interpreted in terms of the HTML source.  With arg N, do it N times; a
negative arg -N means move backward N paragraphs.

   A paragraph is delimited by either one or more blank lines (which
separate them), or an HTML tag at the start of the line (possibly with
whitespace in front of it) that may cause the browser to start a new
paragraph.  This is defined broadly as any tag that appears on the
`rgr-html-non-paragraph-tags' list, which is initially just those tags
that do font changes (e.g. <b>, <em>, etc.).  Anchors are handled
specially; <a name=\"...\"> starts a paragraph; <a href=\"...\"> and
</a> do not.

   Note that tags that start paragraphs, including <p> itself, are not
considered to actually do so unless they are at the beginning of an HTML
source line.

   The implementation is conceptually compatible with the default
forward-paragraph (\\[forward-paragraph]) command.  It uses the
`paragraph-separate' regexp to identify lines (usually blank) that
separate paragraphs, but replaces `paragraph-start' with code that looks
for either a separator or an HTML tag as defined above.  As for
\\[forward-paragraph], a paragraph end is the beginning of a line which
is not part of the paragraph to which the end of the previous line
belongs, or the end of the buffer." t nil)

(autoload (quote rgr-html-backward-paragraph) "rgr-html-hacks" "\
Move backward to the start of the HTML paragraph, where `paragraph'
is interpreted in terms of the HTML source.  With arg N, do it N times;
a negative arg -N means move forward N paragraphs." t nil)

(autoload (quote rgr-html-fill-paragraph) "rgr-html-hacks" "\
Fill a paragraph as defined by the rgr-html-forward-paragraph command
\(presently bound to \\[rgr-html-forward-paragraph].  See the
rgr-html-forward-paragraph documentation for details." t nil)

(autoload (quote rgr-html-make-toc) "rgr-html-hacks" "\
Insert a table of contents generated from <h#></h#> lines.  If this
is the first invocation in this buffer, this command inserts at point,
and you will be asked to confirm this.  Otherwise, the previous TOC is
replaced with a new one.

   If given a numeric argument, the items are generated within nested <ul>
forms; otherwise, they are within <ol> forms.

   In order to be seen, the markup for section headers must appear at
the start of the line, and the entire header must appear on one line.
\(You should doublecheck these constraints if the code seems to be
mysteriously ignoring sections.)  If an <a name=\"tag\"> appears on the
previous line, the TOC entry for that section will be wrapped in a
hypertext reference." t nil)

(autoload (quote rgr-html-define-commands) "rgr-html-hacks" "\
Bind the rgr-html-* commands to the usual keys.  Use this as a hook
function if you want it:

	(add-hook 'html-helper-load-hook 'rgr-html-define-commands)
" nil nil)

(autoload (quote rgr-html-helper-mode-hook) "rgr-html-hacks" "\
This does html-helper-mode to my taste.  You probably want to use
rgr-html-define-commands as a load hook instead.  -- rgr, 14-Oct-97." nil nil)

;;;***

;;; Generated autoloads from rgr-html-boilerplate.el

(autoload (quote rgr-html-update-buffer-boilerplate) "rgr-html-boilerplate" "\
Redoes all the boilerplate in the buffer (or the currently restricted
portion of it)." t nil)

(autoload (quote rgr-html-update-boilerplate) "rgr-html-boilerplate" "\
Update this (or the next) <!boilerplate> construct." t nil)

(autoload (quote rgr-html-batch-update-boilerplate) "rgr-html-boilerplate" nil nil nil)

(autoload (quote rgr-html-delete-boilerplate) "rgr-html-boilerplate" nil t nil)

;;;***

;;; Generated autoloads from rgr-html-head.el

(autoload (quote rgr-html-increment-buffer-heading-nesting) "rgr-html-head" "\
Increment or decrement the nesting level of headings in the buffer,
e.g. changing <h1> to <h2>, <h2> to <h3>, etc.  By default, the depth is
increased by one, but this can be changed with a prefix argument, which
can be negative to decrease the nesting depth.  HTML tag nesting errors
are not detected." t nil)

(autoload (quote rgr-html-increment-region-heading-nesting) "rgr-html-head" "\
Increment or decrement the nesting level of headings in the buffer,
e.g. changing <h1> to <h2>, <h2> to <h3>, etc.  By default, the depth is
increased by one, but this can be changed with a prefix argument, which
can be negative to decrease the nesting depth.  HTML tag nesting errors
are not detected." t nil)

(autoload (quote rgr-html-number-headings) "rgr-html-head" "\
Scan the buffer for headings, putting section numbers on them." t nil)

(autoload (quote rgr-html-add-heading-anchors) "rgr-html-head" "\
Add anchors to all headings that do not already have them.
A numeric arg means only go down to that depth." t nil)

;;;***

;;; Generated autoloads from rgr-html-hacks.el

(autoload (quote rgr-html-forward-paragraph) "rgr-html-hacks" "\
Move forward to the end of the HTML paragraph, where `paragraph' is
interpreted in terms of the HTML source.  With arg N, do it N times; a
negative arg -N means move backward N paragraphs.

   A paragraph is delimited by either one or more blank lines (which
separate them), or an HTML tag at the start of the line (possibly with
whitespace in front of it) that may cause the browser to start a new
paragraph.  This is defined broadly as any tag that appears on the
`rgr-html-non-paragraph-tags' list, which is initially just those tags
that do font changes (e.g. <b>, <em>, etc.).  Anchors are handled
specially; <a name=\"...\"> starts a paragraph; <a href=\"...\"> and
</a> do not.

   Note that tags that start paragraphs, including <p> itself, are not
considered to actually do so unless they are at the beginning of an HTML
source line.

   The implementation is conceptually compatible with the default
forward-paragraph (\\[forward-paragraph]) command.  It uses the
`paragraph-separate' regexp to identify lines (usually blank) that
separate paragraphs, but replaces `paragraph-start' with code that looks
for either a separator or an HTML tag as defined above.  As for
\\[forward-paragraph], a paragraph end is the beginning of a line which
is not part of the paragraph to which the end of the previous line
belongs, or the end of the buffer." t nil)

(autoload (quote rgr-html-backward-paragraph) "rgr-html-hacks" "\
Move backward to the start of the HTML paragraph, where `paragraph'
is interpreted in terms of the HTML source.  With arg N, do it N times;
a negative arg -N means move forward N paragraphs." t nil)

(autoload (quote rgr-html-fill-paragraph) "rgr-html-hacks" "\
Fill a paragraph as defined by the rgr-html-forward-paragraph command
\(presently bound to \\[rgr-html-forward-paragraph].  See the
rgr-html-forward-paragraph documentation for details." t nil)

(autoload (quote rgr-html-make-toc) "rgr-html-hacks" "\
Insert a table of contents generated from <h#></h#> lines.  If this
is the first invocation in this buffer, this command inserts at point,
and you will be asked to confirm this.  Otherwise, the previous TOC is
replaced with a new one.

   If given a numeric argument, the items are generated within nested <ul>
forms; otherwise, they are within <ol> forms.

   In order to be seen, the markup for section headers must appear at
the start of the line, and the entire header must appear on one line.
\(You should doublecheck these constraints if the code seems to be
mysteriously ignoring sections.)  If an <a name=\"tag\"> appears on the
previous line, the TOC entry for that section will be wrapped in a
hypertext reference." t nil)

(autoload (quote rgr-html-define-commands) "rgr-html-hacks" "\
Bind the rgr-html-* commands to the usual keys.  Use this as a hook
function if you want it:

	(add-hook 'html-helper-load-hook 'rgr-html-define-commands)
" nil nil)

(autoload (quote rgr-html-helper-mode-hook) "rgr-html-hacks" "\
This does html-helper-mode to my taste.  You probably want to use
rgr-html-define-commands as a load hook instead.  -- rgr, 14-Oct-97." nil nil)

;;;***

;;; Generated autoloads from rgr-html-head.el

(autoload (quote rgr-html-increment-buffer-heading-nesting) "rgr-html-head" "\
Increment or decrement the nesting level of headings in the buffer,
e.g. changing <h1> to <h2>, <h2> to <h3>, etc.  By default, the depth is
increased by one, but this can be changed with a prefix argument, which
can be negative to decrease the nesting depth.  HTML tag nesting errors
are not detected." t nil)

(autoload (quote rgr-html-increment-region-heading-nesting) "rgr-html-head" "\
Increment or decrement the nesting level of headings in the buffer,
e.g. changing <h1> to <h2>, <h2> to <h3>, etc.  By default, the depth is
increased by one, but this can be changed with a prefix argument, which
can be negative to decrease the nesting depth.  HTML tag nesting errors
are not detected." t nil)

(autoload (quote rgr-html-number-headings) "rgr-html-head" "\
Scan the buffer for headings, putting section numbers on them." t nil)

(autoload (quote rgr-html-add-heading-anchors) "rgr-html-head" "\
Add anchors to all headings that do not already have them.
A numeric arg means only go down to that depth." t nil)

;;;***

;;; Generated autoloads from rgr-html-hacks.el

(autoload (quote rgr-html-forward-paragraph) "rgr-html-hacks" "\
Move forward to the end of the HTML paragraph, where `paragraph' is
interpreted in terms of the HTML source.  With arg N, do it N times; a
negative arg -N means move backward N paragraphs.

   A paragraph is delimited by either one or more blank lines (which
separate them), or an HTML tag at the start of the line (possibly with
whitespace in front of it) that may cause the browser to start a new
paragraph.  This is defined broadly as any tag that appears on the
`rgr-html-non-paragraph-tags' list, which is initially just those tags
that do font changes (e.g. <b>, <em>, etc.).  Anchors are handled
specially; <a name=\"...\"> starts a paragraph; <a href=\"...\"> and
</a> do not.

   Note that tags that start paragraphs, including <p> itself, are not
considered to actually do so unless they are at the beginning of an HTML
source line.

   The implementation is conceptually compatible with the default
forward-paragraph (\\[forward-paragraph]) command.  It uses the
`paragraph-separate' regexp to identify lines (usually blank) that
separate paragraphs, but replaces `paragraph-start' with code that looks
for either a separator or an HTML tag as defined above.  As for
\\[forward-paragraph], a paragraph end is the beginning of a line which
is not part of the paragraph to which the end of the previous line
belongs, or the end of the buffer." t nil)

(autoload (quote rgr-html-backward-paragraph) "rgr-html-hacks" "\
Move backward to the start of the HTML paragraph, where `paragraph'
is interpreted in terms of the HTML source.  With arg N, do it N times;
a negative arg -N means move forward N paragraphs." t nil)

(autoload (quote rgr-html-fill-paragraph) "rgr-html-hacks" "\
Fill a paragraph as defined by the rgr-html-forward-paragraph command
\(presently bound to \\[rgr-html-forward-paragraph].  See the
rgr-html-forward-paragraph documentation for details." t nil)

(autoload (quote rgr-html-make-toc) "rgr-html-hacks" "\
Insert a table of contents generated from <h#></h#> lines.  If this
is the first invocation in this buffer, this command inserts at point,
and you will be asked to confirm this.  Otherwise, the previous TOC is
replaced with a new one.

   If given a numeric argument, the items are generated within nested <ul>
forms; otherwise, they are within <ol> forms.

   In order to be seen, the markup for section headers must appear at
the start of the line, and the entire header must appear on one line.
\(You should doublecheck these constraints if the code seems to be
mysteriously ignoring sections.)  If an <a name=\"tag\"> appears on the
previous line, the TOC entry for that section will be wrapped in a
hypertext reference." t nil)

(autoload (quote rgr-html-define-commands) "rgr-html-hacks" "\
Bind the rgr-html-* commands to the usual keys.  Use this as a hook
function if you want it:

	(add-hook 'html-helper-load-hook 'rgr-html-define-commands)
" nil nil)

(autoload (quote rgr-html-helper-mode-hook) "rgr-html-hacks" "\
This does html-helper-mode to my taste.  You probably want to use
rgr-html-define-commands as a load hook instead.  -- rgr, 14-Oct-97." nil nil)

;;;***

;;; Generated autoloads from rgr-html-head.el

(autoload (quote rgr-html-increment-buffer-heading-nesting) "rgr-html-head" "\
Increment or decrement the nesting level of headings in the buffer,
e.g. changing <h1> to <h2>, <h2> to <h3>, etc.  By default, the depth is
increased by one, but this can be changed with a prefix argument, which
can be negative to decrease the nesting depth.  HTML tag nesting errors
are not detected." t nil)

(autoload (quote rgr-html-increment-region-heading-nesting) "rgr-html-head" "\
Increment or decrement the nesting level of headings in the buffer,
e.g. changing <h1> to <h2>, <h2> to <h3>, etc.  By default, the depth is
increased by one, but this can be changed with a prefix argument, which
can be negative to decrease the nesting depth.  HTML tag nesting errors
are not detected." t nil)

(autoload (quote rgr-html-number-headings) "rgr-html-head" "\
Scan the buffer for headings, putting section numbers on them." t nil)

(autoload (quote rgr-html-add-heading-anchors) "rgr-html-head" "\
Add anchors to all headings that do not already have them.
A numeric arg means only go down to that depth." t nil)

;;;***

;;; Generated autoloads from rgr-html-hacks.el

(autoload (quote rgr-html-forward-paragraph) "rgr-html-hacks" "\
Move forward to the end of the HTML paragraph, where `paragraph' is
interpreted in terms of the HTML source.  With arg N, do it N times; a
negative arg -N means move backward N paragraphs.

   A paragraph is delimited by either one or more blank lines (which
separate them), or an HTML tag at the start of the line (possibly with
whitespace in front of it) that may cause the browser to start a new
paragraph.  This is defined broadly as any tag that appears on the
`rgr-html-non-paragraph-tags' list, which is initially just those tags
that do font changes (e.g. <b>, <em>, etc.).  Anchors are handled
specially; <a name=\"...\"> starts a paragraph; <a href=\"...\"> and
</a> do not.

   Note that tags that start paragraphs, including <p> itself, are not
considered to actually do so unless they are at the beginning of an HTML
source line.

   The implementation is conceptually compatible with the default
forward-paragraph (\\[forward-paragraph]) command.  It uses the
`paragraph-separate' regexp to identify lines (usually blank) that
separate paragraphs, but replaces `paragraph-start' with code that looks
for either a separator or an HTML tag as defined above.  As for
\\[forward-paragraph], a paragraph end is the beginning of a line which
is not part of the paragraph to which the end of the previous line
belongs, or the end of the buffer." t nil)

(autoload (quote rgr-html-fill-paragraph) "rgr-html-hacks" "\
Fill a paragraph as defined by the rgr-html-forward-paragraph command
\(presently bound to \\[rgr-html-forward-paragraph].  See the
rgr-html-forward-paragraph documentation for details." t nil)

(autoload (quote rgr-html-make-toc) "rgr-html-hacks" "\
Insert a table of contents generated from <h#></h#> lines.  If this
is the first invocation in this buffer, this command inserts at point,
and you will be asked to confirm this.  Otherwise, the previous TOC is
replaced with a new one.

   If given a numeric argument, the items are generated within nested <ul>
forms; otherwise, they are within <ol> forms.

   In order to be seen, the markup for section headers must appear at
the start of the line, and the entire header must appear on one line.
\(You should doublecheck these constraints if the code seems to be
mysteriously ignoring sections.)  If an <a name=\"tag\"> appears on the
previous line, the TOC entry for that section will be wrapped in a
hypertext reference." t nil)

(autoload (quote rgr-html-define-commands) "rgr-html-hacks" "\
Bind the rgr-html-* commands to the usual keys.  Use this as a hook
function if you want it:

	(add-hook 'html-helper-load-hook 'rgr-html-define-commands)
" nil nil)

(autoload (quote rgr-html-helper-mode-hook) "rgr-html-hacks" "\
This does html-helper-mode to my taste.  You probably want to use
rgr-html-define-commands as a load hook instead.  -- rgr, 14-Oct-97." nil nil)

;;;***

;;;### (autoloads (rgr-html-helper-mode-hook rgr-html-define-commands rgr-html-make-toc rgr-html-fill-paragraph rgr-html-backward-paragraph rgr-html-forward-paragraph) "rgr-html-hacks" "rgr-html-hacks.el" (14007 49584))
;;; Generated autoloads from rgr-html-hacks.el

(autoload (quote rgr-html-forward-paragraph) "rgr-html-hacks" "\
Move forward to the end of the HTML paragraph, where `paragraph' is
interpreted in terms of the HTML source.  With arg N, do it N times; a
negative arg -N means move backward N paragraphs.

   A paragraph is delimited by either one or more blank lines (which
separate them), or an HTML tag at the start of the line (possibly with
whitespace in front of it) that may cause the browser to start a new
paragraph.  This is defined broadly as any tag that appears on the
`rgr-html-non-paragraph-tags' list, which is initially just those tags
that do font changes (e.g. <b>, <em>, etc.).  Anchors are handled
specially; <a name=\"...\"> starts a paragraph; <a href=\"...\"> and
</a> do not.

   Note that tags that start paragraphs, including <p> itself, are not
considered to actually do so unless they are at the beginning of an HTML
source line.

   The implementation is conceptually compatible with the default
forward-paragraph (\\[forward-paragraph]) command.  It uses the
`paragraph-separate' regexp to identify lines (usually blank) that
separate paragraphs, but replaces `paragraph-start' with code that looks
for either a separator or an HTML tag as defined above.  As for
\\[forward-paragraph], a paragraph end is the beginning of a line which
is not part of the paragraph to which the end of the previous line
belongs, or the end of the buffer." t nil)

(autoload (quote rgr-html-backward-paragraph) "rgr-html-hacks" "\
Move backward to the start of the HTML paragraph, where `paragraph'
is interpreted in terms of the HTML source.  With arg N, do it N times;
a negative arg -N means move forward N paragraphs." t nil)

(autoload (quote rgr-html-fill-paragraph) "rgr-html-hacks" "\
Fill a paragraph as defined by the rgr-html-forward-paragraph command
\(presently bound to \\[rgr-html-forward-paragraph].  See the
rgr-html-forward-paragraph documentation for details." t nil)

(autoload (quote rgr-html-make-toc) "rgr-html-hacks" "\
Insert a table of contents generated from <h#></h#> lines.  If this
is the first invocation in this buffer, this command inserts at point,
and you will be asked to confirm this.  Otherwise, the previous TOC is
replaced with a new one.

   If given a numeric argument, the items are generated within nested <ul>
forms; otherwise, they are within <ol> forms.

   In order to be seen, the markup for section headers must appear at
the start of the line, and the entire header must appear on one line.
\(You should doublecheck these constraints if the code seems to be
mysteriously ignoring sections.)  If an <a name=\"tag\"> appears on the
previous line, the TOC entry for that section will be wrapped in a
hypertext reference." t nil)

(autoload (quote rgr-html-define-commands) "rgr-html-hacks" "\
Bind the rgr-html-* commands to the usual keys.  Use this as a hook
function if you want it:

	(add-hook 'html-helper-load-hook 'rgr-html-define-commands)
" nil nil)

(autoload (quote rgr-html-helper-mode-hook) "rgr-html-hacks" "\
This does html-helper-mode to my taste.  You probably want to use
rgr-html-define-commands as a load hook instead.  -- rgr, 14-Oct-97." nil nil)

;;;***

;;;### (autoloads (rgr-html-add-heading-anchors rgr-html-number-headings rgr-html-increment-region-heading-nesting rgr-html-increment-buffer-heading-nesting) "rgr-html-head" "rgr-html-head.el" (13622 43206))
;;; Generated autoloads from rgr-html-head.el

(autoload (quote rgr-html-increment-buffer-heading-nesting) "rgr-html-head" "\
Increment or decrement the nesting level of headings in the buffer,
e.g. changing <h1> to <h2>, <h2> to <h3>, etc.  By default, the depth is
increased by one, but this can be changed with a prefix argument, which
can be negative to decrease the nesting depth.  HTML tag nesting errors
are not detected." t nil)

(autoload (quote rgr-html-increment-region-heading-nesting) "rgr-html-head" "\
Increment or decrement the nesting level of headings in the buffer,
e.g. changing <h1> to <h2>, <h2> to <h3>, etc.  By default, the depth is
increased by one, but this can be changed with a prefix argument, which
can be negative to decrease the nesting depth.  HTML tag nesting errors
are not detected." t nil)

(autoload (quote rgr-html-number-headings) "rgr-html-head" "\
Scan the buffer for headings, putting section numbers on them." t nil)

(autoload (quote rgr-html-add-heading-anchors) "rgr-html-head" "\
Add anchors to all headings that do not already have them.
A numeric arg means only go down to that depth." t nil)

;;;***

;;;### (autoloads (rgr-html-check-tag-nesting) "rgr-html-nest" "rgr-html-nest.el" (14080 8801))
;;; Generated autoloads from rgr-html-nest.el

(autoload (quote rgr-html-check-tag-nesting) "rgr-html-nest" "\
Go through the entire current buffer, looking for possible HTML tag
nesting errors (the definition of which appears below).  If one problem
is found, then print a message and go there.  If two or more problems
are found, then they are described in a `compilation' buffer, which is
displayed in another window -- invoking \\[next-error] will visit the errors
one at a time.

   I don't really understand the whole deal with nesting rules (is there
an HTML ref somewhere?), but I do know the following:

   1.  Every <b> needs a </b> to match.  This applies to all tags except
a few such as <hr>, <li>, etc.  Those tags appear as symbols on the
rgr-html-tags-that-do-not-nest list.

   2.  Some tags, most notably `<p>', may or may not nest.  These are
listed on rgr-html-tags-that-may-nest, which means that all of them must
nest, or none of them.  Following current practice, `<p>' is on this
list; remove it if you wish to enforce HTML 3.0 containerization of
paragraph tags.  Also present is `a-name', which is short for
`<a name=...>'.

   3.  For the most part, tags nest independently of each other.  That
is, `<b>this<i>is</b>wierd</i>' is legal, if eccentric.  (And the `is'
should come out in italic-bold -- most browsers these days are smart
enough.)  The exceptions (e.g. `<ol><ul></ol></ul>') are *not* detected.

   4.  Nesting depths of more than one, as in `<em>foo<em>bar</em>baz</em>',
are perfectly fine, though `bar' is not likely to be doubly emphasized
on that account.

   Since it is a common error to put `<em>' where `</em>' is meant, the
first place the nesting depth goes over 1 is used as the error point if
there is an unequal number of open and close tags.  Unfortunately, this
heuristic loses for badly nested `<ol>' and `<ul>' constructs; since
these are frequently nested, this command will erroneously report the
first nested case as being in error." t nil)

;;;***

;;;### (autoloads (rgr-html-check-directory-hrefs rgr-html-fix-buffer-hrefs rgr-html-check-buffer-hrefs rgr-html-update-all-buffer-anchors rgr-html-update-buffer-anchors rgr-html-flush-file-anchors) "rgr-html-tags" "rgr-html-tags.el" (14034 10974))
;;; Generated autoloads from rgr-html-tags.el

(autoload (quote rgr-html-flush-file-anchors) "rgr-html-tags" "\
Forget all anchor definitions from the database for the given file." t nil)

(autoload (quote rgr-html-update-buffer-anchors) "rgr-html-tags" nil t nil)

(autoload (quote rgr-html-update-all-buffer-anchors) "rgr-html-tags" "\
Update the anchor database for all (HTML) buffers." t nil)

(autoload (quote rgr-html-check-buffer-hrefs) "rgr-html-tags" "\
Check the rest of the current buffer for references to undefined
anchors.  With a numeric argument, offers to replace an undefined
reference to one in another file (with caveats about the extensiveness
of the tag database).  Can't handle offsite references." t nil)

(autoload (quote rgr-html-fix-buffer-hrefs) "rgr-html-tags" "\
Fix references to undefined anchors in the rest of the current buffer.
Offers to replace an undefined reference to one in another file (with
caveats about the extensiveness of the tag database).  Can't handle
offsite references." t nil)

(autoload (quote rgr-html-check-directory-hrefs) "rgr-html-tags" "\
Check references to undefined anchors in the current buffer, and everything
in the same directory that is reachable from the current buffer." t nil)

;;;***


(provide 'rgr-public-hacks)
