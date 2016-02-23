---
title: Some technical details about this blog
author: Călin Ardelean
tags: meta
math: true
---

The markdown and the beast
--------------------------

Overall I'm quite pleased with [Hakyll], the Haskell [Jakyll].

For convenient [GitHub Pages][github] hosting, you can either keep the sources
in the same repo with the output, in which case you'll need some convoluted
scripts to strap it together, or you can figure it's not worth the bother and
just manage a slightly more tedious [2][repo1] - [repo][repo2] setup, like me.
After committing changes in the source repo, I run `site deploy` and then
`git commit` and `git push` the front repo.
I even dig the extra control.

*Hakyll* comes integrated with the mighty, multilingual [pandoc], that
champions an enriched [Markdown] dialect I'm delighted to dance with.
In particular, you can write paper-like articles, publish them straightforwardly
on your blog, and generate alongside a *pdf* with the `pandoc` executable,
all the while inlining *LaTeX* for math.

To render the formulas you can pick a standard *js* service like *MathJax* or,
with the [`WebTeX`][HTMLMathMethod] constructor, a remote image service like
*Google Chart API*, or you can generate math images directly on your server
with the [__latex-formulae-hakyll__][lfh] package, all painless to setup.
Currently, the blog employs the [`MathML`][HTMLMathMethod] variant to deliver
*MathML* directly inside the *XHTML*.
Moreover, it injects an icon under the headline (but only if the *Markdown*
source file is explicitly tagged with `math` in the header section, like this
post's), to encourage readers to use a browser sporting native *MathML*, like
*Firefox* or *Safari*.

Here's a test formula for you:
$\hat{f}(\xi) = \int_{-\infty}^\infty f(x)\ e^{- 2\pi i x \xi}\,dx.$

Web theme
---------

I started with the default *Hakyll* theme but made a few adjustments.

All sizes, including fonts and page width, are relative (in `em` units).
There is also no `font-family` setting, so the site will use the default fonts
on all devices.
High-dpi displays won't collapse the text into a string of microscopic black
holes, while on wide screens the need for binoculars and current line
markers for navigation will be eschewed.

The black icons are vectorial (*SVG*), so they look smooth on high-dpi too.
With a *SVG* editor like [Inkscape] it's snappy to take some public domain
icon from the net (wikipedia has plenty) and delete layers, change colors and
sizes, or add your own shapes.
The hover effect just uses the *CSS3* `opacity` property.

I followed [Dan Piponi][dan]'s example and inlined the *archive* page into *home*
and *contact* into the footer.
I reckoned the text that would normally go there would be something
generic, and surely I'd write more compelling things in the actual blog?!?
So why would I pin the trivia?
These days, personal info belongs to social networking accounts.
And being more honest by placing the index inside `index.html` should benefit
search engine robots' situational awareness (humans' too!).

Hope this pasta is plain enough for a copy.

[Hakyll]: http://jaspervdj.be/hakyll "Hakyll"
[Jakyll]: http://jekyllrb.com/ "Jakyll"
[github]: https://pages.github.com "GitHub Pages"
[repo1]: https://github.com/mmn80/mmn80.github.io.src "Source Repo for this Blog"
[repo2]: https://github.com/mmn80/mmn80.github.io "Main GitHub Pages Repo"
[pandoc]: http://pandoc.org/README.html "Pandoc User’s Guide"
[Markdown]: http://daringfireball.net/projects/markdown/ "Markdown"
[HTMLMathMethod]: http://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Options.html#t:HTMLMathMethod "Documentation for pandoc math rendering options"
[lfh]: https://hackage.haskell.org/package/latex-formulae-hakyll-0.2.0.1 "The latex-formulae-hakyll package on Hackage"

[Inkscape]: https://inkscape.org/en/ "Inkscape"
[dan]: http://blog.sigfpe.com/ "A Neighborhood of Infinity"
