mmn80.github.io
==============

These are the [Hakyll][hakyll] sources for my blog.

I use `site deploy` to `rsync` the output files into the main [repo][repo].

If `clean` would not just wipe the `_site` folder, but ignore some configurable
subfolders, then it would be easy to keep the sources in the main repo.
You would just set `destinationDirectory` to `..` and add a `_config.yml`
with an `ignore: src` inside so Github's *Jakyll* would skip the sources when rendering.
I tried it, it works, except for the `clean`/`rebuild` thing.

[repo]: https://github.com/mmn80/mmn80.github.io "mmn80.github.io"
[hakyll]: http://jaspervdj.be/hakyll "Hakyll website"
