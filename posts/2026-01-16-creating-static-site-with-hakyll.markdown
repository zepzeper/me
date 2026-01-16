---
title: "My First Blog: Tackling Haskell (Again)"
date: 2026-01-16
description: ""
---

# Hello World

I've been a software engineer for a while now, but this is actually the first time I've ever sat down to write a blog. I've always wanted to share some of the code and tech I'm working on, but I never really got around to setting up a proper place for it.

So, for my first project, I decided to build this site. And naturally, I chose the most complicated way possible: **Hakyll** (not really).

# The Haskell Trauma

I actually had some exposure to Haskell back in school. To be honest, I didn’t really bother with it. It looked scary. It felt less like “writing code” and more like doing math homework. I wanted to build things, not wrestle with abstract theory, so I kind of just got through the class and never looked back.

But recently, I've been wanting to challenge myself. I wanted to see if I could actually build something practical with it.

# Why Hakyll?

Hakyll is a static site generator written in Haskell. Unlike other tools where you just edit a config file, here you basically compile your own website builder.

I wanted something that felt like *code*. The configuration file `site.hs` is just a Haskell program.

## Breaking Down the Code

It turns out, it's not as scary as I remember. Here is a snippet from my site configuration. This handles the rendering of the posts you're reading:

```haskell
match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
```

Even without a PhD in Category Theory, you can sort of see what's happening:

1. **Match** all files in the `posts/` folder.
2. **Route** them to an `.html` extension.
3. **Compile** them using Pandoc (which converts the Markdown to HTML).
4. **Apply** the templates.

It's surprisingly clean.

# What's Next?

I'm still learning the ropes, but I'm excited to actually write more blogs here. I plan to share more snippets and things I learn as I go not just about Haskell, but about whatever tech I'm digging into.

Thanks for reading!
