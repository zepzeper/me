# krugten.org

This is the source code for my personal blog, built with [Hakyll](https://jaspervdj.be/hakyll).

## Building

Prerequisites:
- [Stack](https://docs.haskellstack.org/en/stable/README/)

To build the site:

```bash
stack build
stack exec me build
```

To run a preview server:

```bash
stack exec me watch
```

## Structure

- `posts/`: Blog posts in Markdown format
- `projects/`: Project pages
- `css/`: Stylesheets
- `templates/`: HTML templates
- `site.hs`: Main Hakyll configuration
