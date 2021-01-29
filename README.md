# DocumenterEpub.jl

A Julia package to create [EPUB](https://en.wikipedia.org/wiki/EPUB) files from
[Documenter.jl](https://github.com/JuliaDocs/Documenter.jl) sources for use with e-reader
devices.

The code is basically a modified and extended version of Documenters.jl `HTMLWriter` and
took in a couple of Franklin.jl code snippets.

 ### Usage

 Specify the format as `EPUB()`:

 ```julia
using Pkg
Pkg.add("Documenter")
Pkg.add("DocumenterEpub")
using Documenter
using DocumenterEpub
makedocs(;
    sitename="your-publication",    # publication name
    source="path-of-doc",           # as usual
    pages=["index.md","second.md"], # as usual (but won't hide pages)
    version="1.0"                   # shown on the titlepage
    authors="me"                    # shown on the titlepage
    repo="https://github..."        # used to set the source in the EPUB metadata
    format=EPUB(
        color=false,                # syntax highlighting will use colors
        lang="en",                  # publication language
        snap_animation=true         # only take first frame of an animated gif
    )
)
```

### Features / Design choices
 - Target e-ink e-reader devices - Tablets/Computers will not have a problem displaying the
    HTML output version of Documenter
 - Create EPUB 3.2 files, but stay backward compatible where possible (e.g. include the
    navigation as .ncx file)
 - Use a simple layout/CSS to be compatible with older e-reader devices
 - Use [JuliaMono](https://github.com/cormullion/juliamono) for code
 - No JavaScript dependency in the resulting EPUB through:
    - prerendering of syntax highlighting for code listings using Highlight.js, including
        the work of Fredrik Ekre (see the blog post: https://fredrikekre.se/posts/highlight-julia/)
    - prerendering of LaTeX math/formulas into SVG (epub 3.2 spec contains MathML but this is
        poorly supported... )
 - I've chosen the controversial route of letting code listings wrap the source code on
    whitespace
 - Keep the options to a minimum - currently only one option (color=true/false)
 - Remote links are displayed but aren't active
 - Files that can't be shown on e-reader devices are removed (e.g. videos)
