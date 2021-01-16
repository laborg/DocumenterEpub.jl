module DocumenterEpub

import Markdown
using Dates: Dates, now, UTC

import Documenter
import Documenter.Documents
import Documenter.Writers
import Documenter.Writers.FormatSelector
import Documenter.Anchors
import Documenter.Selectors
import Documenter.Documenter
import Documenter.Utilities
using Documenter.Utilities.DOM: DOM, Tag, @tags
using Documenter.Utilities: Default
using Documenter.Utilities.MDFlatten

import NodeJS
import ZipFile
import EzXML
import Rsvg
import Cairo

export EPUB

#####

abstract type EPUBFormat <: Documenter.Writers.FormatSelector end

Selectors.order(::Type{EPUBFormat}) = 4.0 # or a higher number.
Selectors.matcher(::Type{EPUBFormat}, fmt, _) = isa(fmt, EPUB)
Selectors.runner(::Type{EPUBFormat}, fmt, doc) = render(doc, fmt)


"""
    EPUB(;[color=false, lang="en"])

EPUB format Writer. `color` specifies whether or not the code highlighting will be grayscale
or in color. `lang` sets the language in the EPUB.
"""
Base.@kwdef struct EPUB <: Documenter.Writer
    color::Bool = false
    lang::String = "en"
end

"""
[`DocumenterEpub`](@ref)-specific globals that are passed to [`domify`](@ref) and
other recursive functions.
"""
mutable struct EPUBContext
    doc::Documents.Document
    settings::EPUB
    ids::Set{String}
    footnotes::Vector{Markdown.Footnote}
end

EPUBContext(doc, settings=EPUB(doc)) = EPUBContext(doc, settings, Set([]), [])

"""
Returns a page (as a [`Documents.Page`](@ref) object) using the [`HTMLContext`](@ref).
"""
getpage(ctx, path) = ctx.doc.blueprint.pages[path]
getpage(ctx, navnode::Documents.NavNode) = getpage(ctx, navnode.page)

function toepub(doc)
    epub_dir = joinpath(doc.user.build, "epub")
    path = joinpath(doc.user.build, "$(doc.user.sitename).epub")
    w = ZipFile.Writer(path)
    for (root, dirs, files) in walkdir(epub_dir)
        for f in files
            p = joinpath(root, f)
            zf = ZipFile.addfile(w, relpath(p, epub_dir))
            write(zf, read(p))
            @debug "Zipping epub file: $p"
        end
    end
    close(w)
end


const OPFNS = [
    "ns" => "http://www.idpf.org/2007/opf",
    "dc" => "http://purl.org/dc/elements/1.1/"
    ]

# create functions operating on the content.opf
for f in (findfirst, findlast, findall)
    @eval $(Symbol("opf_" * string(f)))(xp, doc) = $f(xp, doc.root, OPFNS)
end

function update_content_metadata(doc;
    title,
    id=title,
    lang="en",
    modified_date=now(UTC),
    source=nothing)

    # identifier
    identifier_el = opf_findfirst("/ns:package/ns:metadata/dc:identifier", doc)
    identifier_el.content = id

    # modified date
    meta_el = opf_findfirst("/ns:package/ns:metadata/ns:meta", doc)
    meta_el.content = string(Dates.format(modified_date, "YYYY-mm-ddTHH:MM:SS"), "Z")

    # title
    title_el = opf_findfirst("/ns:package/ns:metadata/dc:title", doc)
    title_el.content = title

    # language
    language_el = opf_findfirst("/ns:package/ns:metadata/dc:language", doc)
    language_el.content = lang

    # source [optional]
    if source !== nothing
        metadata_el = opf_findfirst("/ns:package/ns:metadata", doc)
        addelement!(metadata_el, "dc:source", source)
    end
    return nothing
end

_build_xhtml_navcontent(ctx,nnv::Vector, iob) = foreach(nn -> _build_xhtml_navcontent(ctx, nn, iob), nnv)
function _build_xhtml_navcontent(ctx, nn, iob)
    @tags a span
    print(iob, "<li>")
    if !isempty(nn.children)
        title = mdflatten(pagetitle(ctx, nn))
        if nn.page !== nothing # hidden can have a target and children..
            page = get_url(nn.page)
            print(iob, a[:href => page](title))
            print(iob, "</li><li>")
        end
        print(iob, span(title))
        print(iob, "<ol>")
        _build_xhtml_navcontent(ctx, nn.children, iob)
        print(iob, "</ol>")
    else
        title = mdflatten(pagetitle(ctx, nn))
        page = get_url(nn.page)
        print(iob, a[:href => page](title))
    end
    print(iob, "</li>")
end
function _create_xhtml_nav(ctx, doc)
    iob = IOBuffer()
    print(iob, "<ol>")
    _build_xhtml_navcontent(ctx, doc.internal.navtree, iob)
    print(iob, "</ol>")
    String(take!(iob))
end
# depth first search for page
function firstpage(nn)
    nn.page !== nothing && return nn.page
    for nnc in nn.children
        fp = firstpage(nnc)
        fp !== nothing && return fp
    end
    return nothing
end
_build_ncx_nav_content(ctx,nnv::Vector, iob) =  foreach(nn -> _build_ncx_nav_content(ctx, nn, iob), nnv)
function _build_ncx_nav_content(ctx, nn, iob)
    @tags navLabel text content
    if !isempty(nn.children)
        page = get_url(firstpage(nn))
        title = mdflatten(pagetitle(ctx, nn))
        print(iob, """<navPoint id="navpoint-$(rand(Int))">""")
        print(iob, navLabel(text(title)))
        print(iob, content[:src => page]())
        _build_ncx_nav_content(ctx, nn.children, iob)
        print(iob, """</navPoint>""")
    else
        title = mdflatten(pagetitle(ctx, nn))
        page = get_url(nn.page)
        print(iob, """<navPoint id="navpoint-$(rand(Int))">""")
        print(iob, navLabel(text(title)))
        print(iob, content[:src => page]())
        print(iob, """</navPoint>""")
    end
end
function _create_ncx_nav(ctx, doc)
    iob = IOBuffer()
    print(iob, "<navMap>")
    _build_ncx_nav_content(ctx, doc.internal.navtree, iob)
    print(iob, "</navMap>")
    String(take!(iob))
end

function _contains_svg(root, pagefile)
    try
        page_as_xml = EzXML.readxml(joinpath(root, pagefile))
        return findfirst("//svg:svg", page_as_xml.root, ["svg" => "http://www.w3.org/2000/svg"]) !== nothing
    catch e
        @debug "Error while checking for svg tag" e
    end
    return false
end


# Copied from https://github.com/JuliaLang/julia/blob/acb7bd93fb2d5adbbabeaed9f39ab3c85495b02f/stdlib/Markdown/src/render/html.jl#L25-L31
const _htmlescape_chars = IdDict()
for ch in """"'`!\$%()=+{}[]<>&"""
    _htmlescape_chars[ch] = "#$(Int(ch));"
end

"""
    html_unescape

Internal function to reverse the escaping of some html code (in order to avoid
double escaping when pre-rendering with highlight, see issue 326).
"""
function html_unescape(cs::AbstractString)
    # this is a bit inefficient but whatever, `cs` shouldn't  be very long.
    for (ssfrom, ssto) in _htmlescape_chars
        cs = replace(cs, ssto => ssfrom)
    end
    return cs
end

# https://www.npmjs.com/package/mathjax
# function prerender_mathjax(formula::String, display_mode::Bool)
#     mathjax = joinpath(@__DIR__, "..", "res", "mathjax","node-main.js")
#     rf = escape_string(replace(formula, '\'' => "\\prime"))
#     resid = rand(1:1000000)
#     html_code = nothing
#     cd(dirname(mathjax)) do
#         html_code= String(read(`$(NodeJS.nodejs_cmd()) -e """
#             require('$(escape_string(mathjax))').init({
#                 loader: {load: ['input/tex', 'output/chtml']}}).then((MathJax) => {
#                 const chtml = MathJax.tex2chtml('$rf', {display: $(display_mode)});
#                 console.log(MathJax.startup.adaptor.outerHTML(chtml));
#             }).catch((err) => console.log(err.message));
#             """`))
#     end
#     return html_code
# end
function prerender_mathjax(formula::String, display_mode::Bool)
    mathjax = joinpath(@__DIR__, "..", "res", "mathjax","node-main.js")
    rf = escape_string(replace(formula, '\'' => "\\prime"))
    pngbuf = IOBuffer()
    svg_code =""
    cd(dirname(mathjax)) do
        svg_code= String(read(`$(NodeJS.nodejs_cmd()) -e """
            require('$(escape_string(mathjax))').init({
                loader: {load: ['input/tex', 'output/svg']}, svg: {
                    fontCache: 'none',
                    'localID':$(rand(1:100000))}
            }).then((MathJax) => {
                const svg = MathJax.tex2svg('$rf', {display: $(display_mode)});
                console.log(MathJax.startup.adaptor.innerHTML(svg));
            }).catch((err) => console.log(err.message));
            """`))
        r = Rsvg.handle_new_from_data(svg_code)
        d = Rsvg.handle_get_dimensions(r);
        cs = Cairo.CairoImageSurface(d.width,d.height,Cairo.FORMAT_ARGB32);
        c = Cairo.CairoContext(cs);
        Rsvg.handle_render_cairo(c,r);
        pngbuf = IOBuffer()
        Cairo.write_to_png(cs,pngbuf);
    end
    return svg_code, pngbuf
end

const HLJS_LANGS = readlines(joinpath(@__DIR__, "..", "res", "supported_langs.txt"))

"""
Takes a html string that may contain `<pre><code ... </code></pre>` blocks and use node and
highlight.js to pre-render them to HTML.
"""
function prerender_highlightjs(hs::String, lang::String)::String
    # select highlight js script (julia one stems from https://fredrikekre.se/posts/highlight-julia/)
    hljsfile = occursin("julia", lang) ? "julia.highlight.min.js" : "highlight.pack.js"
    hljsfile = abspath(joinpath(@__DIR__, "..", "res", hljsfile))

    # buffer to write the JS script
    inbuffer = IOBuffer()
    write(inbuffer, """const hljs = require('$(escape_string(hljsfile))');""")

    # un-escape code string
    cs = escape_string(html_unescape(hs))
    # add to content of jsbuffer
    write(inbuffer, """console.log("<pre><code class=\\"hljs\\">" + hljs.highlight("$lang", "$cs").value + "</code></pre>");""")

    outbuffer = IOBuffer()
    run(pipeline(`$(NodeJS.nodejs_cmd()) -e "$(String(take!(inbuffer)))"`, stdout=outbuffer))
    return String(take!(outbuffer))
end

"""
    safeid(page)
Sanitize `page` for use in the navigation files toc.xhtml and toc.ncx.
"""
safeid(page) = replace(replace(page, "-" => "_"), "/" => "_")

# create a manifest entry for a given 'pagefile'
function _create_manifest_item(root, pagefile)
    pagefile = normpath(pagefile)
    mediatypes = Dict(".jpg|.jpeg" => "image/jpeg",
                    ".gif" => "image/gif",
                    ".png" => "image/png",
                    ".svg" => "image/svg+xml",
                    ".css" => "text/css",
                    ".js" => "application/javascript",
                    ".woff2" => "font/woff2",
                    ".woff" => "font/woff",
                    ".ttf" => "application/vnd.ms-opentype",
                    ".ncx" => "application/x-dtbncx+xml")

    ext = lowercase(splitext(pagefile)[2])
    mediatype = get(mediatypes, ext, "application/xhtml+xml")

    manifest_item = EzXML.ElementNode("item")
    manifest_item["id"] = safeid(pagefile)
    manifest_item["href"] = pagefile
    manifest_item["media-type"] = mediatype
    if mediatype == "application/xhtml+xml" && endswith(pagefile, ".xhtml")
        if _contains_svg(root, pagefile)
            manifest_item["properties"] = "svg"
        end
    end
    if pagefile == "toc.xhtml"
        manifest_item["properties"] = "nav"
    end
    if pagefile == "content.opf"
        return nothing
    end
    return manifest_item
end

function _create_spine_item(pagefile)
    spine_item = EzXML.ElementNode("itemref")
    spine_item["idref"] = safeid(pagefile)
    return spine_item
end

const resdir = Ref{String}("")
const ALLOWED_EXT = (".xhtml", ".png", ".jpg", "jpeg", ".gif", ".md", ".svg", "")
function render(doc::Documents.Document, settings::EPUB=EPUB())
    !isempty(doc.user.sitename) || error("EPUB output requires `sitename`.")

    if isempty(doc.blueprint.pages)
        error("Aborting HTML build: no pages under src/")
    elseif !haskey(doc.blueprint.pages, "index.md")
        @warn "Can't generate landing page (index.html): src/index.md missing" keys(doc.blueprint.pages)
    end
    @info "DocumenterEpub: rendering HTML pages for EPUB."

    epub_content_root = abspath(joinpath(doc.user.build, "epub", "Content"))
    resdir[] = abspath(joinpath(doc.user.build,"generated"))
    mkpath(resdir[])

    # first do the html creation into doc.user.build
    ctx = EPUBContext(doc, settings)
    for page in keys(doc.blueprint.pages)
        idx = findfirst(nn -> nn.page == page, doc.internal.navlist)
        nn = (idx === nothing) ? Documents.NavNode(page, nothing, nothing) : doc.internal.navlist[idx]
        @debug "Rendering $(page) [$(repr(idx))]"
        render_page(ctx, nn)
    end

    # move the created files into from "<doc.user.build>/*" into "<doc.user.build>/epub/Content/*"
    tmp_html_build = mktempdir()
    mv(doc.user.build, tmp_html_build; force=true)
    mkpath(epub_content_root)
    mv(tmp_html_build, epub_content_root;force=true)


    # remove all non allowed files
    for (root, _, files) in walkdir(epub_content_root)
        for f in files
            if lowercase(splitext(f)[2]) ∉ ALLOWED_EXT
                rm(joinpath(root, f))
            end
        end
    end



    # copy the epub template into the `epub_content_root`
    template_source = joinpath(@__DIR__, "..", "res", "template")
    template_target = dirname(epub_content_root)
    for (root, _, files) in walkdir(template_source)
        destpath = joinpath(template_target, relpath(root, template_source))
        isdir(destpath) || mkpath(destpath)

        for f in files
            src = joinpath(root, f)
            dest = joinpath(destpath, f)
            isfile(dest) && error("Destination $dest for as target of template already exists")
            cp(src, dest)
        end
    end

    # remove the unused css
    rm(joinpath(epub_content_root, settings.color ? "grayscale.css" : "github.css"))

    # create the coverpage
    open(joinpath(epub_content_root, "titlepage.xhtml"), "w") do io
        print(io,  render_cover_page(ctx))
    end

    # fill/adjust the epub3 relevant files
    # first the content.opf
    @info "EPUB: Writing meta data to content.opf"
    content_xml_path = joinpath(epub_content_root, "content.opf")
    content_xml_doc = EzXML.readxml(content_xml_path)
    content_xml_ns = EzXML.namespace(content_xml_doc.root)
    content_xml_manifest_node = findfirst("//x:manifest", content_xml_doc.root, ["x" => content_xml_ns])
    content_xml_spine_node = findfirst("//x:spine", content_xml_doc.root, ["x" => content_xml_ns])
    update_content_metadata(content_xml_doc;
        id=doc.user.sitename,
        title=doc.user.sitename,
        lang=settings.lang,
        source=isempty(doc.user.repo) ? nothing : doc.user.repo )

    # walk along the navlist and create manifest and spine item
    # the navlist can contain the pages multiple times, so only do it once for each page
    visited = Set{String}([])
    root = epub_content_root

    # add titlepage
    spine_item = _create_spine_item("titlepage.xhtml")
    manifest_item = _create_manifest_item(root, "titlepage.xhtml")
    if manifest_item !== nothing
        EzXML.link!(content_xml_manifest_node, manifest_item)
        EzXML.link!(content_xml_spine_node, spine_item)
    end

    for nn in doc.internal.navlist
        pagefile = replace(nn.page, r".md$" => ".xhtml")
        pagefile in visited && continue
        spine_item = _create_spine_item(pagefile)
        manifest_item = _create_manifest_item(root, pagefile)
        if manifest_item !== nothing
            EzXML.link!(content_xml_manifest_node, manifest_item)
            EzXML.link!(content_xml_spine_node, spine_item)
        end
        push!(visited, pagefile)
    end

    # there might be files that are not part of the navlist, walk through all files
    # and add any remaining to the manifest
    for (fileroot, _, files) in walkdir(epub_content_root)
        for file in files
            pagefile = normpath(replace(joinpath(relpath(fileroot, root), file), r".md$" => ".xhtml"))
            pagefile in visited && continue
            pagefile == "titlepage.xhtml" && continue # already added
            manifest_item = _create_manifest_item(root, pagefile)
            spine_item = nothing
            if endswith(pagefile, ".xhtml") || endswith(pagefile, ".svg")
                spine_item = _create_spine_item(pagefile)
            end
            if manifest_item !== nothing
                EzXML.link!(content_xml_manifest_node, manifest_item)
                if spine_item !== nothing
                    EzXML.link!(content_xml_spine_node, spine_item)
                end
            end
        end
    end

    # write the adjusted xml content into content.opf
    write(content_xml_path, content_xml_doc)

    # create the navigation file `toc.ncx` for epub2 compatibility
    @info "EPUB: Writing toc.ncx"
    toc_ncx_path = joinpath(epub_content_root, "toc.ncx")
    toc_ncx_doc = EzXML.readxml(toc_ncx_path)
    toc_ncx_ns = EzXML.namespace(toc_ncx_doc.root)
    toc_ncx_doctitle_node = findfirst("//x:docTitle", toc_ncx_doc.root, ["x" => toc_ncx_ns])
    EzXML.addelement!(toc_ncx_doctitle_node, "text", doc.user.sitename)
    toc_ncx_head_node = findfirst("//x:meta[@name='dtb:uid']", toc_ncx_doc.root, ["x" => toc_ncx_ns])
    toc_ncx_head_node["content"] = doc.user.sitename
    toc_ncx_navmap_content = EzXML.unlink!(EzXML.parsexml(_create_ncx_nav(ctx, doc)).root)
    EzXML.link!(toc_ncx_doc.root, toc_ncx_navmap_content)
    write(toc_ncx_path, toc_ncx_doc)

    # create the navigation file `toc.xhtml` for epub3
    @info "EPUB: Writing toc.xhtml"
    toc_xhtml_path = joinpath(epub_content_root, "toc.xhtml")
    toc_xhtml_doc = EzXML.readxml(toc_xhtml_path)
    toc_xhtml_ns = EzXML.namespace(toc_xhtml_doc.root)
    toc_xhtml_nav_node = findfirst("//x:nav", toc_xhtml_doc.root, ["x" => toc_xhtml_ns])
    toc_xhtml_nav_content = EzXML.unlink!(EzXML.parsexml(_create_xhtml_nav(ctx, doc)).root)
    EzXML.link!(toc_xhtml_nav_node, toc_xhtml_nav_content)
    write(toc_xhtml_path, toc_xhtml_doc)

    # zip the directory into a epub file
    toepub(doc)

end

"""
Copies an asset from Documenters `assets/html/` directory to `doc.user.build`.
Returns the path of the copied asset relative to `.build`.
"""
function copy_asset(file, doc)
    src = joinpath(Utilities.assetsdir(), "html", file)
    alt_src = joinpath(doc.user.source, "assets", file)
    dst = joinpath(doc.user.build, "assets", file)
    isfile(src) || error("Asset '$file' not found at $(abspath(src))")

    # Since user's alternative assets are already copied over in a previous build
    # step and they should override Documenter's original assets, we only actually
    # perform the copy if <source>/assets/<file> does not exist. Note that checking
    # the existence of <build>/assets/<file> is not sufficient since the <build>
    # directory might be dirty from a previous build.
    if isfile(alt_src)
        @warn "not copying '$src', provided by the user."
    else
        ispath(dirname(dst)) || mkpath(dirname(dst))
        ispath(dst) && @warn "overwriting '$dst'."
        cp(src, dst, force=true)
    end
    assetpath = normpath(joinpath("assets", file))
    # Replace any backslashes in links, if building the docs on Windows
    return replace(assetpath, '\\' => '/')
end

# Page
# ------------------------------------------------------------------------------

## Cover page

function render_cover_page(ctx)
    @tags html div body head meta link script title h1 p

    return DOM.HTMLDocument(
        html[Symbol("lang") => ctx.settings.lang,
            Symbol("xml:lang") => ctx.settings.lang,
            :xmlns => "http://www.w3.org/1999/xhtml",
            Symbol("xmlns:epub") => "http://www.idpf.org/2007/ops",
            Symbol("xmlns:svg") => "http://www.w3.org/2000/svg"](
                head(
                    meta[:charset => "UTF-8"],
                    title(ctx.doc.user.sitename),
                    link[:href => "style.css", :rel => "stylesheet", :type => "text/css"](),
                ),
                body[:class => "titlepage",Symbol("epub:type") => "frontmatter titlepage"](
                    h1[:class => "titlepage"](uppercase(ctx.doc.user.sitename)),
                    p(ctx.doc.user.version),
                    p(ctx.doc.user.authors)
                ),
            )
        )
end

## Standard page
"""
Constructs and writes the page referred to by the `navnode` to `.build`.
"""
function render_page(ctx, navnode)
    @tags html div body
    page = getpage(ctx, navnode)
    head = render_head(ctx, navnode)
    article = render_article(ctx, navnode)
    htmldoc = render_html(ctx, head,  article)
    open_output(ctx, navnode) do io
        print(io,  htmldoc)
    end
end

## Rendering HTML elements
# ------------------------------------------------------------------------------

"""
Renders the main `<html>` tag.
"""
function render_html(ctx, head,  article)
    @tags html body div
    DOM.HTMLDocument(
    html[Symbol("lang") => ctx.settings.lang,
        Symbol("xml:lang") => ctx.settings.lang,
        :xmlns => "http://www.w3.org/1999/xhtml",
        Symbol("xmlns:epub") => "http://www.idpf.org/2007/ops",
        Symbol("xmlns:svg") => "http://www.w3.org/2000/svg"](
            head,
            body(article),
        )
    )
end

function render_head(ctx, navnode)
    @tags head meta link script title
    src = get_url(navnode)
    stylesheet = ctx.settings.color ? "github.css" : "grayscale.css"

    page_title = "$(mdflatten(pagetitle(ctx, navnode))) · $(ctx.doc.user.sitename)"

    prefix = !isempty(dirname(src)) ? relpath(".", dirname(src)) : "."
    head(
        meta[:charset => "UTF-8"],
        title(page_title),
        link[:href => joinpath(prefix, stylesheet), :rel => "stylesheet", :type => "text/css"](),
        link[:href => joinpath(prefix, "style.css"), :rel => "stylesheet", :type => "text/css"](),
        )

end

# Navigation menu
# ------------------------------------------------------------------------------

function find_image_asset(ctx, name)
    for ext in ["svg", "png", "webp", "gif", "jpg", "jpeg"]
        filename = joinpath("assets", "$(name).$(ext)")
        isfile(joinpath(ctx.doc.user.build, filename)) && return filename
    end
    return nothing
end

# Article (page contents)
# ------------------------------------------------------------------------------

function render_article(ctx, navnode)
    @tags article section hr span a div p aside dl dt dd

    # Build the page itself (and collect any footnotes)
    empty!(ctx.footnotes)
    empty!(ctx.ids)
    art_body = article(domify(ctx, navnode))
    # Footnotes, if there are any
    if !isempty(ctx.footnotes)
        fnotes = map(ctx.footnotes) do f
            fid = "footnote-$(f.id)"
            if length(f.text) == 1 && first(f.text) isa Markdown.Paragraph
                dt(f.id),
                dd[:id => "$(fid)", Symbol("epub:type") => "footnote"](mdconvert(f.text[1].content))
            else
                dt(f.id),
                # passing an empty MD() as `parent` to give it block context
                dd[:id => "$(fid)", Symbol("epub:type") => "footnote"](mdconvert(f.text, Markdown.MD()))
            end
        end
        push!(art_body.nodes, hr())
        push!(art_body.nodes, section[Symbol("epub:type") => "footnotes"](dl(fnotes)))
    end
    return art_body
end

"""
    saneid(anchor)
Sanitize the `anchor`s id.
"""
saneid(a::Anchors.Anchor) = saneid(a.id, a.nth)
function saneid(id, nth=1)

    isempty(id) && return string("id", rand(1_10000))
    # use strict html 4 rules for ids: start with a letter followed by a subset of allowed chars
    if occursin(r"^[^a-zA-Z]", id) || occursin(r"[^a-zA-Z0-9-_:.]", id)
        # just hash it
        return string("id", hash(id) + nth - 1)
    else
        return nth == 1 ? id : string(id, "-", nth)
    end
end

## domify(...)
# ------------

"""
Converts recursively a [`Documents.Page`](@ref), `Markdown` or Documenter
`*Node` objects into HTML DOM.
"""
function domify(ctx, navnode)
    page = getpage(ctx, navnode)
    map(page.elements) do elem
        domify(ctx, navnode, page.mapping[elem])
    end
end

function domify(ctx, navnode, node)
    fixlinks!(ctx, navnode, node)
    mdconvert(node, Markdown.MD(); footnotes=ctx.footnotes)
end

function domify(ctx, navnode, anchor::Anchors.Anchor)
    @tags a
    id = saneid(anchor)
    if id ∈ ctx.ids
        id = string("id-", rand(1:100000))
    end
    push!(ctx.ids, id)
    frag = '#' * id
    if isa(anchor.object, Markdown.Header)
        h = anchor.object
        fixlinks!(ctx, navnode, h)
        DOM.Tag(Symbol("h$(Utilities.header_level(h))"))[:id => id](mdconvert(h.text, h))
    else
        a[:href => frag](domify(ctx, navnode, anchor.object))
    end
end


struct ListBuilder
    es::Vector
end
ListBuilder() = ListBuilder([])

import Base: push!
function push!(lb::ListBuilder, level, node)
    @assert level >= 1
    if level == 1
        push!(lb.es, node)
    else
        if isempty(lb.es) || typeof(last(lb.es)) !== ListBuilder
            push!(lb.es, ListBuilder())
        end
        push!(last(lb.es), level - 1, node)
    end
end

function domify(lb::ListBuilder)
    @tags ul li
    ul(map(e -> e isa ListBuilder ? li(domify(e)) : li(e), lb.es))
end

function domify(ctx, navnode, contents::Documents.ContentsNode)
    @tags a
    navnode_dir = dirname(navnode.page)
    navnode_url = get_url(navnode)
    lb = ListBuilder()
    for (count, path, anchor) in contents.elements
        path = joinpath(navnode_dir, path) # links in ContentsNodes are relative to current page
        path = relhref(navnode_url, get_url(path))
        header = anchor.object
        url = string(path, '#', saneid(anchor))
        node = a[:href => url](mdconvert(header.text; droplinks=true))
        level = Utilities.header_level(header)
        push!(lb, level, node)
    end
    domify(lb)
end

function domify(ctx, navnode, index::Documents.IndexNode)
    @tags a code li ul
    navnode_dir = dirname(navnode.page)
    navnode_url = get_url(navnode)
    lis = map(index.elements) do el
        object, doc, path, mod, cat = el
        path = joinpath(navnode_dir, path) # links in IndexNodes are relative to current page
        path = relhref(navnode_url, get_url(path))
        url = string(path, "#", Utilities.slugify(object))
        li(a[:href => url](code("$(object.binding)")))
    end
    ul(lis)
end

function domify(ctx, navnode, docs::Documents.DocsNodes)
    [domify(ctx, navnode, node) for node in docs.nodes]
end

function domify(ctx, navnode, node::Documents.DocsNode)
    @tags code article header span

    id = saneid(node.anchor)
    if id ∈ ctx.ids
        id = string("id-", rand(1:100000))
    end
    push!(ctx.ids, id)
    article[".docstring"](
        header(
            span[".docstring-binding", :id => id](code("$(node.object.binding)")),
            " — ", # &mdash;
            span[".docstring-category"]("$(Utilities.doccat(node.object))")
        ),
        domify_doc(ctx, navnode, node.docstr)
    )
end

function domify_doc(ctx, navnode, md::Markdown.MD)
    @tags a section div
    if haskey(md.meta, :results)
        # The `:results` field contains a vector of `Docs.DocStr` objects associated with
        # each markdown object. The `DocStr` contains data such as file and line info that
        # we need for generating correct source links.
        map(zip(md.content, md.meta[:results])) do md
            markdown, result = md
            ret = section(div(domify(ctx, navnode, Writers.MarkdownWriter.dropheaders(markdown))))
            # When a source link is available then print the link.
            return ret
        end
    else
        # Docstrings with no `:results` metadata won't contain source locations so we don't
        # try to print them out. Just print the basic docstring.
        section(domify(ctx, navnode, Writers.MarkdownWriter.dropheaders(md)))
    end
end

function domify(ctx, navnode, node::Documents.EvalNode)
    node.result === nothing ? DOM.Node[] : domify(ctx, navnode, node.result)
end

# nothing to show for MetaNodes, so we just return an empty list
domify(ctx, navnode, node::Documents.MetaNode) = DOM.Node[]

function domify(ctx, navnode, raw::Documents.RawNode)
    raw.name === :html ? Tag(Symbol("#RAW#"))(raw.text) : DOM.Node[]
end


# Utilities
# ------------------------------------------------------------------------------

"""
Opens the output file of the `navnode` in write node. If necessary, the path to the output
file is created before opening the file.
"""
function open_output(f, ctx, navnode, mode="w")
    path = joinpath(ctx.doc.user.build, get_url(navnode))
    isdir(dirname(path)) || mkpath(dirname(path))
    open(f, path, mode)
end

"""
Get the relative hyperlink between two [`Documents.NavNode`](@ref)s. Assumes that both
[`Documents.NavNode`](@ref)s have an associated [`Documents.Page`](@ref) (i.e. `.page`
is not `nothing`).
"""
navhref(ctx, to, from) = relhref(get_url(from), get_url(to))

"""
Calculates a relative HTML link from one path to another.
"""
function relhref(from, to)
    pagedir = dirname(from)
    # The regex separator replacement is necessary since otherwise building the docs on
    # Windows will result in paths that have `//` separators which break asset inclusion.
    replace(relpath(to, isempty(pagedir) ? "." : pagedir), r"[/\\]+" => "/")
end

"""
Returns the full path corresponding to a path of a `.md` page file. The the input and output
paths are assumed to be relative to `src/`.
"""
get_url(path::AbstractString) = string(splitext(path)[1], ".xhtml")

"""
Returns the full path of a [`Documents.NavNode`](@ref) relative to `src/`.
"""
get_url(navnode::Documents.NavNode) = get_url(navnode.page)

"""
Tries to guess the page title by looking at the `<h1>` headers and returns the
header contents of the first `<h1>` on a page (or `nothing` if the algorithm
was unable to find any `<h1>` headers).
"""
function pagetitle(page::Documents.Page)
    title = nothing
    for element in page.elements
        if isa(element, Markdown.Header{1})
            @debug title
            title = element.text
            break
        end
    end
    title
end

function pagetitle(ctx, navnode::Documents.NavNode)
    if navnode.title_override !== nothing
        # parse title_override as markdown
        md = Markdown.parse(navnode.title_override)
        # Markdown.parse results in a paragraph so we need to strip that
        if !(length(md.content) === 1 && isa(first(md.content), Markdown.Paragraph))
            error("Bad Markdown provided for page title: '$(navnode.title_override)'")
        end
        return first(md.content).content
    end

    if navnode.page !== nothing
        title = pagetitle(getpage(ctx, navnode))
        title === nothing || return title
    end

    "-"
end

# mdconvert
# ------------------------------------------------------------------------------

const md_block_nodes = [
    Markdown.MD,
    Markdown.BlockQuote,
    Markdown.List,
    Markdown.Admonition,
]

"""
[`MDBlockContext`](@ref) is a union of all the Markdown nodes whose children should
be blocks. It can be used to dispatch on all the block-context nodes at once.
"""
const MDBlockContext = Union{md_block_nodes...}

"""
Convert a markdown object to a `DOM.Node` object.

The `parent` argument is passed to allow for context-dependant conversions.
"""
mdconvert(md; kwargs...) = mdconvert(md, md; kwargs...)

mdconvert(text::AbstractString, parent; kwargs...) = DOM.Node(text)

mdconvert(vec::Vector, parent; kwargs...) = [mdconvert(x, parent; kwargs...) for x in vec]

mdconvert(md::Markdown.MD, parent; kwargs...) = mdconvert(md.content, md; kwargs...)

mdconvert(b::Markdown.BlockQuote, parent; kwargs...) = Tag(:blockquote)(mdconvert(b.content, b; kwargs...))

mdconvert(b::Markdown.Bold, parent; kwargs...) = Tag(:strong)(mdconvert(b.text, parent; kwargs...))

function codelang(infostring::AbstractString)
    m = match(r"^\s*(\S*)", infostring)
    return m[1]
end
function mdconvert(c::Markdown.Code, parent::MDBlockContext; kwargs...)
    @tags pre code span
    language = codelang(c.language)
    if isempty(language) || startswith(language, "@") || language ∉ HLJS_LANGS
        pre(code[".hljs"](c.code))
    else
        rendered_code = prerender_highlightjs(c.code, string(language))
        Tag(Symbol("#RAW#"))(rendered_code)
    end
end

mdconvert(c::Markdown.Code, parent; kwargs...) = Tag(:code)(c.code)

mdconvert(h::Markdown.Header{N}, parent; kwargs...) where {N} = DOM.Tag(Symbol("h$N"))(mdconvert(h.text, h; kwargs...))

mdconvert(::Markdown.HorizontalRule, parent; kwargs...) = Tag(:hr)()

function mdconvert(i::Markdown.Image, parent; kwargs...)
    @tags video img a

    # remove external links
    startswith(i.url, "http") && return i.url

    alt = i.alt === nothing ? i.url : isempty(i.alt) ? "none" : i.alt
    if occursin(r"\.(webm|mp4|ogg|ogm|ogv|avi)$", i.url)
        video[:src => i.url, :controls => "true", :title => alt](
            a[:href => i.url](alt)
        )
    else
        img[:src => i.url, :alt => alt]
    end
end

mdconvert(i::Markdown.Italic, parent; kwargs...) = Tag(:em)(mdconvert(i.text, i; kwargs...))
using Base64

function mdconvert(m::Markdown.LaTeX, ::MDBlockContext; kwargs...)
    @tags div img object
    svgcode, pngbuf = prerender_mathjax(m.formula, true)
    psvg = EzXML.parsexml(svgcode)
    desc = EzXML.ElementNode("desc")
    img = EzXML.ElementNode("img")
    img["src"] ="data:image/png;base64, " * base64encode(take!(pngbuf))
    EzXML.link!(desc,img)
    EzXML.linkprev!(psvg.root.firstnode,desc)
    Tag(Symbol("#RAW#"))(string(psvg.root))
end

function mdconvert(m::Markdown.LaTeX, parent; kwargs...)
    @tags div img object
    svgcode, pngbuf = prerender_mathjax(m.formula, false)
    psvg = EzXML.parsexml(svgcode)
    desc = EzXML.ElementNode("desc")
    img = EzXML.ElementNode("img")
    img["src"] ="data:image/png;base64, " * base64encode(take!(pngbuf))
    EzXML.link!(desc,img)
    EzXML.linkprev!(psvg.root.firstnode,desc)
    Tag(Symbol("#RAW#"))(string(psvg.root))
end

mdconvert(::Markdown.LineBreak, parent; kwargs...) = Tag(:br)()

function mdconvert(link::Markdown.Link, parent; droplinks=false, kwargs...)
    link_text = mdconvert(link.text, link; droplinks=droplinks, kwargs...)
    # remove external links
    droplinks = startswith(link.url, "http") || droplinks
    droplinks ? link_text : Tag(:a)[:href => link.url](link_text)
end

mdconvert(list::Markdown.List, parent; kwargs...) = (Markdown.isordered(list) ? Tag(:ol) : Tag(:ul))(map(Tag(:li), mdconvert(list.items, list; kwargs...)))

mdconvert(paragraph::Markdown.Paragraph, parent; kwargs...) = Tag(:p)(mdconvert(paragraph.content, paragraph; kwargs...))

# For compatibility with versions before Markdown.List got the `loose field, Julia PR #26598
const list_has_loose_field = :loose in fieldnames(Markdown.List)
function mdconvert(paragraph::Markdown.Paragraph, parent::Markdown.List; kwargs...)
    content = mdconvert(paragraph.content, paragraph; kwargs...)
    return (list_has_loose_field && !parent.loose) ? content : Tag(:p)(content)
end

function mdconvert(t::Markdown.Table, parent; kwargs...)
    @tags table tr th td
    alignment_style = map(t.align) do align
        if align == :r
            "text-align: right"
        elseif align == :c
            "text-align: center"
        else
            "text-align: left"
        end
    end
    table(
        tr(map(enumerate(t.rows[1])) do (i, x)
        th[:style => alignment_style[i]](mdconvert(x, t; kwargs...))
    end),
        map(t.rows[2:end]) do x
        tr(map(enumerate(x)) do (i, y) # each cell in a row
            td[:style => alignment_style[i]](mdconvert(y, x; kwargs...))
        end)
    end
    )
end

mdconvert(expr::Union{Expr,Symbol}, parent; kwargs...) = string(expr)

function mdconvert(f::Markdown.Footnote, parent; footnotes=nothing, kwargs...)
    @tags sup a
    if f.text === nothing # => Footnote link
        return sup(a[Symbol("epub:type") => "noteref",:href => "#footnote-$(f.id)"]("[$(f.id)]"))
    elseif footnotes !== nothing # Footnote definition
        push!(footnotes, f)
    else # => Footnote definition, but nowhere to put it
        @error "Bad footnote definition."
    end
    return []
end

function mdconvert(a::Markdown.Admonition, parent; kwargs...)
    @tags header div span
    prefix = (a.category == "danger")  ? "\u26a1"  :
        (a.category == "warning") ? "\u26a0" :
        (a.category == "note")    ? "\u261b"    :
        (a.category == "info")    ? "\u2328"    :
        (a.category == "tip")     ? "\u270e" :
        (a.category == "compat")  ? "\u221e"  : "\u58e"
    colorclass =
        (a.category == "danger")  ? ".is-danger"  :
        (a.category == "warning") ? ".is-warning" :
        (a.category == "note")    ? ".is-info"    :
        (a.category == "info")    ? ".is-info"    :
        (a.category == "tip")     ? ".is-success" :
        (a.category == "compat")  ? ".is-compat"  : ".is-default"
    div[".admonition$(colorclass)",Symbol("epub:type") => "notice"](
        header[".admonition-header"](span[".icon"](prefix), " " * a.title),
        div[".admonition-body"](mdconvert(a.content, a; kwargs...))
    )
end

mdconvert(html::Documents.RawHTML, parent; kwargs...) = Tag(Symbol("#RAW#"))(html.code)

# Select the "best" representation for HTML output.
mdconvert(mo::Documents.MultiOutput, parent; kwargs...) =
    Base.invokelatest(mdconvert, mo.content, parent; kwargs...)
function mdconvert(d::Dict{MIME,Any}, parent; kwargs...)
    if haskey(d, MIME"text/html"())
        out = Documents.RawHTML(d[MIME"text/html"()])
    elseif haskey(d, MIME"image/svg+xml"())
        out = Documents.RawHTML(d[MIME"image/svg+xml"()])
    elseif haskey(d, MIME"image/png"())
        out = Documents.RawHTML(string("<img src=\"data:image/png;base64,", d[MIME"image/png"()], "\" />"))
    elseif haskey(d, MIME"image/webp"())
        out = Documents.RawHTML(string("<img src=\"data:image/webp;base64,", d[MIME"image/webp"()], "\" />"))
    elseif haskey(d, MIME"image/gif"())
        out = Documents.RawHTML(string("<img src=\"data:image/gif;base64,", d[MIME"image/gif"()], "\" />"))
    elseif haskey(d, MIME"image/jpeg"())
        out = Documents.RawHTML(string("<img src=\"data:image/jpeg;base64,", d[MIME"image/jpeg"()], "\" />"))
    elseif haskey(d, MIME"text/latex"())
        # If the show(io, ::MIME"text/latex", x) output is already wrapped in \[ ... \] or $$ ... $$, we
        # unwrap it first, since when we output Markdown.LaTeX objects we put the correct
        # delimiters around it anyway.
        latex = d[MIME"text/latex"()]
        equation = false
        m_bracket = match(r"\s*\\\[(.*)\\\]\s*", latex)
        m_dollars = match(r"\s*\$\$(.*)\$\$\s*", latex)
        if m_bracket === nothing && m_dollars === nothing
            out = Utilities.mdparse(latex; mode=:single)
        else
            out = Markdown.LaTeX(m_bracket !== nothing ? m_bracket[1] : m_dollars[1])
        end
    elseif haskey(d, MIME"text/markdown"())
        out = Markdown.parse(d[MIME"text/markdown"()])
    elseif haskey(d, MIME"text/plain"())
        @tags pre code
        return pre(code[".hljs"](d[MIME"text/plain"()]))
    else
        error("this should never happen.")
    end
    return mdconvert(out, parent; kwargs...)
end

# Fallback
function mdconvert(x, parent; kwargs...)
    @debug "Strange inline Markdown node (typeof(x) = $(typeof(x))), falling back to repr()" x
    repr(x)
end

# fixlinks!
# ------------------------------------------------------------------------------

"""
Replaces URLs in `Markdown.Link` elements (if they point to a local `.md` page) with the
actual URLs.
"""
function fixlinks!(ctx, navnode, link::Markdown.Link)
    fixlinks!(ctx, navnode, link.text)
    Utilities.isabsurl(link.url) && return

    # anything starting with mailto: doesn't need fixing
    startswith(link.url, "mailto:") && return

    # links starting with a # are references within the same file -- there's nothing to fix
    # for such links
    startswith(link.url, '#') && return

    s = split(link.url, "#", limit=2)
    if Sys.iswindows() && ':' in first(s)
        @warn "invalid local link: colons not allowed in paths on Windows in $(Utilities.locrepr(navnode.page))" link = link.url
        return
    end
    path = normpath(joinpath(dirname(navnode.page), first(s)))

    if (endswith(path, ".md") && path in keys(ctx.doc.blueprint.pages)) ||
        (endswith(path, ".xhtml") && string(splitext(path)[1], ".md") in keys(ctx.doc.blueprint.pages))
        # make sure that links to different valid pages are correct
        path = relhref(get_url(navnode), get_url(path))
    elseif isfile(joinpath(ctx.doc.user.build, path))
        # update links to other files that are present in build/ (e.g. either user
        # provided files or generated by code examples)
        path = relhref(get_url(navnode), path)
    else
        @warn "invalid local link: unresolved path in $(Utilities.locrepr(navnode.page))" link.text link.url path
    end

    # Replace any backslashes in links, if building the docs on Windows
    path = replace(path, '\\' => '/')

    link.url = (length(s) > 1) ? "$path#$(saneid(last(s)))" : String(path)
end

function fixlinks!(ctx, navnode, img::Markdown.Image)
    Utilities.isabsurl(img.url) && return

    if Sys.iswindows() && ':' in img.url
        @warn "invalid local image: colons not allowed in paths on Windows in $(Utilities.locrepr(navnode.page))" link = img.url
        return
    end

    path = joinpath(dirname(navnode.page), img.url)
    if isfile(joinpath(ctx.doc.user.build, path))
        path = relhref(get_url(navnode), path)
        # Replace any backslashes in links, if building the docs on Windows
        img.url = replace(path, '\\' => '/')
    else
        @warn "invalid local image: unresolved path in $(Utilities.locrepr(navnode.page))" link = img.url
    end
end

fixlinks!(ctx, navnode, md::Markdown.MD) = fixlinks!(ctx, navnode, md.content)
function fixlinks!(ctx, navnode, a::Markdown.Admonition)
    fixlinks!(ctx, navnode, a.title)
    fixlinks!(ctx, navnode, a.content)
end
fixlinks!(ctx, navnode, b::Markdown.BlockQuote) = fixlinks!(ctx, navnode, b.content)
fixlinks!(ctx, navnode, b::Markdown.Bold) = fixlinks!(ctx, navnode, b.text)
fixlinks!(ctx, navnode, f::Markdown.Footnote) = fixlinks!(ctx, navnode, f.text)
fixlinks!(ctx, navnode, h::Markdown.Header) = fixlinks!(ctx, navnode, h.text)
fixlinks!(ctx, navnode, i::Markdown.Italic) = fixlinks!(ctx, navnode, i.text)
fixlinks!(ctx, navnode, list::Markdown.List) = fixlinks!(ctx, navnode, list.items)
fixlinks!(ctx, navnode, p::Markdown.Paragraph) = fixlinks!(ctx, navnode, p.content)
fixlinks!(ctx, navnode, t::Markdown.Table) = fixlinks!(ctx, navnode, t.rows)

fixlinks!(ctx, navnode, mds::Vector) = map(md -> fixlinks!(ctx, navnode, md), mds)
fixlinks!(ctx, navnode, md) = nothing

# TODO: do some regex-magic in raw HTML blocks? Currently ignored.
# fixlinks!(ctx, navnode, md::Documents.RawHTML) = ...

end
