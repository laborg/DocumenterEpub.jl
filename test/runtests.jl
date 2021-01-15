
using Test
using Documenter
using DocumenterEpub

pages = [
    "index.md",
    "Footnotes" => "footnotes.md",
    "Subpage" => "sub/subpage.md",
    "Autodocs" => "autodocs.md",
    hide("Hidden section" => "hiddenpage.md"),
]

module EPUBTest
# a complicated docstring
"""
    QRCompactWY <: Factorization

A QR matrix factorization stored in a compact blocked format, typically obtained from
[`qr`]. If ``A`` is an `m`×`n` matrix, then

```math
A = Q R
```

where ``Q`` is an orthogonal/unitary matrix and ``R`` is upper triangular. It is similar
to the [`QR`] format except that the orthogonal/unitary matrix ``Q`` is stored in
*Compact WY* format [^Schreiber1989].  For the block size ``n_b``, it is stored as
a `m`×`n` lower trapezoidal matrix ``V`` and a matrix ``T = (T_1 \\; T_2 \\; ... \\;
T_{b-1} \\; T_b')`` composed of ``b = \\lceil \\min(m,n) / n_b \\rceil`` upper triangular
matrices ``T_j`` of size ``n_b``×``n_b`` (``j = 1, ..., b-1``) and an upper trapezoidal
``n_b``×``\\min(m,n) - (b-1) n_b`` matrix ``T_b'`` (``j=b``) whose upper square part
denoted with ``T_b`` satisfying

```math
Q = \\prod_{i=1}^{\\min(m,n)} (I - \\tau_i v_i v_i^T)
= \\prod_{j=1}^{b} (I - V_j T_j V_j^T)
```

such that ``v_i`` is the ``i``th column of ``V``, ``\\tau_i`` is the ``i``th element
of `[diag(T_1); diag(T_2); …; diag(T_b)]`, and ``(V_1 \\; V_2 \\; ... \\; V_b)``
is the left `m`×`min(m, n)` block of ``V``.  When constructed using [`qr`],
the block size is given by ``n_b = \\min(m, n, 36)``.

Iterating the decomposition produces the components `Q` and `R`.

The object has two fields:

* `factors`, as in the [`QR`] type, is an `m`×`n` matrix.

  - The upper triangular part contains the elements of ``R``, that is `R =
    triu(F.factors)` for a `QR` object `F`.

  - The subdiagonal part contains the reflectors ``v_i`` stored in a packed format such
    that `V = I + tril(F.factors, -1)`.

* `T` is a ``n_b``-by-``\\min(m,n)`` matrix as described above. The subdiagonal elements
  for each triangular matrix ``T_j`` are ignored.

!!! note

    This format should not to be confused with the older *WY* representation
    [^Bischof1987].


[^Bischof1987]: C Bischof and C Van Loan, "The WY representation for products of Householder matrices", SIAM J Sci Stat Comput 8 (1987), s2-s13. [doi:10.1137/0908009](https://doi.org/10.1137/0908009)

[^Schreiber1989]: R Schreiber and C Van Loan, "A storage-efficient WY representation for products of Householder transformations", SIAM J Sci Stat Comput 10 (1989), 53-57. [doi:10.1137/0910005](https://doi.org/10.1137/0910005)
"""
complicated(x) = x


"""
    simple(x)
A very simple docstring. [`complicated`](@ref)
"""
simple(x) = x

"""
    multidoc(x)
Invokation with one param.
"""
multidoc(x) = x


"""
    multidoc(x,y)
Invokation with two params.
"""
multidoc(x,y) = x + y

"""
    multidoc(x,y,z)
Invokation with three params.
"""
multidoc(x,y,z) = x + y + z

export complicated, simple
end

# for color in (true, false)
#     sitename= "testepub_" * (color ? "with_color" : "grayscale")
#     makedocs(;
#         sitename=sitename,
#         source=abspath(joinpath(@__DIR__, "doc")),
#         pages=pages,
#         format=EPUB(color=color),
#     )
# end

isepubcheck = false
try
    if Sys.iswindows()
        run(`where epubcheck`)
    else
        run(`which epubcheck`)
    end
    global isepubcheck = true
catch e
    @warn "Couldn't find `epubcheck` - necessary for running the test suite"
end

testconfigs =[
    (;sitename="color",color=true,pages=pages,version="",authors="",lang="en"),
    (;sitename="gray",color=false,pages=pages,version="",authors="MemeLord3000",lang="en"),
    (;sitename="nopages",color=false,pages=[],version="3.0",authors="",lang="de")
]

# the test relies on the external tool "epubcheck" to see if there are errors...
@testset "epubcheck $(config[:sitename])" for config in testconfigs

    makedocs(;
        sitename=config[:sitename],
        modules=[EPUBTest],
        version=config[:version],
        authors=config[:authors],
        source=abspath(joinpath(@__DIR__, "doc")),
        pages=config[:pages],
        format=EPUB(color=config[:color],lang=config[:lang]),
    )

    expectedfile = joinpath(@__DIR__, "build", config[:sitename] * ".epub")
    @test isfile(expectedfile)
    if isepubcheck
        @test_nowarn run(`epubcheck -q $expectedfile`)
    end
end