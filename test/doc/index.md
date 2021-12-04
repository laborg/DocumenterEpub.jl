# Test page
# h1
Julia is a high-level, high-performance dynamic programming language for
technical computing, with syntax that is familiar to users of other
technical computing environments.

## h2
Julia is a high-level, high-performance dynamic programming language for
technical computing, with syntax that is familiar to users of other
technical computing environments.

### h3
Julia is a high-level, high-performance dynamic programming language for
technical computing, with syntax that is familiar to users of other
technical computing environments.

#### h4
Julia is a high-level, high-performance dynamic programming language for
technical computing, with syntax that is familiar to users of other
technical computing environments.

##### h5
Julia is a high-level, high-performance dynamic programming language for
technical computing, with syntax that is familiar to users of other
technical computing environments.

###### h6
Julia is a high-level, high-performance dynamic programming language for
technical computing, with syntax that is familiar to users of other
technical computing environments.


# Tests

A paragraph containing some ``\LaTeX`` markup.

![Julia logo](logo.png)

Here's a quote:

> Julia is a high-level, high-performance dynamic programming language for
> technical computing, with syntax that is familiar to users of other
> technical computing environments.

And a table:

---

| Column One | Column Two | Column Three |
|:---------- | ---------- |:------------:|
| Row `1`    | Column `2` |              |
| *Row* 2    | **Row And column** 2  | Column ``3`` |

---

### Some math

```math
\begin{aligned}
\nabla\cdot\mathbf{E}  &= 4 \pi \rho \\
\nabla\cdot\mathbf{B}  &= 0 \\
\nabla\times\mathbf{E} &= - \frac{1}{c} \frac{\partial\mathbf{B}}{\partial t} \\
\nabla\times\mathbf{B} &= - \frac{1}{c} \left(4 \pi \mathbf{J} + \frac{\partial\mathbf{E}}{\partial t} \right)
\end{aligned}
```

```math
K(\theta,v)
\ =\
\frac{\left(1-\left(\frac{v}{c}\right)^2\right)^\frac{8}{3}}
{\left(1-\frac{v}{c}\cos\theta\right)^4}
K(0)
```

### Latex equations

$$x=3$$

Inline $x=99$ is okay.



---

Another list:

  * item one

  * item two

    ```
    f(x) = x
    ```

  * And a sublist:

      + sub-item one
      + sub-item two

### Admonitions

!!! andone
    I am the god of hellfire
    ```julia
    function admonitionfunction(x)
        42
    end
    ```

!!! danger
    danger...

!!! warning
    warning

!!! note
    note

!!! info
    info

!!! tip
    tip

!!! compat
    compat

### Unicode

```@repl
UniversalDeclarationOfHumanRightsStart = "人人生而自由，在尊严和权利上一律平等。"
"人人生而自由，在尊严和权利上一律平等。"
```

### External link

[https://docs.julialang.org/en/v1/](https://docs.julialang.org/en/v1/)


## Documenter specifics

### @contents

```@contents
```

### @example
```@example
a = 1
b = 2
a + b
```

### @repl

```@repl
3+3 # should be 6, right?
```

### @setup
```@setup abc
abctext = "not shown"
```

```@example abc
println(abctext)
```

### @eval

```@eval
nine = 6+3
```


### doctest

```jldoctest; output = false
3+3
# output
6
```

### Refs

[Syntax highlighting](@ref)

An issue: [#42](@ref)

### Syntax highlighting

And some inline code `identity(x) = x`.

```julia

Vector{Int} where Int
Vector{UserType} where UserType

struct Struct
    x::Int
    y::Union{String,UserType}
end

struct Struct <: AbstractStruct
    x::Int
    y::UserType
end

struct Struct{T} <: AbstractStruct{T}
    x::Int
    y::Union{String,UserType}
end

mutable struct MutableStruct
    x::UserType
end

mutable struct MutableStruct{T} <: AbstractVector{T}
    x::String
end

function sayhi(who::String = "world")
    println("hello, " who)
end

function sayhi(who::T) where T <: AbstractString
    println("hello, " who)
end

function Base.print(who::T) where T
    println("hello, " who)
end

saybye(who::String = "world") = println("goodbye, ", who)
saybye(who::T) where T <: AbstractString = println("goodbye, ", who)
Base.print(who::T) where T = println("goodbye, ", who)

r = r"single line regex"
r = r"""
multiline
regex
"""

```

### RAW html

```@raw html
<svg xmlns="http://www.w3.org/2000/svg" style="display: block; margin: 0 auto;" width="5em" height="5em">
	<circle cx="2.5em" cy="2.5em" r="2em" stroke="black" stroke-width=".1em" fill="red" />
</svg>
```
