# homepage

Felix Springer's Homepage.

```
# Set up development environment with nix flakes.
nix develop

# Run to debug.
cabal run homepage

# Compile blog.
asciidoctor ./static/blog/myWayToCoreboot.adoc --backend html5 --doctype article --out-file .static/blog/myWayToCoreboot.html --safe-mode secure --no-header-footer
asciidoctor ./static/blog/myOwnImplementationOfIExpressions.adoc --backend html5 --doctype article --out-file ./static/blog/myOwnImplementationOfIExpressions.html --safe-mode secure --no-header-footer
```
