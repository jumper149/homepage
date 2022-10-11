# Blog

The articles are generated using Asciidoctor.

## Development

```
# Enter development shell.
nix develop ..#subflakes.blog.devShells.x86_64-linux.default

# Generate a PDF preview of a new article `example.adoc`.
make build/example.pdf
```
