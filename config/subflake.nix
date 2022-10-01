{ self, nixpkgs, setup, blog, files, static }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ setup.overlays.default ]; };
    writeText "homepage.json" (builtins.toJSON config);

  config =
    builtins.fromJSON (builtins.readFile ./homepage.json) // {
      revision = if self ? rev then self.rev else null;
      directory-blog = "${blog.packages.x86_64-linux.default}";
      directory-files = "${files.packages.x86_64-linux.default}";
      directory-static = "${static.packages.x86_64-linux.default}";
      blog-entries = blog.entries;
    };

}
