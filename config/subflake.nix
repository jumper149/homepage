{ self, nixpkgs }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    writeText "homepage.json" (builtins.toJSON config);

  config =
    self.subflakes.setup.config // {
      revision = if self ? rev then self.rev else null;
      directory-blog = "${self.subflakes.blog.packages.x86_64-linux.default}";
      directory-files = "${self.subflakes.files.packages.x86_64-linux.default}";
      directory-static = "${self.subflakes.static.packages.x86_64-linux.default}";
      blog-entries = self.subflakes.blog.entries;
      file-entries = self.subflakes.files.entries;
    };

}
