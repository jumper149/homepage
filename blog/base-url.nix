let
  parse-base-url-authority-maybe-port = maybe-port:
    if isNull maybe-port
    then ""
    else ":${toString maybe-port}";
  parse-base-url-maybe-authority = maybe-authority:
    if isNull maybe-authority
    then ""
    else "//${maybe-authority.host}${parse-base-url-authority-maybe-port maybe-authority.port}";
  parse-base-url-path-segments = path-segments:
    builtins.concatStringsSep "" (map (segment: "/${segment}") path-segments);
  parse-base-url = base-url:
    "${base-url.scheme}:${parse-base-url-maybe-authority base-url.authority}${parse-base-url-path-segments base-url.path}";
in
  parse-base-url
