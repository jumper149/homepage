let
  parse-base-url-authority-port = port:
    ":${port}";
  parse-base-url-authority-maybe-port = maybe-port:
    if isNull maybe-port
    then ""
    else parse-base-url-authority-port maybe-port;
  parse-base-url-authority = authority:
    "//${authority.host}";
  parse-base-url-maybe-authority = maybe-authority:
    if isNull maybe-authority
    then ""
    else parse-base-url-authority maybe-authority;
  parse-base-url-path-segments = path-segments:
    if [] == path-segments
    then ""
    else map (segment: "/${segment}") path-segments;
  parse-base-url = base-url:
    "${base-url.scheme}:${parse-base-url-maybe-authority base-url.authority}${parse-base-url-path-segments base-url.path}";
in
  parse-base-url
