with import <nixpkgs> {};

runCommand "shake-it-build-inputs" {
  buildInputs = [
    unzip
    coreutils
    eject
    imagemagick
    graphviz
    (ghc.withPackages (p: with p;
      [
        shake
        pandoc
        wreq
        lens
        bytestring
        text
        dhall
        hlint
      ]
    ))
    (texlive.combine {
      inherit (texlive)
      scheme-medium
      beamer
      listings
      minted
      cleveref
      microtype
      babel
      todonotes
      chngcntr
      excludeonly
      upquote
      ifplatform
      xstring
      xcolor
      enumitem;
    })
  ];
} ""
