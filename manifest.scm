(use-modules (guix packages)
             (guix download)
             ((guix licenses) #:prefix license:)
             (guix gexp)
             (guix build-system haskell)
             (gnu packages commencement)
             (gnu packages haskell)
             (gnu packages haskell-apps)
             (gnu packages haskell-check)
             (gnu packages haskell-web)
             (gnu packages haskell-xyz)
             (gnu packages linux))

(define-public ghc-lrucache
  (package
    (name "ghc-lrucache")
    (version "1.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "lrucache" version))
       (sha256
        (base32 "11avhnjnb89rvn2s41jhh5r40zgp7r6kb5c0hcfiibpabqvv46pw"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "lrucache")))
    (inputs (list ghc-contravariant))
    (arguments
     `(#:cabal-revision ("1"
                         "0v2wc5k2knvv5knbarzspmbzf657r52jyjm9kf6r4ylsmi9cbq0k")))
    (home-page "https://github.com/chowells79/lrucache")
    (synopsis "Implementation of a simple LRU cache")
    (description
     "This package contains a simple and pure @acronym{LRU, Least Recently
Used} cache.  The implementation is based on @code{Data.Map} from the
@code{containers} library.  Further, the package also containes a multiple IO
wrapper that enables atomic updates of the cache.")
    (license license:bsd-3)))

(define-public ghc-hakyll
  (package
    (name "ghc-hakyll")
    (version "4.16.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hakyll" version))
       (sha256
        (base32 "18wg5ay6l3ngsmqq00g6y7djmg4f8285kwdi47g0rg70mq6sn0py"))))
    (build-system haskell-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "tests/Hakyll/Core/UnixFilter/Tests.hs"
                (("unixFilter \"([^\"]+)\"" all cmd)
                 (string-append
                   "unixFilter "
                   "\""
                   (search-input-file inputs (string-append "/bin/" cmd))
                   "\""))))))))
    (properties '((upstream-name . "hakyll")))
    (inputs (list ghc-aeson
                  ghc-blaze-html
                  ghc-data-default
                  ghc-file-embed
                  ghc-hashable
                  ghc-lrucache
                  ghc-network-uri
                  ghc-optparse-applicative
                  ghc-random
                  ghc-regex-tdfa
                  ghc-resourcet
                  ghc-scientific
                  ghc-tagsoup
                  ghc-time-locale-compat
                  ghc-vector
                  ghc-wai-app-static
                  ghc-yaml
                  ghc-xml-conduit
                  ghc-wai
                  ghc-warp
                  ghc-http-types
                  ghc-fsnotify
                  ghc-http-conduit
                  ghc-pandoc
                  ghc-pandoc-types))
    (native-inputs (list ghc-quickcheck ghc-tasty ghc-tasty-golden
                         ghc-tasty-hunit ghc-tasty-quickcheck util-linux))
    (home-page "https://jaspervdj.be/hakyll/")
    (synopsis
     "This package provides a Haskell-based static website compiler library")
    (description
     "Hakyll is a static website compiler library.  That is, it provides you
with the tools to create a simple or advanced static website using a Haskell
@acronym{EDSL, Embedded Domain Specific Language} and input formats such as
Markdown or @acronym{RST, reStructuredText}.")
    (license license:bsd-3)))

(packages->manifest
  (list
    chimerautils
    cabal-install
    gcc-toolchain
    ghc
    ghc-hakyll))
