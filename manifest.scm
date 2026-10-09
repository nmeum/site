(use-modules (gnu packages commencement)
             (gnu packages curl)
             (gnu packages entr)
             (gnu packages haskell)
             (gnu packages haskell-apps)
             (gnu packages haskell-xyz)
             (gnu packages nss)
             (gnu packages linux))

(packages->manifest
  (list
    cabal-install
    chimerautils
    curl ;needed by cabal for https:// URLs.
    gcc-toolchain ;required by cabal.
    ghc
    nss-certs ;needed by curl for TLS support.
    ghc-hakyll
    entr))
