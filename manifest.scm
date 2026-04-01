(use-modules (guix packages)
             (guix build-system haskell)
             (guix git-download)
             (gnu packages commencement)
             (gnu packages entr)
             (gnu packages haskell)
             (gnu packages haskell-apps)
             (gnu packages haskell-xyz)
             (gnu packages linux))

;; XXX: build ghc-hakyll from Git until changes required for metadata-only
;; dependencies end up in a new Hakyll release. For more information refer
;; to the following pull request <https://github.com/jaspervdj/hakyll/pull/1084>.
(define-public ghc-hakyll-latest
  (let ((commit "2dc90eb9931203efd821ed68db089f4065e069af")
        (revision "0"))
    (package/inherit ghc-hakyll
      (name "ghc-hakyll-latest")
      (version (git-version "4.16.7.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jaspervdj/hakyll")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1bmjrj27529xpq7kf5y50i2fxyvgqvfa5h86is4dflg9npn1nskq")))))))

(packages->manifest
  (list
    chimerautils
    cabal-install
    gcc-toolchain ;required by cabal.
    ghc
    ghc-hakyll-latest
    entr))
