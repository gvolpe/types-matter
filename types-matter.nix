{ mkDerivation, base, refined, stdenv, template-haskell, text }:
mkDerivation {
  pname = "types-matter";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base refined template-haskell text ];
  executableHaskellDepends = [ base refined ];
  homepage = "https://github.com/gvolpe/types-matter";
  license = stdenv.lib.licenses.asl20;
}
