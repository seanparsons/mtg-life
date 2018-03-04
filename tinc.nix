{ nixpkgs }:
rec {
  compiler = nixpkgs.haskell.packages.ghcjsHEAD;
  resolver =
    let
      callPackage = compiler.callPackage;

      overrideFunction = self: super: rec {
        base-compat = callPackage
          (
            { mkDerivation, base, hspec, QuickCheck, stdenv, unix }:
            mkDerivation {
              pname = "base-compat";
              version = "0.9.3";
              sha256 = "7d602b0f0543fadbd598a090c738e9ce9b07a1896673dc27f1503ae3bea1a210";
              libraryHaskellDepends = [ base unix ];
              testHaskellDepends = [ base hspec QuickCheck ];
              description = "A compatibility layer for base";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        base-orphans = callPackage
          (
            { mkDerivation, base, ghc-prim, hspec, QuickCheck, stdenv }:
            mkDerivation {
              pname = "base-orphans";
              version = "0.6";
              sha256 = "c7282aa7516652e6e4a78ccdfb654a99c9da683875748ad5898a3f200be7ad0e";
              libraryHaskellDepends = [ base ghc-prim ];
              testHaskellDepends = [ base hspec QuickCheck ];
              homepage = "https://github.com/haskell-compat/base-orphans#readme";
              description = "Backwards-compatible orphan instances for base";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        base64-bytestring = callPackage
          (
            { mkDerivation, base, bytestring, containers, HUnit, QuickCheck
            , stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2
            }:
            mkDerivation {
              pname = "base64-bytestring";
              version = "1.0.0.1";
              sha256 = "ab25abf4b00a2f52b270bc3ed43f1d59f16c8eec9d7dffb14df1e9265b233b50";
              libraryHaskellDepends = [ base bytestring ];
              testHaskellDepends = [
                base bytestring containers HUnit QuickCheck test-framework
                test-framework-hunit test-framework-quickcheck2
              ];
              homepage = "https://github.com/bos/base64-bytestring";
              description = "Fast base64 encoding and decoding for ByteStrings";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        cabal-doctest = callPackage
          (
            { mkDerivation, base, Cabal, directory, filepath, stdenv }:
            mkDerivation {
              pname = "cabal-doctest";
              version = "1.0.6";
              sha256 = "decaaa5a73eaabaf3c4f8c644bd7f6e3f428b6244e935c0cf105f75f9b24ed2d";
              libraryHaskellDepends = [ base Cabal directory filepath ];
              homepage = "https://github.com/phadej/cabal-doctest";
              description = "A Setup.hs helper for doctests running";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        call-stack = callPackage
          (
            { mkDerivation, base, nanospec, stdenv }:
            mkDerivation {
              pname = "call-stack";
              version = "0.1.0";
              sha256 = "f25f5e0992a39371079cc25c2a14b5abb872fa7d868a32753aac3a258b83b1e2";
              libraryHaskellDepends = [ base ];
              testHaskellDepends = [ base nanospec ];
              homepage = "https://github.com/sol/call-stack#readme";
              description = "Use GHC call-stacks in a backward compatible way";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        dlist = callPackage
          (
            { mkDerivation, base, Cabal, deepseq, QuickCheck, stdenv }:
            mkDerivation {
              pname = "dlist";
              version = "0.8.0.4";
              sha256 = "acf1867b80cdd618b8d904e89eea33be60d3c4c3aeb80d61f29229a301cc397a";
              libraryHaskellDepends = [ base deepseq ];
              testHaskellDepends = [ base Cabal QuickCheck ];
              homepage = "https://github.com/spl/dlist";
              description = "Difference lists";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        fail = callPackage
          (
            { mkDerivation, base, stdenv }:
            mkDerivation {
              pname = "fail";
              version = "4.9.0.0";
              sha256 = "6d5cdb1a5c539425a9665f740e364722e1d9d6ae37fbc55f30fe3dbbbb91d4a2";
              libraryHaskellDepends = [ base ];
              homepage = "https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail";
              description = "Forward-compatible MonadFail class";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        integer-logarithms = callPackage
          (
            { mkDerivation, array, base, ghc-prim, integer-gmp, nats
            , QuickCheck, smallcheck, stdenv, tasty, tasty-hunit
            , tasty-quickcheck, tasty-smallcheck
            }:
            mkDerivation {
              pname = "integer-logarithms";
              version = "1.0.2";
              sha256 = "31069ccbff489baf6c4a93cb7475640aabea9366eb0b583236f10714a682b570";
              revision = "1";
              editedCabalFile = "0sccd0d6qrcm3a7nni5lqv40g5m5knf965z4skkgbyyhb3z6qsq8";
              libraryHaskellDepends = [ array base ghc-prim integer-gmp nats ];
              testHaskellDepends = [
                base nats QuickCheck smallcheck tasty tasty-hunit tasty-quickcheck
                tasty-smallcheck
              ];
              homepage = "https://github.com/phadej/integer-logarithms";
              description = "Integer logarithms";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        mtl = callPackage
          (
            { mkDerivation, base, stdenv, transformers }:
            mkDerivation {
              pname = "mtl";
              version = "2.2.1";
              sha256 = "cae59d79f3a16f8e9f3c9adc1010c7c6cdddc73e8a97ff4305f6439d855c8dc5";
              revision = "1";
              editedCabalFile = "0fsa965g9h23mlfjzghmmhcb9dmaq8zpm374gby6iwgdx47q0njb";
              libraryHaskellDepends = [ base transformers ];
              homepage = "http://github.com/ekmett/mtl";
              description = "Monad classes, using functional dependencies";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        parallel = callPackage
          (
            { mkDerivation, array, base, containers, deepseq, stdenv }:
            mkDerivation {
              pname = "parallel";
              version = "3.2.1.1";
              sha256 = "323bb9bc9e36fb9bfb08e68a772411302b1599bfffbc6de20fa3437ce1473c17";
              revision = "1";
              editedCabalFile = "12sgigg7r4nmyhbfn1p09ajf4s576yca31b7daj5zpp1mxgb5x7i";
              libraryHaskellDepends = [ array base containers deepseq ];
              description = "Parallel programming library";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        prelude-extras = callPackage
          (
            { mkDerivation, base, stdenv }:
            mkDerivation {
              pname = "prelude-extras";
              version = "0.4.0.3";
              sha256 = "09bb087f0870a353ec1e7e1a08017b9a766d430d956afb88ca000a6a876bf877";
              libraryHaskellDepends = [ base ];
              homepage = "http://github.com/ekmett/prelude-extras";
              description = "Higher order versions of Prelude classes";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        primitive = callPackage
          (
            { mkDerivation, base, ghc-prim, stdenv, transformers }:
            mkDerivation {
              pname = "primitive";
              version = "0.6.3.0";
              sha256 = "cddeff804e0f577f1be0179d5d145dfc170f8bfb66f663b9fba67104a45d9555";
              libraryHaskellDepends = [ base ghc-prim transformers ];
              testHaskellDepends = [ base ghc-prim ];
              homepage = "https://github.com/haskell/primitive";
              description = "Primitive memory-related operations";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        random = callPackage
          (
            { mkDerivation, base, stdenv, time }:
            mkDerivation {
              pname = "random";
              version = "1.1";
              sha256 = "b718a41057e25a3a71df693ab0fe2263d492e759679b3c2fea6ea33b171d3a5a";
              revision = "1";
              editedCabalFile = "1pv5d7bm2rgap7llp5vjsplrg048gvf0226y0v19gpvdsx7n4rvv";
              libraryHaskellDepends = [ base time ];
              testHaskellDepends = [ base ];
              description = "random number library";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        reflection = callPackage
          (
            { mkDerivation, base, semigroups, stdenv }:
            mkDerivation {
              pname = "reflection";
              version = "2.1.3";
              sha256 = "88f81923abd7211e51de7071cd5800b30784e374c193de8cdd7b1c201f8de405";
              libraryHaskellDepends = [ base semigroups ];
              homepage = "http://github.com/ekmett/reflection";
              description = "Reifies arbitrary terms into types that can be reflected back into terms";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        semigroups = callPackage
          (
            { mkDerivation, base, stdenv }:
            mkDerivation {
              pname = "semigroups";
              version = "0.18.4";
              sha256 = "589e3042329a6bcffb5c0e85834143586db22eb7a2aae094d492cd004f685d27";
              libraryHaskellDepends = [ base ];
              homepage = "http://github.com/ekmett/semigroups/";
              description = "Anything that associates";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        stm = callPackage
          (
            { mkDerivation, array, base, stdenv }:
            mkDerivation {
              pname = "stm";
              version = "2.4.5.0";
              sha256 = "31d7db183f13beed5c71409d12747a7f4cf3e145630553dc86336208540859a7";
              libraryHaskellDepends = [ array base ];
              homepage = "https://wiki.haskell.org/Software_transactional_memory";
              description = "Software Transactional Memory";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        text = callPackage
          (
            { mkDerivation, array, base, binary, bytestring, deepseq, directory
            , ghc-prim, HUnit, integer-gmp, QuickCheck, quickcheck-unicode
            , random, stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2
            }:
            mkDerivation {
              pname = "text";
              version = "1.2.3.0";
              sha256 = "20e0b1627f613b32cc7f2d2e8dcc48a4a61938b24f3d14fb77cee694f0c9311a";
              libraryHaskellDepends = [
                array base binary bytestring deepseq ghc-prim integer-gmp
              ];
              testHaskellDepends = [
                array base binary bytestring deepseq directory ghc-prim HUnit
                integer-gmp QuickCheck quickcheck-unicode random test-framework
                test-framework-hunit test-framework-quickcheck2
              ];
              doCheck = false;
              homepage = "https://github.com/haskell/text";
              description = "An efficient packed Unicode text type";
              license = stdenv.lib.licenses.bsd2;
              doHaddock = false;
            }
          )
          { };
        th-abstraction = callPackage
          (
            { mkDerivation, base, containers, ghc-prim, stdenv
            , template-haskell
            }:
            mkDerivation {
              pname = "th-abstraction";
              version = "0.2.6.0";
              sha256 = "e52e289a547d68f203d65f2e63ec2d87a3c613007d2fe873615c0969b981823c";
              revision = "1";
              editedCabalFile = "0k4s4nbg9jlgaza38842jnzs8s01ig85fzmjgd10k9hl02gc3r44";
              libraryHaskellDepends = [
                base containers ghc-prim template-haskell
              ];
              testHaskellDepends = [ base containers template-haskell ];
              homepage = "https://github.com/glguy/th-abstraction";
              description = "Nicer interface for reified information about data types";
              license = stdenv.lib.licenses.isc;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        time-locale-compat = callPackage
          (
            { mkDerivation, base, old-locale, stdenv, time }:
            mkDerivation {
              pname = "time-locale-compat";
              version = "0.1.1.3";
              sha256 = "9144bf68b47791a2ac73f45aeadbc5910be2da9ad174909e1a10a70b4576aced";
              libraryHaskellDepends = [ base old-locale time ];
              homepage = "https://github.com/khibino/haskell-time-locale-compat";
              description = "Compatibility of TimeLocale between old-locale and time-1.5";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        transformers-compat = callPackage
          (
            { mkDerivation, base, ghc-prim, stdenv, transformers }:
            mkDerivation {
              pname = "transformers-compat";
              version = "0.5.1.4";
              sha256 = "d881ef4ec164b631591b222efe7ff555af6d5397c9d86475b309ba9402a8ca9f";
              libraryHaskellDepends = [ base ghc-prim transformers ];
              homepage = "http://github.com/ekmett/transformers-compat/";
              description = "A small compatibility shim exposing the new types from transformers 0.3 and 0.4 to older Haskell platforms.";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        void = callPackage
          (
            { mkDerivation, base, deepseq, hashable, semigroups, stdenv }:
            mkDerivation {
              pname = "void";
              version = "0.7.2";
              sha256 = "d3fffe66a03e4b53db1e459edf75ad8402385a817cae415d857ec0b03ce0cf2b";
              libraryHaskellDepends = [ base deepseq hashable semigroups ];
              homepage = "http://github.com/ekmett/void";
              description = "A Haskell 98 logically uninhabited data type";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        vector = callPackage
          (
            { mkDerivation, base, deepseq, ghc-prim, HUnit, primitive
            , QuickCheck, random, semigroups, stdenv, template-haskell
            , test-framework, test-framework-hunit, test-framework-quickcheck2
            , transformers
            }:
            mkDerivation {
              pname = "vector";
              version = "0.12.0.1";
              sha256 = "b100ee79b9da2651276278cd3e0f08a3c152505cc52982beda507515af173d7b";
              revision = "2";
              editedCabalFile = "0vzr8kra73anchp86knkmkq2afkd1hw6hirldn9vn69frynb1n6y";
              libraryHaskellDepends = [
                base deepseq ghc-prim primitive semigroups
              ];
              testHaskellDepends = [
                base HUnit QuickCheck random template-haskell test-framework
                test-framework-hunit test-framework-quickcheck2 transformers
              ];
              homepage = "https://github.com/haskell/vector";
              description = "Efficient Arrays";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        ref-tf = callPackage
          (
            { mkDerivation, base, stdenv, stm, transformers }:
            mkDerivation {
              pname = "ref-tf";
              version = "0.4.0.1";
              sha256 = "fcb522c5dca437fbd0c0132c56664a71c48fe2c06b150fcfa77d3bad5ce4be0e";
              libraryHaskellDepends = [ base stm transformers ];
              description = "A type class for monads with references using type families";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        StateVar = callPackage
          (
            { mkDerivation, base, stdenv, stm, transformers }:
            mkDerivation {
              pname = "StateVar";
              version = "1.1.0.4";
              sha256 = "7ad68decb5c9a76f83c95ece5fa13d1b053e4fb1079bd2d3538f6b05014dffb7";
              libraryHaskellDepends = [ base stm transformers ];
              homepage = "https://github.com/haskell-opengl/StateVar";
              description = "State variables";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        hashable = callPackage
          (
            { mkDerivation, base, bytestring, criterion, deepseq, ghc-prim
            , HUnit, QuickCheck, random, siphash, stdenv, test-framework
            , test-framework-hunit, test-framework-quickcheck2, text, unix
            }:
            mkDerivation {
              pname = "hashable";
              version = "1.2.6.1";
              sha256 = "94ca8789e13bc05c1582c46b709f3b0f5aeec2092be634b8606dbd9c5915bb7a";
              revision = "2";
              editedCabalFile = "0w4756sa04nk2bw3vnysb0y9d09zzg3c77aydkjfxz1hnl1dvnjn";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [ base bytestring deepseq ];
              testHaskellDepends = [
                base bytestring ghc-prim HUnit QuickCheck random test-framework
                test-framework-hunit test-framework-quickcheck2 text unix
              ];
              benchmarkHaskellDepends = [
                base bytestring criterion ghc-prim siphash text
              ];
              homepage = "http://github.com/tibbe/hashable";
              description = "A class for types that can be converted to a hash value";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        blaze-builder = callPackage
          (
            { mkDerivation, base, bytestring, deepseq, HUnit, QuickCheck
            , stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2, text, utf8-string
            }:
            mkDerivation {
              pname = "blaze-builder";
              version = "0.4.0.2";
              sha256 = "9ad3e4661bf5556d650fb9aa56a3ad6e6eec7575e87d472e8ab6d15eaef163d4";
              libraryHaskellDepends = [ base bytestring deepseq text ];
              testHaskellDepends = [
                base bytestring HUnit QuickCheck test-framework
                test-framework-hunit test-framework-quickcheck2 text utf8-string
              ];
              homepage = "http://github.com/lpsmith/blaze-builder";
              description = "Efficient buffered output";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        tagged = callPackage
          (
            { mkDerivation, base, deepseq, stdenv, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "tagged";
              version = "0.8.5";
              sha256 = "e47c51c955ed77b0fa36897f652df990aa0a8c4eb278efaddcd604be00fc8d99";
              revision = "2";
              editedCabalFile = "0r2knfcq0b4s652vlvlnfwxlc2mkc2ra9kl8bp4zdn1awmfy0ia5";
              libraryHaskellDepends = [
                base deepseq transformers transformers-compat
              ];
              homepage = "http://github.com/ekmett/tagged";
              description = "Haskell 98 phantom types to avoid unsafely passing dummy arguments";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        exceptions = callPackage
          (
            { mkDerivation, base, mtl, QuickCheck, stdenv, stm
            , template-haskell, test-framework, test-framework-quickcheck2
            , transformers, transformers-compat
            }:
            mkDerivation {
              pname = "exceptions";
              version = "0.8.3";
              sha256 = "4d6ad97e8e3d5dc6ce9ae68a469dc2fd3f66e9d312bc6faa7ab162eddcef87be";
              revision = "4";
              editedCabalFile = "18iip6wffnrp1jgnf09gxg4v17ymjank50kjshxvcy9s9l9g13ln";
              libraryHaskellDepends = [
                base mtl stm template-haskell transformers transformers-compat
              ];
              testHaskellDepends = [
                base mtl QuickCheck stm template-haskell test-framework
                test-framework-quickcheck2 transformers transformers-compat
              ];
              homepage = "http://github.com/ekmett/exceptions/";
              description = "Extensible optionally-pure exceptions";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        contravariant = callPackage
          (
            { mkDerivation, base, semigroups, StateVar, stdenv, tagged
            , transformers, transformers-compat, void
            }:
            mkDerivation {
              pname = "contravariant";
              version = "1.4.1";
              sha256 = "c93d3d619fa378f3fdf92c53bb8b04b8f47963b88aba7cfa54b57656189ad0ed";
              libraryHaskellDepends = [
                base semigroups StateVar tagged transformers transformers-compat
                void
              ];
              homepage = "http://github.com/ekmett/contravariant/";
              description = "Contravariant functors";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        uuid-types = callPackage
          (
            { mkDerivation, base, binary, bytestring, containers, criterion
            , deepseq, hashable, HUnit, QuickCheck, random, stdenv, tasty
            , tasty-hunit, tasty-quickcheck, text
            }:
            mkDerivation {
              pname = "uuid-types";
              version = "1.0.3";
              sha256 = "9276517ab24a9b06f39d6e3c33c6c2b4ace1fc2126dbc1cd9806866a6551b3fd";
              revision = "1";
              editedCabalFile = "0iwwj07gp28g357hv76k4h8pvlzamvchnw003cv3qk778pcpx201";
              libraryHaskellDepends = [
                base binary bytestring deepseq hashable random text
              ];
              testHaskellDepends = [
                base bytestring HUnit QuickCheck tasty tasty-hunit tasty-quickcheck
              ];
              benchmarkHaskellDepends = [
                base bytestring containers criterion deepseq random
              ];
              homepage = "https://github.com/aslatter/uuid";
              description = "Type definitions for Universally Unique Identifiers";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        unordered-containers = callPackage
          (
            { mkDerivation, base, bytestring, ChasingBottoms, containers
            , criterion, deepseq, deepseq-generics, hashable, hashmap, HUnit
            , mtl, QuickCheck, random, stdenv, test-framework
            , test-framework-hunit, test-framework-quickcheck2
            }:
            mkDerivation {
              pname = "unordered-containers";
              version = "0.2.9.0";
              sha256 = "6730cb5c4a3e953e2c199d6425be08fd088ff0089a3e140d63226c052e318250";
              libraryHaskellDepends = [ base deepseq hashable ];
              testHaskellDepends = [
                base ChasingBottoms containers hashable HUnit QuickCheck
                test-framework test-framework-hunit test-framework-quickcheck2
              ];
              benchmarkHaskellDepends = [
                base bytestring containers criterion deepseq deepseq-generics
                hashable hashmap mtl random
              ];
              homepage = "https://github.com/tibbe/unordered-containers";
              description = "Efficient hashing-based container types";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        scientific = callPackage
          (
            { mkDerivation, base, binary, bytestring, containers, criterion
            , deepseq, hashable, integer-gmp, integer-logarithms, primitive
            , QuickCheck, smallcheck, stdenv, tasty, tasty-ant-xml, tasty-hunit
            , tasty-quickcheck, tasty-smallcheck, text
            }:
            mkDerivation {
              pname = "scientific";
              version = "0.3.5.2";
              sha256 = "5ce479ff95482fb907267516bd0f8fff450bdeea546bbd1267fe035acf975657";
              revision = "4";
              editedCabalFile = "108m6b9w8l2q4r68mla9m5z47k6ahb0p68hypsmn140hgfr6a8la";
              libraryHaskellDepends = [
                base binary bytestring containers deepseq hashable integer-gmp
                integer-logarithms primitive text
              ];
              testHaskellDepends = [
                base binary bytestring QuickCheck smallcheck tasty tasty-ant-xml
                tasty-hunit tasty-quickcheck tasty-smallcheck text
              ];
              benchmarkHaskellDepends = [ base criterion ];
              homepage = "https://github.com/basvandijk/scientific";
              description = "Numbers represented using scientific notation";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        case-insensitive = callPackage
          (
            { mkDerivation, base, bytestring, criterion, deepseq, hashable
            , HUnit, semigroups, stdenv, test-framework, test-framework-hunit
            , text
            }:
            mkDerivation {
              pname = "case-insensitive";
              version = "1.2.0.10";
              sha256 = "66321c40fffb35f3a3188ba508753b74aada53fb51c822a9752614b03765306c";
              revision = "1";
              editedCabalFile = "153x2i7gw7lyhydlf0924vfxmkk53r65c40104bbha2bhp1vj7fi";
              libraryHaskellDepends = [
                base bytestring deepseq hashable semigroups text
              ];
              testHaskellDepends = [
                base bytestring HUnit test-framework test-framework-hunit text
              ];
              benchmarkHaskellDepends = [ base bytestring criterion deepseq ];
              homepage = "https://github.com/basvandijk/case-insensitive";
              description = "Case insensitive string comparison";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        distributive = callPackage
          (
            { mkDerivation, base, base-orphans, Cabal, cabal-doctest, doctest
            , generic-deriving, hspec, stdenv, tagged, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "distributive";
              version = "0.5.3";
              sha256 = "9173805b9c941bda1f37e5aeb68ae30f57a12df9b17bd2aa86db3b7d5236a678";
              revision = "4";
              editedCabalFile = "1v6b2vnharppjn6w36lxfy0dvl93jzjq7fcyq9cp71650f1g9ai5";
              setupHaskellDepends = [ base Cabal cabal-doctest ];
              libraryHaskellDepends = [
                base base-orphans tagged transformers transformers-compat
              ];
              testHaskellDepends = [ base doctest generic-deriving hspec ];
              homepage = "http://github.com/ekmett/distributive/";
              description = "Distributive functors -- Dual to Traversable";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        attoparsec = callPackage
          (
            { mkDerivation, array, base, bytestring, case-insensitive
            , containers, criterion, deepseq, directory, fail, filepath
            , ghc-prim, http-types, parsec, QuickCheck, quickcheck-unicode
            , scientific, semigroups, stdenv, tasty, tasty-quickcheck, text
            , transformers, unordered-containers, vector
            }:
            mkDerivation {
              pname = "attoparsec";
              version = "0.13.2.2";
              sha256 = "dd93471eb969172cc4408222a3842d867adda3dd7fb39ad8a4df1b121a67d848";
              libraryHaskellDepends = [
                array base bytestring containers deepseq fail scientific semigroups
                text transformers
              ];
              testHaskellDepends = [
                array base bytestring deepseq fail QuickCheck quickcheck-unicode
                scientific semigroups tasty tasty-quickcheck text transformers
                vector
              ];
              benchmarkHaskellDepends = [
                array base bytestring case-insensitive containers criterion deepseq
                directory fail filepath ghc-prim http-types parsec scientific
                semigroups text transformers unordered-containers vector
              ];
              homepage = "https://github.com/bos/attoparsec";
              description = "Fast combinator parsing for bytestrings and text";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        http-types = callPackage
          (
            { mkDerivation, array, base, blaze-builder, bytestring
            , case-insensitive, doctest, hspec, QuickCheck
            , quickcheck-instances, stdenv, text
            }:
            mkDerivation {
              pname = "http-types";
              version = "0.9.1";
              sha256 = "7bed648cdc1c69e76bf039763dbe1074b55fd2704911dd0cb6b7dfebf1b6f550";
              libraryHaskellDepends = [
                array base blaze-builder bytestring case-insensitive text
              ];
              testHaskellDepends = [
                base blaze-builder bytestring doctest hspec QuickCheck
                quickcheck-instances text
              ];
              homepage = "https://github.com/aristidb/http-types";
              description = "Generic HTTP types for Haskell (for both client and server code)";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        comonad = callPackage
          (
            { mkDerivation, base, Cabal, cabal-doctest, containers
            , contravariant, distributive, doctest, semigroups, stdenv, tagged
            , transformers, transformers-compat
            }:
            mkDerivation {
              pname = "comonad";
              version = "5.0.3";
              sha256 = "a7f4584d634051123c547f0d10f88eaf23d99229dbd01dfdcd98cddd41e54df6";
              revision = "1";
              editedCabalFile = "1i72zgxjkbldkwz0g2awf44cm9466ahll89j5kl45vszx4iz0anl";
              setupHaskellDepends = [ base Cabal cabal-doctest ];
              libraryHaskellDepends = [
                base containers contravariant distributive semigroups tagged
                transformers transformers-compat
              ];
              testHaskellDepends = [ base doctest ];
              homepage = "http://github.com/ekmett/comonad/";
              description = "Comonads";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        aeson = callPackage
          (
            { mkDerivation, attoparsec, base, base-compat, base-orphans
            , base16-bytestring, bytestring, containers, deepseq, directory
            , dlist, fail, filepath, generic-deriving, ghc-prim, hashable
            , HUnit, integer-logarithms, nats, QuickCheck, quickcheck-instances
            , scientific, semigroups, stdenv, tagged, template-haskell
            , test-framework, test-framework-hunit, test-framework-quickcheck2
            , text, th-abstraction, time, time-locale-compat, transformers
            , transformers-compat, unordered-containers, uuid-types, vector
            }:
            mkDerivation {
              pname = "aeson";
              version = "1.2.4.0";
              sha256 = "3401dba4fddb92c8a971f6645b38e2f8a1b286ef7061cd392a1a567640bbfc9b";
              libraryHaskellDepends = [
                attoparsec base base-compat bytestring containers deepseq dlist
                fail ghc-prim hashable nats scientific semigroups tagged
                template-haskell text th-abstraction time time-locale-compat
                transformers transformers-compat unordered-containers uuid-types
                vector
              ];
              testHaskellDepends = [
                attoparsec base base-compat base-orphans base16-bytestring
                bytestring containers directory dlist filepath generic-deriving
                ghc-prim hashable HUnit integer-logarithms nats QuickCheck
                quickcheck-instances scientific semigroups tagged template-haskell
                test-framework test-framework-hunit test-framework-quickcheck2 text
                time time-locale-compat transformers transformers-compat
                unordered-containers uuid-types vector
              ];
              homepage = "https://github.com/bos/aeson";
              description = "Fast JSON parsing and encoding";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        bifunctors = callPackage
          (
            { mkDerivation, base, base-orphans, comonad, containers, hspec
            , hspec-discover, QuickCheck, semigroups, stdenv, tagged
            , template-haskell, th-abstraction, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "bifunctors";
              version = "5.5.2";
              sha256 = "332bb2ea19e77dac55282daff8046d89f69514ced5b987779d887e53b5d7cb11";
              libraryHaskellDepends = [
                base base-orphans comonad containers semigroups tagged
                template-haskell th-abstraction transformers transformers-compat
              ];
              testHaskellDepends = [
                base hspec QuickCheck template-haskell transformers
                transformers-compat
              ];
              testToolDepends = [ hspec-discover ];
              homepage = "http://github.com/ekmett/bifunctors/";
              description = "Bifunctors";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        semigroupoids = callPackage
          (
            { mkDerivation, base, base-orphans, bifunctors, Cabal
            , cabal-doctest, comonad, containers, contravariant, distributive
            , doctest, hashable, semigroups, stdenv, tagged, template-haskell
            , transformers, transformers-compat, unordered-containers
            }:
            mkDerivation {
              pname = "semigroupoids";
              version = "5.2.2";
              sha256 = "e4def54834cda65ac1e74e6f12a435410e19c1348e820434a30c491c8937299e";
              revision = "1";
              editedCabalFile = "16pf83y17jbjbqv6rqlz4icdzsv6b10vjci6pf92y7cpizzjw0sy";
              setupHaskellDepends = [ base Cabal cabal-doctest ];
              libraryHaskellDepends = [
                base base-orphans bifunctors comonad containers contravariant
                distributive hashable semigroups tagged template-haskell
                transformers transformers-compat unordered-containers
              ];
              testHaskellDepends = [ base doctest ];
              homepage = "http://github.com/ekmett/semigroupoids";
              description = "Semigroupoids: Category sans id";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        profunctors = callPackage
          (
            { mkDerivation, base, base-orphans, bifunctors, comonad
            , contravariant, distributive, semigroups, stdenv, tagged
            , transformers
            }:
            mkDerivation {
              pname = "profunctors";
              version = "5.2.2";
              sha256 = "e981e6a33ac99d38a947a749179bbea3c294ecf6bfde41660fe6d8d5a2e43768";
              libraryHaskellDepends = [
                base base-orphans bifunctors comonad contravariant distributive
                semigroups tagged transformers
              ];
              homepage = "http://github.com/ekmett/profunctors/";
              description = "Profunctors";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        free = callPackage
          (
            { mkDerivation, base, bifunctors, comonad, containers, distributive
            , exceptions, mtl, prelude-extras, profunctors, semigroupoids
            , semigroups, stdenv, template-haskell, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "free";
              version = "4.12.4";
              sha256 = "c9fe45aae387855626ecb5a0fea6afdb207143cb00af3b1f715d1032d2d08784";
              libraryHaskellDepends = [
                base bifunctors comonad containers distributive exceptions mtl
                prelude-extras profunctors semigroupoids semigroups
                template-haskell transformers transformers-compat
              ];
              homepage = "http://github.com/ekmett/free/";
              description = "Monads for free";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        adjunctions = callPackage
          (
            { mkDerivation, array, base, comonad, containers, contravariant
            , distributive, free, generic-deriving, hspec, hspec-discover, mtl
            , profunctors, semigroupoids, semigroups, stdenv, tagged
            , transformers, transformers-compat, void
            }:
            mkDerivation {
              pname = "adjunctions";
              version = "4.4";
              sha256 = "507c2ef55337ae61c805f8cbc1213dfd7d2b85187342675d662254b8d8a16ae9";
              libraryHaskellDepends = [
                array base comonad containers contravariant distributive free mtl
                profunctors semigroupoids semigroups tagged transformers
                transformers-compat void
              ];
              testHaskellDepends = [ base distributive generic-deriving hspec ];
              testToolDepends = [ hspec-discover ];
              homepage = "http://github.com/ekmett/adjunctions/";
              description = "Adjunctions and representable functors";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        kan-extensions = callPackage
          (
            { mkDerivation, adjunctions, array, base, comonad, containers
            , contravariant, distributive, fail, free, mtl, profunctors
            , semigroupoids, stdenv, tagged, transformers, transformers-compat
            }:
            mkDerivation {
              pname = "kan-extensions";
              version = "5.1";
              sha256 = "193f8e58f267663d5da8e38045b000d0983ac08b84808de42af1a44963f63205";
              libraryHaskellDepends = [
                adjunctions array base comonad containers contravariant
                distributive fail free mtl profunctors semigroupoids tagged
                transformers transformers-compat
              ];
              homepage = "http://github.com/ekmett/kan-extensions/";
              description = "Kan extensions, Kan lifts, the Yoneda lemma, and (co)density (co)monads";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        lens = callPackage
          (
            { mkDerivation, array, base, base-orphans, bifunctors, bytestring
            , Cabal, cabal-doctest, call-stack, comonad, containers
            , contravariant, criterion, deepseq, directory, distributive
            , doctest, exceptions, filepath, free, generic-deriving, ghc-prim
            , hashable, HUnit, kan-extensions, mtl, nats, parallel, profunctors
            , QuickCheck, reflection, semigroupoids, semigroups, simple-reflect
            , stdenv, tagged, template-haskell, test-framework
            , test-framework-hunit, test-framework-quickcheck2
            , test-framework-th, text, th-abstraction, transformers
            , transformers-compat, unordered-containers, vector, void
            }:
            mkDerivation {
              pname = "lens";
              version = "4.15.4";
              sha256 = "742e7b87d7945e3d9c1d39d3f8440094c0a31cd098f06a08f8dabefba0a57cd2";
              setupHaskellDepends = [ base Cabal cabal-doctest filepath ];
              libraryHaskellDepends = [
                array base base-orphans bifunctors bytestring call-stack comonad
                containers contravariant distributive exceptions filepath free
                ghc-prim hashable kan-extensions mtl parallel profunctors
                reflection semigroupoids semigroups tagged template-haskell text
                th-abstraction transformers transformers-compat
                unordered-containers vector void
              ];
              testHaskellDepends = [
                base bytestring containers deepseq directory doctest filepath
                generic-deriving HUnit mtl nats parallel QuickCheck semigroups
                simple-reflect test-framework test-framework-hunit
                test-framework-quickcheck2 test-framework-th text transformers
                unordered-containers vector
              ];
              benchmarkHaskellDepends = [
                base bytestring comonad containers criterion deepseq
                generic-deriving transformers unordered-containers vector
              ];
              homepage = "http://github.com/ekmett/lens/";
              description = "Lenses, Folds and Traversals";
              license = stdenv.lib.licenses.bsd2;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        jsaddle = callPackage
          (
            { mkDerivation, aeson, base, base64-bytestring, bytestring
            , ghcjs-base, ghcjs-prim, lens, primitive, stdenv, text
            , transformers
            }:
            mkDerivation {
              pname = "jsaddle";
              version = "0.9.4.0";
              sha256 = "01af1f5c54a4c6e43913a152dc12693b543e78b74cc2040e637f5841f7626452";
              libraryHaskellDepends = [
                aeson base base64-bytestring bytestring ghcjs-base ghcjs-prim lens
                primitive text transformers
              ];
              description = "Interface for JavaScript that works with GHCJS and GHC";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        jsaddle-dom = callPackage
          (
            { mkDerivation, base, base-compat, ghc-prim, ghcjs-base, ghcjs-prim
            , jsaddle, lens, stdenv, text, transformers
            }:
            mkDerivation {
              pname = "jsaddle-dom";
              version = "0.9.2.0";
              sha256 = "18c8a1a9020d3001ce6fae663887330339693d0f3ffec580e2ed5222b728a792";
              libraryHaskellDepends = [
                base base-compat ghc-prim ghcjs-base ghcjs-prim jsaddle lens text
                transformers
              ];
              description = "DOM library that uses jsaddle to support both GHCJS and GHC";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        ghcjs-dom-jsaddle = callPackage
          (
            { mkDerivation, jsaddle-dom, stdenv }:
            mkDerivation {
              pname = "ghcjs-dom-jsaddle";
              version = "0.9.2.0";
              sha256 = "d4c8d989ed17afae5af35e98cfc253f612f87d10fa04951eb78f7e61877e3616";
              libraryHaskellDepends = [ jsaddle-dom ];
              description = "DOM library that supports both GHCJS and GHC using jsaddle";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        ghcjs-dom = callPackage
          (
            { mkDerivation, base, ghcjs-dom-jsffi, stdenv, text, transformers
            }:
            mkDerivation {
              pname = "ghcjs-dom";
              version = "0.9.2.0";
              sha256 = "4a01996bb07fea34deb1ddfd049e0fd92fc2fe36ef9b3ae0182c230373b71b7a";
              libraryHaskellDepends = [ base ghcjs-dom-jsffi text transformers ];
              description = "DOM library that supports both GHCJS and GHC";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
      };

      newResolver = compiler.override {
        overrides = overrideFunction;
      };

    in newResolver;
}
