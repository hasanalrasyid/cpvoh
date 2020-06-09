{ withHoogle ? false
# , compiler ? "ghc8101"
}:
let
  fetchFromGitHub = { owner, repo, rev, sha256, branch }:
    builtins.fetchTarball {
        inherit sha256;
        url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      };

config = {
  doCheck = false;
  allowBroken = true;
  packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc883.override {
          overrides = (self: super: {
#           fortran-src = pkgs.haskellPackages.callCabal2nix "fortran-src" ../refs/fortran-src {};
#           microlens = pkgs.haskellPackages.callHackage "microlens" "0.4.11.2" {};
#           doctest = pkgs.haskell.lib.dontCheck (pkgs.haskellPackages.callHackage "doctest" "0.16.2" {});
#           memory = pkgs.haskellPackages.callHackage "memory" "0.15.0" {};
#           HsYAML = pkgs.haskellPackages.callHackage "HsYAML" "0.2.1.0" {};
#           pandoc = pkgs.haskellPackages.callHackage "pandoc" "2.9.1.1" {};
#           fortran-src = pkgs.haskell.lib.dontCheck (pkgs.haskellPackages.callHackage "fortran-src" "0.4.0" {});
#           cabal2nix = pkgs.haskell.lib.dontCheck (pkgs.haskellPackages.callHackage "cabal2nix" "2.14.4" {});
            fortran-src = pkgs.haskellPackages.callCabal2nix "fortran-src" (
              builtins.fetchTarball {
                url = "https://github.com/hasanalrasyid/fortran-src/archive/a910caee4ee97244a6cb4eec9b429e4e6b779b40.tar.gz";
                sha256 = "1z2w0mgvvdrxjfkc7nxz1gmil66nmz9ck231w4sgrw5pv92hg300";
            }){};
#           stack = pkgs.haskellPackages.callCabal2nix "stack" (
#             builtins.fetchTarball {
#               url = "https://github.com/commercialhaskell/stack/archive/9dcef52902d01646d63fe76fc8e6b1b3ac6cc9b8.tar.gz";
#               sha256 = "0xs8zmxvklgqmm44xb76w45cgs5kbx2ab1677gmlclkng105px3h";
#           }){};
            });
        };
    };
  };
npkgs = (import ./nixpkgs {inherit config;});
newestpkgs = (import <nixpkgs> {});
pkgs = npkgs.pkgs;
ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [ fortran-src ] );

# this refers to openmpi-4.0.3
openmpi_static = newestpkgs.openmpi.overrideAttrs (oldAttrs: rec {
  configureFlags = oldAttrs.configureFlags ++ [ "--enable-static" ];
  });
this = rec {
    inherit pkgs ghc;
  };
project =
pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "cpvoh";
  buildInputs = with pkgs; [ git protobuf zlib gcc.cc gfortran gfortran.cc haskellPackages.happy haskellPackages.alex
  fftw blas liblapack
#  pkgs.pandoc
  openmpi_static
  which hwloc openssh
  gnuplot ghostscript perl
  ];
  doHaddock = false;
  doCheck = false;
  shellHook = ''
    LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/aku/opt/python3-3.8.2/lib
    PATH=$PATH:/usr/local/bin:/usr/bin:/home/aku/opt/python3-3.8.2/bin
#   . ./.inline-fortran/env
  echo shellHook out
  '';
};
in
this // project
