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
      haskellPackages = pkgs.haskell.packages.ghc865.override {
          overrides = (self: super: {
#           microlens = pkgs.haskellPackages.callHackage "microlens" "0.4.11.2" {};
#           doctest = pkgs.haskell.lib.dontCheck (pkgs.haskellPackages.callHackage "doctest" "0.16.2" {});
#           memory = pkgs.haskellPackages.callHackage "memory" "0.15.0" {};
#           HsYAML = pkgs.haskellPackages.callHackage "HsYAML" "0.2.1.0" {};
#           pandoc = pkgs.haskellPackages.callHackage "pandoc" "2.9.1.1" {};
            fortran-src = pkgs.haskell.lib.dontCheck (pkgs.haskellPackages.callHackage "fortran-src" "0.4.0" {});
#           pandoc = pkgs.haskellPackages.callCabal2nix "pandoc" (
#             builtins.fetchTarball {
#               url = "https://github.com/jgm/pandoc/archive/7c6dbd37eb3e785f875e0030d723e422db72d453.zip";
#               sha256 = "0xyiaj3z0hm4zdkdird5296q5r9lb5dpqlm4352z9kglksq66k63";
#           }){};
            stack = pkgs.haskellPackages.callCabal2nix "stack" (
              builtins.fetchTarball {
                url = "https://github.com/commercialhaskell/stack/archive/9dcef52902d01646d63fe76fc8e6b1b3ac6cc9b8.tar.gz";
                sha256 = "0xs8zmxvklgqmm44xb76w45cgs5kbx2ab1677gmlclkng105px3h";
            }){};
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
