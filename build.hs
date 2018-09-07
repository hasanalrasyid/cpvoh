#!/usr/bin/env stack
--stack --resolver lts-11.3  --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

-- {-# LANGUAGE OverloadedStrings #-}


import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

--cxxFlags = "-cpp -Wall -g -static -Iincludes -L/usr/lib64/openmpi3/lib -I/usr/include/openmpi3-x86_64"
--linkFlags = "-lstdc++ -lgfortran -lm -lmpi -L/usr/lib64/openmpi3/lib"
cxxFlags = "-fno-underscoring -cpp -Wall -g -static -Iincludes -L/usr/lib64/openmpi3/lib"
linkFlags = "-L/usr/lib64/openmpi3/lib -lmpi -lstdc++ -lgfortran -lm"
ghcFlags = "-O"

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles="_build"
                             } $ do
    want ["_build/run" <.> exe]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    phony "cleanf" $ do
        putNormal "Cleaning files in _build/f-src"
        removeFilesAfter "_build" ["/f-src/*"]

    "_build/run" <.> exe %> \out -> do
        cs <- getDirectoryFiles "" ["f-src//*.f90","cpp-src//*.cpp","c-src//*.c"]
        hs <- getDirectoryFiles "" ["src//*.hs"]
        let os = ["_build" </> c -<.> "o" | c <- cs]
        let ohs = ["_build" </> c -<.> "o" | c <- hs]
        putNormal $ show os
        need ohs
        need os
        cmd_ "ghc -no-hs-main -o" [out] os ohs linkFlags
        {-
        Stdout lh' <- cmd "ghc-pkg field base include-dirs"
        let libHs = takeDirectory $ last $ words $ lh'
        cmd_ "mpifort -o" [out] os ohs $ linkFlags ++ libHs
        -}


    "_build//*.o" %> \out -> do
        let (cmp,fSrc) = case (takeDirectory1 $ dropDirectory1 out) of
                       "c-src" -> ("mpicc",dropDirectory1 $ out -<.> "c")
                       "cpp-src" -> ("mpic++",dropDirectory1 $ out -<.> "cpp")
                       "f-src" -> ("mpifort",dropDirectory1 $ out -<.> "f90")
                       "src" -> ("ghc",dropDirectory1 $ out -<.> "hs")
                       otherwise -> ("echo compiler undefined","")
            m = out -<.> "m"
            incDir = fmap (</> "includes") takeDirectory1 out
        putNormal $ show out
        case cmp of
          "ghc" -> do
            cmd_ cmp ghcFlags "-c" [fSrc] "-o" [out] "-stubdir" [incDir]
          otherwise -> do
            Stdout incGHC' <- cmd "ghc-pkg field rts include-dirs"
            let incGHC = last $ words $ incGHC'
            cmd_ cmp cxxFlags "-c" [fSrc] "-o" [out] "-MMD -MF" [m] $ concat ["-I",incDir," -I",incGHC]
            needMakefileDependencies m
