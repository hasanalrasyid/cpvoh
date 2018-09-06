#!/usr/bin/env stack
--stack --resolver lts-11.3  --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

-- {-# LANGUAGE OverloadedStrings #-}


import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

cxxFlags = "-cpp -Wall -g -static -Iincludes"
linkFlags = "-lstdc++ -lgfortran -lm"
ghcFlags = "-O"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["_build/run" <.> exe]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    "_build/run" <.> exe %> \out -> do
        cs <- getDirectoryFiles "" ["f-src//*.f90","cpp-src//*.cpp","c-src//*.c"]
        hs <- getDirectoryFiles "" ["src//*.hs"]
        let os = ["_build" </> c -<.> "o" | c <- cs]
        let ohs = ["_build" </> c -<.> "o" | c <- hs]
        putNormal $ show os
        need os
        need ohs
        cmd_ "ghc -no-hs-main -o" [out] os ohs linkFlags

    "_build//*.o" %> \out -> do
        let (cmp,fSrc) = case (takeDirectory1 $ dropDirectory1 out) of
                       "c-src" -> ("gcc",dropDirectory1 $ out -<.> "c")
                       "cpp-src" -> ("g++",dropDirectory1 $ out -<.> "cpp")
                       "f-src" -> ("gfortran",dropDirectory1 $ out -<.> "f90")
                       "src" -> ("ghc",dropDirectory1 $ out -<.> "hs")
                       otherwise -> ("echo compiler undefined","")
            m = out -<.> "m"
            incDir = fmap (</> "includes") takeDirectory1 out
        putNormal $ show out
        Stdout incGHC' <- cmd "ghc-pkg field rts include-dirs"
        let incGHC = last $ words $ incGHC'
        case cmp of
          "ghc" -> do
            cmd_ cmp ghcFlags "-c" [fSrc] "-o" [out] "-stubdir" [incDir]
          otherwise -> do
            cmd_ cmp cxxFlags "-c" [fSrc] "-o" [out] "-MMD -MF" [m] $ concat ["-I",incDir," -I",incGHC]
            needMakefileDependencies m
