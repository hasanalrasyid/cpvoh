#!/usr/bin/env stack
--stack --resolver lts-11.3  --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/hascpvo/stack.yaml




import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

cxxFlags = "-Wall -Werror -g -static -Iincludes"
linkFlags = "-lstdc++"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["_build/run" <.> exe]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    "_build/run" <.> exe %> \out -> do
        cs <- getDirectoryFiles "" ["cpp-src//*.cpp","c-src//*.c"]
        let os = ["_build" </> c -<.> "o" | c <- cs]
        putNormal $ show os
        need os
        cmd "gcc -o" [out] os linkFlags

    "_build//*.o" %> \out -> do
        let (cmp,fSrc) = case (takeDirectory1 $ dropDirectory1 out) of
                       "c-src" -> ("gcc",dropDirectory1 $ out -<.> "c")
                       "cpp-src" -> ("g++",dropDirectory1 $ out -<.> "cpp")
                       "f-src" -> ("gfortran",dropDirectory1 $ out -<.> "f")
                       otherwise -> ("echo compiler undefined","")
        let m = out -<.> "m"
        cmd_ cmp cxxFlags "-c" [fSrc] "-o" [out] "-MMD -MF" [m]
        needMakefileDependencies m
