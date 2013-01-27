{- Haskell version of Perl 6 code from
 - http://perl6maven.com/from-iterative-to-functional-perl6-code
 -}
import Control.Monad (filterM)
import System.Directory

existingFiles' =
    let paths = ["/tmp", "/home/rjh"]
        filename = "stripes"
        exts = ["gif", "jpg", "png"]
    in filterM doesFileExist
         [concat [path, "/", filename, ".", ext] | path <- paths, ext <- exts]
