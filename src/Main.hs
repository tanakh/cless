{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.Maybe
import           Data.String
import           System.Console.Terminfo.Color       as Terminfo
import           System.Console.Terminfo.PrettyPrint
import           System.Environment
import           Text.Highlighting.Kate              as Kate
import qualified Text.Highlighting.Kate.Styles       as Styles
import           Text.PrettyPrint.Free

main :: IO ()
main = do
  [file] <- getArgs
  con <- readFile file

  let [lang] = languagesByFilename file
      ss = highlightAs lang con

  display $ ppr Styles.tango ss

ppr :: Style -> [SourceLine] -> TermDoc
ppr Style{..} = vcat . map (hcat . map token) where
  token (tokenType, str) =
    tokenEffect tokenType $ fromString str

  tokenEffect :: TokenType -> TermDoc -> TermDoc
  tokenEffect tokenType =
    let tokenStyle = fromMaybe defs $ lookup tokenType tokenStyles
        defs = defStyle { tokenColor = defaultColor
                        , tokenBackground = Nothing -- backgroundColor
                        }
    in styleToEffect tokenStyle

  styleToEffect TokenStyle{..} =
    with (maybe Nop (Foreground . cnvColor) tokenColor) .
    with (maybe Nop (Background . cnvColor) tokenBackground) .
    with (if tokenBold      then Bold      else Nop) .
    -- with (if tokenItalic    then Standout  else Nop) .
    with (if tokenUnderline then Underline else Nop)

cnvColor :: Kate.Color -> Terminfo.Color
cnvColor (RGB r g b) = ColorNumber
  $ 16
  + lucol (fromIntegral r) * 6 * 6
  + lucol (fromIntegral g) * 6
  + lucol (fromIntegral b)
  where
    tbl = [0x00, 0x5f, 0x87, 0xaf, 0xd7, 0xff :: Int]
    lucol v = snd $ minimum [ (abs $ a - v, i) | (a, i) <- zip tbl [0..] ]
