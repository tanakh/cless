{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Options.Applicative
import           System.Console.Terminfo.Color       as Terminfo
import           System.Console.Terminfo.PrettyPrint
import           Text.Highlighting.Kate              as Kate
import           Text.PrettyPrint.Free               (hcat, vcat)

main :: IO ()
main = join $ execParser opts where
  opts = info (helper <*> cmd)
         ( fullDesc
        <> progDesc "Print the content of FILE with syntax highlighting"
        <> header "cless: Colorized LESS" )

  cmd = process
        <$> switch ( long "list-langs"
                  <> short 'L'
                  <> help "Show the list of supported languages" )
        <*> switch ( long "list-styles"
                  <> short 'S'
                  <> help "Show the list of supported styles" )
        <*> optional (strOption ( long "lang"
                  <> short 'l'
                  <> metavar "LANG"
                  <> help "Specify language name" ) )
        <*> optional (strOption ( long "style"
                  <> short 's'
                  <> metavar "STYLE"
                  <> help "Specify style name (default 'pygments')" ) )
        <*> argument str (metavar "FILE")

styles :: [(String, Style)]
styles =
  [ ("pygments"  , pygments  )
  , ("kate"      , kate      )
  , ("espresso"  , espresso  )
  , ("tango"     , tango     )
  , ("haddock"   , haddock   )
  , ("monochrome", monochrome)
  , ("zenburn"   , zenburn   )
  ]

process :: Bool -> Bool -> Maybe String -> Maybe String -> FilePath -> IO ()
process True _ _ _ _ =
  mapM_ putStrLn languages

process _ True _ _ _ =
  mapM_ (putStrLn . fst) styles

process _ _ mb_lang mb_stylename file = do
  con <- readFile file

  let lang = fromMaybe (error "cannot determin language")
             $ mb_lang <|> listToMaybe (languagesByFilename file)

      ss = highlightAs lang con

      style = maybe pygments findStyle mb_stylename

      findStyle name =
        fromMaybe (error $ "invalid style name: " ++ name)
        $ lookup name styles

  displayLn $ ppr style ss

ppr :: Style -> [SourceLine] -> TermDoc
ppr Style{..} = vcat . map (hcat . map token) where
  token (tokenType, ss) =
    tokenEffect tokenType $ fromString ss

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
