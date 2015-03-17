{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Exception
import           Control.Monad
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Options.Applicative
import           System.Console.Terminfo
import           System.Console.Terminfo.Color       as Terminfo
import           System.Console.Terminfo.PrettyPrint
import           System.Environment
import           System.IO
import           System.Process
import           Text.Highlighting.Kate              as Kate
import           Text.PrettyPrint.Free               hiding ((<>))

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
        <*> optional (argument str (metavar "FILE"))

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

defaultPager :: String
defaultPager = "less -R"

defaultTerm :: String
defaultTerm = "xterm-256color"

process :: Bool -> Bool -> Maybe String -> Maybe String -> Maybe FilePath -> IO ()
process True _ _ _ _ =
  mapM_ putStrLn languages

process _ True _ _ _ =
  mapM_ (putStrLn . fst) styles

process _ _ mb_lang mb_stylename mb_file = do
  con <- case mb_file of
      Just file -> readFile file
      Nothing -> getContents

  let lang = fromMaybe (error $ "cannot determin language: " ++ fromMaybe "<stdin>" mb_file)
             $ mb_lang <|> do file <- mb_file; listToMaybe (languagesByFilename file)

      ss = highlightAs lang con

      style = maybe pygments findStyle mb_stylename

      findStyle name =
        fromMaybe (error $ "invalid style name: " ++ name)
        $ lookup name styles

      doc = ppr style ss <> linebreak
      sdoc = renderPretty 0.6 80 (prettyTerm doc)

  evaluate sdoc

  termType <- fromMaybe defaultTerm <$> lookupEnv "TERM"
  pager <- fromMaybe defaultPager <$> lookupEnv "PAGER"
  term  <- setupTerm $ if termType == "screen" then defaultTerm else termType

  bracket
    (createProcess (shell pager) { std_in = CreatePipe } )
    ( \(_, _, _, ph) -> waitForProcess ph )
    $ \(Just h, _, _, _) -> do
      case getCapability term $ evalTermState $ displayCap sdoc of
        Just output -> hRunTermOutput h term output
        Nothing -> displayIO h sdoc
      hClose h

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
