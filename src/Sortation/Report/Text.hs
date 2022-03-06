module Sortation.Report.Text where

import Effectful.Database
import Sortation.Library

reportLibrary ::
  forall db es.
  [Database (Library db), IOE] :>> es =>
  Eff es ()
reportLibrary =
  selectAll @(Library db) @(RomSet db) >>= traverse_ \romSet -> do
    liftIO $ putStrLn $ "ROMSET: " <> romSet.name
    traverse select romSet.roms >>= traverse_ \rom -> do
      liftIO $ putStrLn $ "  ROM: " <> rom.name
      traverse select rom.files >>= traverse_ \file -> do
        liftIO $ putStrLn $ "    FILE: " <> file.name
        pPrintOpt NoCheckColorTty fileOutputOptions file

fileOutputOptions :: OutputOptions
fileOutputOptions =
  defaultOutputOptionsNoColor
    & #outputOptionsIndentAmount .~ 2
    & #outputOptionsInitialIndent .~ 8
