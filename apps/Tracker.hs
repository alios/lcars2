{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings, FlexibleContexts,  
             GADTs #-}

module Main (main) where

import Network.Torrent.Tracker

main :: IO ()
main = app defaultTrackerConfig
