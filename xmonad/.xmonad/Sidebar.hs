{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, OverloadedStrings, DeriveGeneric, FlexibleInstances #-}

module Sidebar (
    EmptyLayout(..),
    toggleTag,
    hideTagged,
    copyTagged,
    copyHere,
    getLayout,
    toggleBar,
    resourceGroup,
    tagWindowGroup,
    titleGroup,
    classGroup,
    addTagHook,
    resourceExcludeGroup,
    spawnApps,
    (~?),
    (/~?)) where

import XMonad
import XMonad.Core
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.Actions.CopyWindow hiding (copy)
import XMonad.Layout.ComboP
import XMonad.Layout.Combo
import XMonad.Actions.DynamicWorkspaces
import qualified XMonad.StackSet as W
import XMonad.Layout.Decoration
import XMonad.Actions.TagWindows
import XMonad.Hooks.Minimize
import XMonad.Layout.Minimize
import XMonad.Layout.DragPane
import XMonad.Layout.BoringWindows
import XMonad.Layout.BoringWindows
import XMonad.Hooks.ManageHelpers
import Data.List 

data EmptyLayout a = EmptyLayout deriving (Show, Read)
instance LayoutClass EmptyLayout a where
    doLayout a b _ = emptyLayout a b
    description _ = "Empty Layout"

(~?) ::  (Eq a, Functor f) => f [a] -> [a] -> f Bool
q ~? x = fmap (isInfixOf x) q

(/~?) :: ( Eq a, Functor f) => f [a]-> [a] -> f Bool
q /~? x = fmap (isInfixOf x) q

resourceGroup :: [String] -> Query Bool
resourceGroup = foldr1 (<||>) . map (resource ~?)

classGroup :: [String] -> Query Bool
classGroup = foldr1 (<||>) . map (className ~?)

titleGroup :: [String] -> Query Bool
titleGroup = foldr1 (<||>) . map (title ~?)

resourceExcludeGroup :: [String] -> Query Bool
resourceExcludeGroup = foldr1 (<&&>) . map (resource /~?)

tagWindowGroup wg t = wg --> addTagHook t

spawnApps :: [(String, String)] -> X ()
spawnApps a = mapM_ (uncurry spawnOrCopy) a

spawnOrCopy :: String -> String -> X ()
spawnOrCopy b c = runOrCopy b (resource ~? c)


addTagHook :: [String] -> ManageHook
addTagHook t = do
    w <- ask
    liftX . sequence . fmap (flip addTag w) $ t
    idHook

toggleTag :: String -> String -> Window -> X ()
toggleTag s ss w = do
    t <- hasTag s w
    if t then do
        delTag s w
        addTag ss w
        killAllOtherCopies
    else do
        addTag s w
        delTag ss w
    sendMessage $ SwapWindow
    copyTagged s

getLayout :: X String
getLayout = 
    let ld = description . W.layout . W.workspace . W.current
    in withWindowSet $ return . ld

hideTagged :: String -> X ()
hideTagged s = do
    addHiddenWorkspace "hidden"
    withTaggedP s (W.shiftWin "hidden")

copyTagged :: String -> X ()
copyTagged s = withTaggedGlobalP s copyHere

copyHere :: (Ord a, Eq s, Eq i) => a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
copyHere w s = copyWindow w (W.currentTag s) s

toggleBar :: String -> X ()
toggleBar t = do
    q <- getLayout
    if q == t then do
        copyTagged t
    else do
        hideTagged t