{-# OPTIONS_HADDOCK hide #-}

{-# language OverloadedStrings #-}

-- | This module contains functions for displaying
--   HTML as a pretty tree.
module Text.XHtml.Debug ( HtmlTree(..), treeHtml, treeColors, debugHtml ) where

import Text.XHtml.Internals
import Text.XHtml.Extras
import Text.XHtml.Table
import Text.XHtml.Strict.Elements
import Text.XHtml.Strict.Attributes
import qualified Data.Text.Lazy as LText

import Data.List (uncons)

--
-- * Tree Displaying Combinators
--

-- | The basic idea is you render your structure in the form
-- of this tree, and then use treeHtml to turn it into a Html
-- object with the structure explicit.
data HtmlTree
      = HtmlLeaf Html
      | HtmlNode Html [HtmlTree] Html

treeHtml :: [LText.Text] -> HtmlTree -> Html
treeHtml colors h =
    table !
      [ border 0,
        cellpadding 0,
        cellspacing 2
      ]
      << treeHtml' colors h
  where
    manycolors = scanr (:) []

    treeHtmls :: [[LText.Text]] -> [HtmlTree] -> HtmlTable
    treeHtmls c ts = aboves (zipWith treeHtml' c ts)

    treeHtml' :: [LText.Text] -> HtmlTree -> HtmlTable
    treeHtml' _ (HtmlLeaf leaf) = cell
                                        (td ! [width "100%"]
                                          << bold
                                              << leaf)
    treeHtml' (c:cs@(c2:_)) (HtmlNode hopen ts hclose)
        | null ts && isNoHtml hclose = cell hd
        | null ts = hd </> bar `beside` (td ! [bgcolor' c2] << spaceHtml) </> tl
        | otherwise = hd </> (bar `beside` treeHtmls morecolors ts) </> tl
      where
        -- This stops a column of colors being the same
        -- color as the immediately outside nesting bar.
        morecolors = filter (maybe True ((/= c) . fst) . uncons) (manycolors cs)
        bar = td ! [bgcolor' c,width "10"] << spaceHtml
        hd = td ! [bgcolor' c] << hopen
        tl = td ! [bgcolor' c] << hclose
    treeHtml' _ _ = error "The imposible happens"

instance HTML HtmlTree where
      toHtml = treeHtml treeColors

-- type "length treeColors" to see how many colors are here.
treeColors :: [LText.Text]
treeColors = ["#88ccff","#ffffaa","#ffaaff","#ccffff"] ++ treeColors


--
-- * Html Debugging Combinators
--

-- | This uses the above tree rendering function, and displays the
-- Html as a tree structure, allowing debugging of what is
-- actually getting produced.
debugHtml :: (HTML a) => a -> Html
debugHtml obj = table ! [border 0] <<
                  ( th ! [bgcolor' "#008888"]
                     << underline'
                       << ("Debugging Output" :: String)
               </>  td << toHtml (debug' (toHtml obj))
              )
  where

      debug' :: Html -> [HtmlTree]
      debug' (Html markups) = map debug (markups [])

      debug :: HtmlElement -> HtmlTree
      debug (HtmlString str) = HtmlLeaf (spaceHtml +++
                                              linesToHtml (lines (builderToString str)))
      debug (HtmlTag {
              markupTag = tag',
              markupContent = content',
              markupAttrs  = mkAttrs
              }) =
              if isNoHtml content'
                then HtmlNode hd [] noHtml
                else HtmlNode hd (map debug (getHtmlElements content')) tl
        where
              attrs = mkAttrs []
              args = if null attrs
                     then ""
                     else "  " <> unwords (map show attrs)
              hd = xsmallFont << ("<" <> lazyByteStringToString tag' <> args <> ">")
              tl = xsmallFont << ("</" <> lazyByteStringToString tag' <> ">")

bgcolor' :: LText.Text -> HtmlAttr
bgcolor' c = thestyle ("background-color:" <> c)

underline' :: Html -> Html
underline' = thespan ! [thestyle "text-decoration:underline"]

xsmallFont :: Html -> Html
xsmallFont  = thespan ! [thestyle "font-size:x-small"]
