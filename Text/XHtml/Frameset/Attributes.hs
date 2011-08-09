{-# OPTIONS_HADDOCK hide #-}
-- #hide
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Text.XHtml.Frameset.Attributes where

import Text.XHtml.Internals

-- * Extra attributes in XHTML Frameset

frameborder         :: Int    -> HtmlAttr
frameborder         =   intAttr "frameborder"

marginheight        :: Int    -> HtmlAttr
marginheight        =   intAttr "marginheight"

marginwidth         :: Int    -> HtmlAttr
marginwidth         =   intAttr "marginwidth"

noresize            ::           HtmlAttr
noresize            = emptyAttr "noresize"

scrolling           :: String -> HtmlAttr
scrolling           =   strAttr "scrolling"
