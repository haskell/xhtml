{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}

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

scrolling           :: LText -> HtmlAttr
scrolling           =   strAttr "scrolling"
