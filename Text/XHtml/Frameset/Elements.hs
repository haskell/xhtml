{-# OPTIONS_HADDOCK hide #-}
-- #hide
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Text.XHtml.Frameset.Elements where

import Text.XHtml.Internals

-- * Extra elements in XHTML Frameset

frame               :: Html -> Html
frame               =  tag "frame"

frameset            :: Html -> Html
frameset            =  tag "frameset"

noframes            :: Html -> Html
noframes            =  tag "noframes"
