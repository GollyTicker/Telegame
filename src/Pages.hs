-- This module defines helper functions for creating web pages using HASTE

module Pages where



import Haste
import Haste.Prim
import Haste.Foreign
import Haste.DOM



-- `mkDiv` makes a container element for grouping elements together
mkDiv :: IO Elem
mkDiv = newElem "div"

-- `wrapDiv e` makes a "div" node with `e` as the only child
wrapDiv :: Elem -> IO Elem
wrapDiv e = mkDiv `with` [children [e]]

-- `appendChildren parent children` adds a list of children to a parent element
appendChildren :: Elem -> [Elem] -> IO ()
appendChildren parent children = sequence_ [appendChild parent c | c <- children]

-- `row parent children` adds the children as a row row to the parent
-- (assuming the children do not use column layout internally)
row :: Elem -> [Elem] -> IO ()
row = appendChildren

-- `column parent children` adds the children as a column column to the parent
column :: Elem -> [Elem] -> IO ()
column parent children = do
    cs <- sequence [wrapDiv c | c <- children]
    appendChildren parent cs

-- `mkInput width init` makes an input element with the specified width and
-- initial text
mkInput :: Int -> String -> IO Elem
mkInput width init = do
    newElem "input" `with` [attr "type"  =: "text",
                            attr "size"  =: show width,
                            attr "value" =: init]

-- `mkButton label` makes a clickable button with the given label
mkButton :: String -> IO Elem
mkButton label = newElem "button" `with` [prop "textContent" =: label]

-- `mkHTML html` makes an element with the specified HTML content
mkHTML :: String -> IO Elem
mkHTML html = newElem "span" `with` [prop "innerHTML" =: html]

-- `select e` makes the text in element `e` selected
select :: Elem -> IO ()
select = ffi $ toJSStr "(function(e) {e.select();})"

-- `mkCanvas width height` makes a drawing canvas of the specified dimensions
mkCanvas :: Int -> Int -> IO Elem
mkCanvas width height =
    newElem "canvas" `with` [style "border"          =: "1px solid black",
                             style "backgroundColor" =: "white",
                             prop "width"            =: show width,
                             prop "height"           =: show height]

