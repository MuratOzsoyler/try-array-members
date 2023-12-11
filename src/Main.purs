module Main where

import Prelude

import Control.Monad.ST.Global (toEffect)
import Data.Foldable (traverse_)
import Data.Tuple.Nested ((/\))
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useDynAtEnd, useState', (<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import FRP.Event as Event
import FRP.Poll (Poll)
import FRP.Poll as Poll

main :: Effect Unit
main = do
  { push, poll } <- toEffect Poll.create
  launchAff_ do
    delay (Milliseconds 0.0)
    liftEffect (push [ "A" ])
    delay (Milliseconds 1000.0)
    liftEffect (push [ "X", "Y" ])
  runInBody Deku.do
    pushItem /\ items <- useState'
    let
      arrayPoll = poll # bindPollToEffect \arr -> launchAff_ do
        Console.debug "before delay"
        delay $ Milliseconds 0.0
        Console.debug "after delay"
        liftEffect (arr # traverse_ pushItem)
    D.table_
      [ D.thead_ [ D.tr_ [ D.th__ "*" ] ]
      , D.tbody_
          [ arrayPoll <#~> \_ -> Deku.do
              { value: item } <- useDynAtEnd items
              D.tr_ [ D.tr__ item ]
          ]
      ]

bindPollToEffect :: forall a b. (a -> Effect b) -> Poll a -> Poll b
bindPollToEffect eff poll = poll # Poll.dredge (_ `Event.bindToEffect` eff)
