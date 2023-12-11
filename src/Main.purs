module Main where

import Prelude

import Control.Monad.ST.Global (toEffect)
import Data.Foldable (traverse_)
import Data.Tuple.Nested ((/\))
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Effect as DE
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
  _ /\ push /\ poll <- toEffect $ DE.useHot [ "A", "B" ] -- DE.useState [ "A", "B" ] -- DE.useState' --  Poll.create
  launchAff_ do
    -- delay (Milliseconds 0.0)
    -- liftEffect (push [ "A", "B" ])
    delay (Milliseconds 1000.0)
    liftEffect (push [ "X", "Y" ])
    delay (Milliseconds 1000.0)
    liftEffect (push [ "M", "N" ])
  runInBody Deku.do
    D.table_
      [ D.thead_ [ D.tr_ [ D.th__ "*" ] ]
      , D.tbody_
          [ Deku.do
              pushItem /\ items <- useState'
              let
                arrayPoll = poll # bindPollToEffect \arr -> launchAff_ do
                  -- Console.debug "before delay"
                  delay $ Milliseconds 0.0
                  -- Console.debug "after delay"
                  liftEffect (arr # traverse_ pushItem)

              arrayPoll <#~> \_ -> Deku.do
                { value: item } <- useDynAtEnd items
                D.tr_ [ D.td__ item ]
          ]
      ]

bindPollToEffect :: forall a b. (a -> Effect b) -> Poll a -> Poll b
bindPollToEffect eff poll = poll # Poll.dredge (_ `Event.bindToEffect` eff)
