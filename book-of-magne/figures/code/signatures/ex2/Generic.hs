module Generic where

import Booleans
import Integers
import Naturals

normalized = (["T"],[
                ("inital",[],"T"),
                ("final",[],"T"),
                ("add",["T","T"],"T"),
                ("mult",["T","T"], "T"),
                ("inv", ["T"],"T")
            ])
