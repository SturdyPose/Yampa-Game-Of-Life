module Utils.Utils where
import qualified SDL

toMaybe:: Bool -> a -> Maybe a
toMaybe True a = Just a
toMaybe False _ = Nothing

whenMaybe :: Bool -> Maybe Bool
whenMaybe False = Nothing
whenMaybe True = Just True


getSDLTime:: IO Double
getSDLTime = (*1000) <$> SDL.time 