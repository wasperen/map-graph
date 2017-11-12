orIfNothing :: Maybe a -> Maybe a -> Maybe a
Nothing `orIfNothing` b = b
a `orIfNothing` _ = a
