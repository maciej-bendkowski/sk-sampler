{-|
 - Module       : CL
 - Description  : Combinatory logic utilities.
 - Copyright    : (c) Maciej Bendkowski, 2016
 - Maintainer   : maciej.bendkowski@tcs.uj.edu.pl
 - Stability    : experimental
 -}
module CL (
    CL(..),
    normalizeN,
    trackNormalizationN
) where

    -- | SK-combinator datatype.
    data CL = S | K 
            | App CL CL

    instance Show CL where
        showsPrec _ S = (:) 'S'
        showsPrec _ K = (:) 'K'
        showsPrec _ (App e t @ (App _ _)) =
            shows e . (:) '(' . shows t . (:) ')'
        showsPrec _ (App e t) = shows e . shows t

    -- | Performs a single reduction step if possible.
    -- Returns the reduct and a flag to indicate
    -- whether the reduction occured.
    reduce :: CL -> (CL, Bool)
    reduce (App (App (App S x) y) z) = (App (App x z) (App y z), True)
    reduce (App (App K x) _) = (x, True)
    reduce t = (t, False)

    -- | Performs a single head reduction step is possible.
    -- Returns the reduct and a flag to indicate
    -- whether the reduction occured.
    headR :: CL -> (CL, Bool)
    headR t @ (App t' t'') = case reduce t of
        (x, True) -> (x, True) 
        (_, False) -> case headR t' of
            (y, True) -> (App y t'', True)
            (_, False) -> case headR t'' of
                (z, True) -> (App t' z, True)
                _ -> (t, False)
    headR t = (t, False)

    -- | Performs up to N head reduction steps.
    -- Returns the reduct and a flag to indicate
    -- whether the reduction occured.
    headRN :: (Integral a) => a -> CL -> CL
    headRN 0 t = t
    headRN n t = case headR t of
        (t', True) -> headRN (n - 1) t'
        _ -> t

    -- | Tests whether the given term
    -- is in normal form or not.
    inNormalForm :: CL -> Bool
    inNormalForm (App (App (App S _) _) _) = False
    inNormalForm (App (App K _) _) = False
    inNormalForm (App t t') = inNormalForm t && inNormalForm t'
    inNormalForm _ = True

    -- | Tests whether the given SK-combinator
    -- normalizes in at most n head reduction steps.
    normalizeN :: (Integral a) => a -> CL -> Bool
    normalizeN 0 _ = False
    normalizeN n t = inNormalForm $ headRN n t

    -- | Tracks the number of head reduction steps
    -- while performing bounded normalization of the given combinator.
    trackHeadRN :: (Integral a) => a -> a -> CL -> (CL, a)
    trackHeadRN 0 m t = (t, m)
    trackHeadRN n m t = case headR t of
        (t', True) -> trackHeadRN (n - 1) (m + 1) t'
        _ -> (t, m)

    -- | Tests whether the given SK-combinator
    -- normalizes in at most n head reduction steps.
    -- Tracks the number of performed reduction steps.
    trackNormalizationN :: (Integral a) => a -> CL -> (Bool, a)
    trackNormalizationN 0 _ = (False, 0)
    trackNormalizationN n t = let
        (t',m) = trackHeadRN n 0 t
        in (inNormalForm t', m)
