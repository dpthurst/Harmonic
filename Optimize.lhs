\documentclass[12pt]{amsart}
\input{preamble}
\input{defs}

\begin{document}

\begin{code}
module Optimize where
import IMatrix
\end{code}

Optimize a generic function, given the function and its
derivatives. The constraints are that all the input parameters must be
positive.

First, the one variable case.

\begin{code}
opt1D :: (Double -> (Double, Double)) -> Double -> (Double,(Double,Double))
opt1D f x0 = opt1D' f (x0,f x0)

opt1D' f v0@(x0,(y0,y0')) | y0' > 0   = opt1D''' f v0 (f 0)
                          | otherwise = opt1D' f (2*x0, f (2*x0))

opt1D'' f v0 (y1,y1') | y1' > 0   = (0,(y1,y1'))
                      | otherwise = opt1D''' f (0,(y1,y1')) v0

-- Main optimization. Given: x0 < x1, y0' < 0, y1' > 0
opt1D''' f v0@(x0,(y0,y0')) v1@(x1,(y1,y1'))
    | x1 - x0 < 0.00001 = v0
    | otherwise = let x2 = (y1' * x0 - y0' * x1)/(y1'-y0')
                      (y2,y2') = f x2
                  in if y2' > 0 then 

\end{code}

\end{document}
