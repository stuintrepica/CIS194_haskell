
--
-- The Towers Of Hanoi
-- Haskell
-- Copyright (C) 1998 Amit Singh. All Rights Reserved.
-- http://hanoi.kernelthread.com
--
dohanoi(0, _, _, _) = []

dohanoi(n, from, to, using) = 
    dohanoi(n - 1, from, using, to) ++
        [(from, to)] ++
        dohanoi(n - 1, using, to, from)

hanoi(n) = dohanoi(n, 'a', 'c', 'b')
