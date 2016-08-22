-- | A module comment.
module X where
-- a comment before bob
bob -- after bob
 =
  foo -- next to foo
  -- line after foo
    (bar
       foo -- next to bar foo
       bar -- next to bar
     ) -- next to the end paren of (bar)
    -- line after (bar)
    mu -- next to mu
    -- line after mu
    -- another line after mu
    zot -- next to zot
    -- line after zot
    (case casey -- after casey
           of
       Just -- after Just
        -> do
         justice -- after justice
          *
           foo
             (blah * blah + z + 2 / 4 + a - -- before a line break
              2 * -- inside this mess
              z /
              2 /
              2 /
              aooooo /
              aaaaa -- bob comment
              ) +
           (sdfsdfsd fsdfsdf) -- blah comment
         putStrLn "")
    [1, 2, 3]
    [ 1 -- foo
    , ( 2 -- bar
      , 2.5 -- mu
       )
    , 3]

-- a comment after bob

-- Comment for foo
foo = 1 -- after foo

-- a comment at the end of the module
